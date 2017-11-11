/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

/**
 * \file api.c
 * \brief PDI public API functions (init, event, ... finalize).
 * \author J. Bigot (CEA)
 **/

#include "config.h"

#ifdef STRDUP_WORKS
	#define _POSIX_C_SOURCE 200809L
#endif

#include <cstring>
#include <string>
#include <type_traits>

#include <stddef.h>

#include "paraconf.h"

#include "pdi.h"

#include "pdi/plugin.h"
#include "pdi/datatype.h"
#include "pdi/state.h"
#include "conf.h"
#include "plugin_loader.h"
#include "status.h"
#include "utils.h"

#include "pdi/data_reference.h"
#include "pdi/data_content.h"
#include "pdi/data_descriptor.h"

#define PDI_BUFFER_SIZE 256

using namespace PDI;
using std::make_shared;
using std::move;
using std::stack;
using std::string;
using std::underlying_type;
using std::shared_ptr;

/* ******* function and operator  ****** */

template<typename T, typename Y>
bool exist(Y &y, T &t)
{
	typename T::iterator it = t.find(y);
	return it != t.end();
}

PDI_inout_t operator|(PDI_inout_t a, PDI_inout_t b)
{
	typedef underlying_type< PDI_inout_t >::type UL;
	return (PDI_inout_t)(static_cast< UL >(a) | static_cast< UL >(b));
}

PDI_inout_t &operator|=(PDI_inout_t &lhs, PDI_inout_t rhs)
{
	return lhs = (PDI_inout_t)(lhs | rhs);
}

PDI_inout_t operator&(PDI_inout_t a, PDI_inout_t b)
{
	typedef underlying_type< PDI_inout_t >::type UL;
	return (PDI_inout_t)(static_cast< UL >(a) & static_cast< UL >(b));
}

PDI_inout_t &operator&=(PDI_inout_t &lhs, PDI_inout_t rhs)
{
	return lhs = (PDI_inout_t)(lhs & rhs);
}



/* ******** API  ********* */

PDI_status_t PDI_init(PC_tree_t conf, MPI_Comm *world)
{
	PDI_status_t status = PDI_OK;
	PDI_state.store.clear();
	PDI_state.descriptors.clear();
	PDI_state.transaction.clear();
	PDI_state.plugins.clear();
	
	PDI_handle_err(load_conf(conf), err0);
	
	int nb_plugins; handle_PC_err(PC_len(PC_get(conf, ".plugins"), &nb_plugins), err0);
	
	for (int ii = 0; ii < nb_plugins; ++ii) {
		PDI_state.PDI_comm = *world;
		//TODO we should concatenate errors here...
		PDI_handle_err(plugin_loader_tryload(conf, ii, world), err0);
	}
	
	if (MPI_Comm_dup(*world, &PDI_state.PDI_comm)) {
		PDI_handle_err(PDI_make_err(PDI_ERR_SYSTEM, "Unable to clone the main communicator"), err0);
	}
	
	return status;
	
err0:
	PDI_finalize();
	return status;
}


PDI_status_t PDI_finalize()
{
	PDI_status_t status = PDI_OK;
	
	// Don't stop on errors in finalize, try to do our best
	PDI_errhandler_t errh = PDI_errhandler(PDI_NULL_HANDLER);
	
	for (auto plugin : PDI_state.plugins) {
		plugin.second->finalize();
	}
	PDI_state.plugins.clear();
	
	PDI_state.descriptors.clear();
	PDI_state.store.clear();
	PDI_errhandler(errh);
	
	//TODO we should concatenate errors here...
	return status;
}


PDI_status_t PDI_event(const char *event)
{
	PDI_status_t status = PDI_OK;
	
	for (auto &elmnt : PDI_state.plugins) {
		//TODO we should concatenate errors here...
		PDI_handle_err(elmnt.second->event(event), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_access(const char *name, void **buffer, PDI_inout_t inout)
{
	PDI_status_t status(PDI_OK);
	*buffer = NULL;
	auto &&refstack = PDI_state.store.find(name);
	if (refstack != PDI_state.store.end()) {
		refstack->second.push(refstack->second.top());
		if (refstack->second.top().grant(inout)) {   // got the requested rights
			*buffer = refstack->second.top().get_content()->get_buffer();
			return PDI_OK;
		} else { // cannot get the requested rights
			refstack->second.pop();
			PDI_handle_err(PDI_make_err(PDI_ERR_RIGHT, "Cannot grant priviledge for data '%s'", name), err0);
		}
	}
	
	return PDI_UNAVAILABLE;
err0:
	return status;
}

PDI_status_t PDI_share(const char *name, void *buffer, PDI_inout_t access)
{
	auto &&descit = PDI_state.descriptors.find(name);
	if ( descit == PDI_state.descriptors.end() ) return PDI_UNAVAILABLE;
	
	Data_descriptor &desc = descit->second;
	stack<Data_ref>& refstack = PDI_state.store[name];
	if (!refstack.empty() && desc.is_metadata()) {
		/// for metadata, unlink happens on share
		refstack.top().reclaim();
		refstack.pop();
	}
	
	// make a reference and put it in the store
	refstack.push(move(Data_ref(desc, buffer, access)));
	Data_ref& ref = refstack.top();
	
	// Provide reference to the plug-ins
	for (auto &&plugin: PDI_state.plugins) {
		PDI_data_end_f data_end = plugin.second->data_end;
		//TODO: register data_end
		plugin.second->data_start(ref); /// Move reference to the plug-in
	}
	
	return PDI_OK;
}



PDI_status_t PDI_release(const char *c_name)
{
	if (exist(c_name, PDI_state.descriptors)) {
		///< move reference out of the store
		auto &&refstack = PDI_state.store.find(c_name);
		if (refstack == PDI_state.store.end()) {
			return PDI_make_err(PDI_ERR_VALUE, "Cannot release a non shared value");
		}
		refstack->second.pop();
		if (refstack->second.empty()) PDI_state.store.erase(refstack);
	} else {
		return PDI_UNAVAILABLE;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_reclaim(const char *name)
{
	auto&& descit = PDI_state.descriptors.find(name);
	if ( descit == PDI_state.descriptors.end() ) return PDI_UNAVAILABLE;
	
	stack<Data_ref>& refstack = PDI_state.store[name];
	if (refstack.empty()) {
		return PDI_make_err(PDI_ERR_VALUE, "Cannot reclaim a non shared value");
	}
	
	// if the content is a metadata, keep it
	if ( descit->second.is_metadata() ) {
		if (PDI_status_t status = refstack.top().get_content()->copy_metadata()) return status;
	} else {
		// Manually reclaiming data
		refstack.top().reclaim();
		refstack.pop();
	}
	
	return PDI_OK;
}


static void add_to_transaction(const char *c_name)
{
	string name(c_name);
	
	if (exist(name, PDI_state.store)) {
		if (!exist(name,  PDI_state.transaction_data)) {    ///< If ref is not found in the store
			PDI_state.transaction_data.insert(name); // add a ref without access right
		}
	}
	return;
}


PDI_status_t PDI_expose(const char *name, const void *data, PDI_inout_t access)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, (void *)data, access), err0);
	
	if (! PDI_state.transaction.empty()) {   // defer the reclaim
		add_to_transaction(name);
	} else { // do the reclaim now
		PDI_handle_err(PDI_reclaim(name), err0);
	}
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_transaction_begin(const char *c_name)
{
	PDI_status_t status = PDI_OK;
	
	if (!PDI_state.transaction.empty()) {
		PDI_handle_err(PDI_make_err(PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"), err0);
	}
	
	PDI_state.transaction = string(c_name);
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_transaction_end()
{
	PDI_status_t status = PDI_OK;
	
	if (PDI_state.transaction.empty()) {
		PDI_handle_err(PDI_make_err(PDI_ERR_STATE, "No transaction in progress, cannot end one"), err0);
	}
	
	PDI_event(PDI_state.transaction.c_str());
	for (auto &iter : PDI_state.transaction_data) {
		//TODO we should concatenate errors here...
		PDI_reclaim(iter.c_str());
	}
	
	PDI_state.transaction_data.clear();
	PDI_state.transaction.clear();
	
	return status;
	
err0:
	return status;
}


PDI_status_t PDI_export(const char *name, const void *data)
{
	PDI_status_t status = PDI_OK;
	
	PDI_handle_err(PDI_share(name, (void *)data, PDI_OUT), err0);
	PDI_handle_err(PDI_release(name), err0);
	
	return status;
	
err0:
	return status;
}

