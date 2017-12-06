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

#include <cstddef>
#include <cstring>
#include <iostream>
#include <string>
#include <type_traits>

#include <paraconf.h>

#include "pdi.h"

#include "pdi/plugin.h"
#include "pdi/datatype.h"
#include "pdi/data_descriptor.h"
#include "pdi/data_reference.h"
#include "pdi/state.h"
#include "pdi/status.h"

#include "conf.h"
#include "plugin_loader.h"
#include "utils.h"


#define PDI_BUFFER_SIZE 256


using namespace PDI;
using std::cerr;
using std::endl;
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
	try {
		Paraconf_raii_forwarder fw;
		PDI_state.transaction.clear();
		PDI_state.plugins.clear();
		
		load_conf(conf);
		
		int nb_plugins; PC_len(PC_get(conf, ".plugins"), &nb_plugins);
		
		for (int ii = 0; ii < nb_plugins; ++ii) {
			PDI_state.PDI_comm = *world;
			//TODO: what to do if a single plugin fails to load?
			plugin_loader_tryload(conf, ii, world);
		}
		
		if (MPI_Comm_dup(*world, &PDI_state.PDI_comm)) {
			throw Error{PDI_ERR_SYSTEM, "Unable to clone the main communicator"};
		}
		
	} catch (const Error& e) {
		for (auto plugin : PDI_state.plugins) {
			try { // ignore errors here, try our best to finalize everyone
				plugin.second->finalize();
			} catch (...) {}
		}
		PDI_state.transaction.clear();
		PDI_state.transaction_data.clear();
		PDI_state.plugins.clear();
		return PDI::return_err(e);
	}
	
	return PDI_OK;
}


PDI_status_t PDI_finalize()
{
	Paraconf_raii_forwarder fw;
	for (auto plugin : PDI_state.plugins) {
		try { // ignore errors here, try our best to finalize everyone
			//TODO: concatenate errors in some way
			plugin.second->finalize();
		} catch (...) {}
	}
	MPI_Comm_free(&PDI_state.PDI_comm);
	PDI_state.transaction.clear();
	PDI_state.transaction_data.clear();
	PDI_state.plugins.clear();
	//TODO: clear PDI_state.m_descriptors
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}


PDI_status_t PDI_event(const char *event)
{
	Paraconf_raii_forwarder fw;
	for (auto &elmnt : PDI_state.plugins) {
		try { // ignore errors here, try our best to notify everyone
			//TODO: concatenate errors in some way
			elmnt.second->event(event);
		} catch (...) {
			//TODO: remove the faulty plugin?
		}
	}
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}


PDI_status_t PDI_access(const char *name, void **buffer, PDI_inout_t inout)
{
	try {
		Paraconf_raii_forwarder fw;
		return PDI_state.desc(name).access(buffer, inout);
	} catch (const Error& e) {
		return return_err(e);
	}
}

PDI_status_t PDI_share(const char *name, void *buffer, PDI_inout_t access)
{
	try {
		Paraconf_raii_forwarder fw;
		Data_descriptor &desc = PDI_state.desc(name);
		desc.share(buffer, &free, access);
		Data_ref ref = desc.value();
		
		// Provide reference to the plug-ins
		for (auto &&plugin : PDI_state.plugins) {
			try { // ignore errors here, try our best to notify everyone
				//TODO: concatenate errors in some way
				plugin.second->data(name, ref);
			} catch (...) {
				//TODO: remove the faulty plugin?
			}
		}
	} catch (const Error& e) {
		return return_err(e);
	}
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}



PDI_status_t PDI_release(const char *name)
{
	try {
		Paraconf_raii_forwarder fw;
		return PDI_state.desc(name).release();
	} catch (const Error& e) {
		return return_err(e);
	}
}


PDI_status_t PDI_reclaim(const char *name)
{
	try {
		Paraconf_raii_forwarder fw;
		return PDI_state.desc(name).reclaim();
	} catch (const Error& e) {
		return return_err(e);
	}
}

PDI_status_t PDI_expose(const char *name, const void *data)
{
	Paraconf_raii_forwarder fw;
	if (PDI_status_t status = PDI_share(name, const_cast<void *>(data), PDI_OUT)) return status;
	
	if (! PDI_state.transaction.empty()) {   // defer the reclaim
		PDI_state.transaction_data.insert(name);
	} else { // do the reclaim now
		if (PDI_status_t status = PDI_reclaim(name)) return status;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_transaction_begin(const char *c_name)
{
	try {
		Paraconf_raii_forwarder fw;
		if (!PDI_state.transaction.empty()) {
			throw Error{PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"};
		}
		PDI_state.transaction = c_name;
	} catch (const Error& e) {
		return return_err(e);
	}
	
	return PDI_OK;
}


PDI_status_t PDI_transaction_end()
{
	try {
		Paraconf_raii_forwarder fw;
		if (PDI_state.transaction.empty()) {
			throw Error{PDI_ERR_STATE, "No transaction in progress, cannot end one"};
		}
		
		PDI_event(PDI_state.transaction.c_str());
		
		for (const string &data : PDI_state.transaction_data) {
			//TODO we should concatenate errors here...
			PDI_reclaim(data.c_str());
		}
		PDI_state.transaction_data.clear();
		PDI_state.transaction.clear();
		
	} catch (const Error& e) {
		return return_err(e);
	}
	
	return PDI_OK;
}


PDI_status_t PDI_export(const char *name, const void *data)
{
	if ( PDI_status_t status = PDI_share(name, (void *)data, PDI_OUT) ) return status;
	if ( PDI_status_t status = PDI_release(name) ) return status;
	return PDI_OK;
}
