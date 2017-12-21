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
 * \author Julien Bigot (CEA) <julien.bigot@cea.fr>
 **/

#include "config.h"

#include <cstddef>
#include <cstring>
#include <iostream>
#include <string>
#include <type_traits>

#include "pdi.h"

#include "pdi/plugin.h"
#include "pdi/datatype.h"
#include "pdi/data_descriptor.h"
#include "pdi/data_reference.h"
#include "pdi/state.h"
#include "pdi/status.h"

#include "conf.h"
#include "paraconf_wrapper.h"
#include "plugin_loader.h"


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
	Try_pc fw;
	try {
		PDI_state.transaction.clear();
		PDI_state.plugins.clear();
		
		load_conf(conf);
		
		int nb_plugins = len(PC_get(conf, ".plugins"));
		
		for (int ii = 0; ii < nb_plugins; ++ii) {
			PDI_state.PDI_comm = *world;
			//TODO: what to do if a single plugin fails to load?
			try_load_plugin(conf, ii, world);
		}
		
		if (MPI_Comm_dup(*world, &PDI_state.PDI_comm)) {
			throw Error{PDI_ERR_SYSTEM, "Unable to clone the main communicator"};
		}
		
	} catch (const Error &e) {
		for (auto &&plugin : PDI_state.plugins) {
			try { // ignore errors here, try our best to finalize everyone
				plugin.second->finalize();
			} catch (...) {
				cerr << "Error while finalizing " << plugin.first << endl;
			}
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
	Try_pc fw;
	for (auto plugin : PDI_state.plugins) {
		try { // ignore errors here, try our best to finalize everyone
			//TODO: concatenate errors in some way
			plugin.second->finalize();
		} catch (const std::exception &e) {
			cerr << "Error while finalizing " << plugin.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while finalizing " << plugin.first << endl;
		}
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
	Try_pc fw;
	for (auto &elmnt : PDI_state.plugins) {
		try { // ignore errors here, try our best to notify everyone
			//TODO: concatenate errors in some way
			elmnt.second->event(event);
		} catch (const std::exception &e) {
			cerr << "Error while triggering event " << event << " for plugin " << elmnt.first << ": " << e.what() << endl;
		} catch (...) {
			cerr << "Error while triggering event " << event << " for plugin " << elmnt.first << endl;
			//TODO: remove the faulty plugin?
		}
	}
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}


PDI_status_t PDI_access(const char *name, void **buffer, PDI_inout_t inout)
{
	Try_pc fw;
	try {
		Data_descriptor &desc = PDI_state.desc(name);
		Data_ref ref = desc.ref();
		desc.share(ref, inout & PDI_IN, inout & PDI_OUT);
		*buffer = ref;
	} catch (const Error &e) {
		return return_err(e);
	}
	return PDI_OK;
}

PDI_status_t PDI_share(const char *name, void *buffer, PDI_inout_t access)
{
	Try_pc fw;
	try {
		Data_descriptor &desc = PDI_state.desc(name);
		desc.share(buffer, &free, access & PDI_OUT, access & PDI_IN);
		Data_ref ref = desc.ref();
		
		// Provide reference to the plug-ins
		for (auto &&plugin : PDI_state.plugins) {
			try { // ignore errors here, try our best to notify everyone
				//TODO: concatenate errors in some way
				plugin.second->data(name, ref);
			} catch (const std::exception &e) {
				cerr << "Error while sharing " << name << " for plugin " << plugin.first << ": " << e.what() << endl;
			} catch (...) {
				cerr << "Error while triggering event " << name << " for plugin " << plugin.first << endl;
				//TODO: remove the faulty plugin?
			}
		}
	} catch (const Error &e) {
		return return_err(e);
	}
	
	//TODO we should return concatenated errors here...
	return PDI_OK;
}



PDI_status_t PDI_release(const char *name)
{
	Try_pc fw;
	try {
		PDI_state.desc(name).release();
	} catch (const Error &e) {
		return return_err(e);
	}
	return PDI_OK;
}


PDI_status_t PDI_reclaim(const char *name)
{
	Try_pc fw;
	try {
		PDI_state.desc(name).reclaim();
	} catch (const Error &e) {
		return return_err(e);
	}
	return PDI_OK;
}

PDI_status_t PDI_expose(const char *name, void *data, PDI_inout_t access)
{
	Try_pc fw;
	if (PDI_status_t status = PDI_share(name, data, access)) return status;
	
	if (! PDI_state.transaction.empty()) {   // defer the reclaim
		PDI_state.transaction_data.insert(name);
	} else { // do the reclaim now
		if (PDI_status_t status = PDI_reclaim(name)) return status;
	}
	
	return PDI_OK;
}


PDI_status_t PDI_transaction_begin(const char *c_name)
{
	Try_pc fw;
	try {
		if (!PDI_state.transaction.empty()) {
			throw Error{PDI_ERR_STATE, "Transaction already in progress, cannot start a new one"};
		}
		PDI_state.transaction = c_name;
	} catch (const Error &e) {
		return return_err(e);
	}
	
	return PDI_OK;
}


PDI_status_t PDI_transaction_end()
{
	Try_pc fw;
	try {
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
		
	} catch (const Error &e) {
		return return_err(e);
	}
	
	return PDI_OK;
}


PDI_status_t PDI_export(const char *name, const void *data)
{
	if (PDI_status_t status = PDI_share(name, (void *)data, PDI_OUT)) return status;
	if (PDI_status_t status = PDI_release(name)) return status;
	return PDI_OK;
}
