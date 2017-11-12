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

//The following is used for doxygen documentation:
/**
* \file state.h
* \brief details of the strucutures that store data, metadata, ...
* \author J. Bigot (CEA)
*/

#ifndef PDI_STATE_H__
#define PDI_STATE_H__

#include <list>
#include <memory>
#include <stack>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi.h>

#include <pdi/state_fwd.h>
#include <pdi/value_fwd.h>
#include <pdi/plugin_fwd.h>
#include <pdi/data_reference.h>
#include <pdi/data_descriptor.h>


struct loaded_plugin_s {
	/// the name of the plugin
	char *name;
	
	/// the plugin implementation, i.e. the functions it provides
	PDI_plugin_t *impl;
	
};


struct PDI_state_s {
	/** A MPI communicator containing all application processes, i.e. all
	 *  those not reserved by any PDI plugin
	 */
	MPI_Comm PDI_comm;
	
	/// References on the data
	std::unordered_map<std::string, std::stack<PDI::Data_ref>> store;
	
	/// Descriptors of the data
	std::unordered_map<std::string, PDI::Data_descriptor> descriptors;
	
	std::string transaction;
	
	/// List of data that are
	std::unordered_set<std::string> transaction_data;
	
	/// The actual loaded plugins
	std::unordered_map<std::string, std::shared_ptr<PDI_plugin_t>> plugins;
	
	/// The current error handling function
	PDI_errfunc_f *errfunc;
	
};


/// The main state of the PDI implementation
extern PDI_state_t PDI_EXPORT PDI_state;

/// Find a reference in the PDI_state and returns it without access right
PDI::Data_ref PDI_EXPORT PDI_find_ref(const std::string &name);

#endif // PDI_STATE_H__
