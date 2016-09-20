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

#ifndef PDI_STATE_H__
#define PDI_STATE_H__

#include <pdi.h>

#include <pdi/state_fwd.h>
#include <pdi/value_fwd.h>
#include <pdi/plugin_fwd.h>
#include <pdi/datatype.h>

struct loaded_plugin_s
{
	/// the name of the plugin
	char *name;
	
	/// the plugin implementation, i.e. the functions it provides
	PDI_plugin_t *impl;
	
};

/** The value of a variable (a.k.a. data) as a reference to the value in code
 */
struct PDI_data_value_s
{
	// PDI_inout_t ORed, 0 if the variable is not currently shared
	int access;
	
	/// a pointer to the data in code representation (i.e. potentially sparse)
	void *data;
	
};

struct PDI_metadata_s
{
	/// The name of this specific parameter (a.k.a. metadata)
	char *name;
	
	/// The type of the parameter (a.k.a. metadata)
	PDI_type_t type;
	
	/// The value of the parameter (a.k.a. metadata) in dense representation
	void *value;
	
};

struct PDI_data_s
{
	/// The name os this specific variable (a.k.a. data)
	char *name;
	
	/// The type of the variable (a.k.a. data)
	PDI_type_t type;
	
	/// A reference to the variable (a.k.a. data) configuration
	PC_tree_t config;
	
	/// The value of the variable (a.k.a. data) as a reference
	PDI_data_value_t content;
	
};

struct PDI_state_s
{
	/** A MPI communicator containing all application processes, i.e. all
	 *  those not reserved by any PDI plugin
	 */
	MPI_Comm PDI_comm;
	
	/// the number of parameters
	int nb_metadata;
	
	/// the actual parameters (a.k.a. metadata)
	PDI_metadata_t *metadata;
	
	/// The number of data
	int nb_data;
	
	/// The actual data (a.k.a data)
	PDI_data_t *data;
	
	/// The number of loaded plugins
	int nb_plugins;
	
	/// The actual loaded plugins
	PDI_plugin_t *plugins;
	
	/// The current error handling function
	PDI_errfunc_f *errfunc;
	
};

/// The main state of the PDI implementation
extern PDI_state_t PDI_EXPORT PDI_state;

#endif // PDI_STATE_H__
