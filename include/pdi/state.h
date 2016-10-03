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
	// PDI_inout_t ORed with PDI_memmode_t. Only the latest 
	int access;
	
	/// a pointer to the data in code representation (i.e. potentially sparse)
	void *data;
	
};

struct PDI_data_s
{
	/// The name of this specific data
	char *name;
	
	/// Whether this represents data or metadata
	PDI_datakind_t kind;
	
	/// The type of the data
	PDI_type_t type;
	
	/// A reference to the data configuration
	PC_tree_t config;
	
	/// The number of versions of the content available
	int nb_content;
	
	/** All the versions of the data content in share order.
	 * Only the latest can be actually shared with the user code, all the others
	 * are copies kept for PDI that should have the PDI_MM_FREE flag set.
	 */
	PDI_data_value_t *content;
	
};


struct PDI_state_s
{
	/** A MPI communicator containing all application processes, i.e. all
	 *  those not reserved by any PDI plugin
	 */
	MPI_Comm PDI_comm;
	
	/// The number of data
	int nb_data;
	
	/// The actual data
	PDI_data_t *data;

	char *transaction;
	
	int nb_transaction_data;

	PDI_data_t **transaction_data;

	/// The number of loaded plugins
	int nb_plugins;
	
	/// The actual loaded plugins
	PDI_plugin_t *plugins;
	
	/// The current error handling function
	PDI_errfunc_f *errfunc;

};


/** Removes a version of the content of a data
 * \param[in] data the data whose content to discard
 * \param[in] content_id the version of the content to discard
 * \return an error code
 */
PDI_status_t PDI_EXPORT PDI_data_unlink( PDI_data_t *data, int content_id );


/// The main state of the PDI implementation
extern PDI_state_t PDI_EXPORT PDI_state;

#endif // PDI_STATE_H__
