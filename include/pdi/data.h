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
* \file data.h
* \brief Functions to manipulate PDI_data_t objects
* \author J. Bigot (CEA)
*/

#ifndef PDI_DATA_H__
#define PDI_DATA_H__

#include <pdi.h>
#include <pdi/datatype.h>
#include <pdi/data_fwd.h>

/** The value of a variable (a.k.a. data) as a reference to the value in code
 */
typedef struct PDI_data_value_s {
	// PDI_inout_t ORed with PDI_memmode_t. Only the latest
	int access;
	
	/// a pointer to the data in code representation (i.e. potentially sparse)
	void *data;
	
} PDI_data_value_t;

struct PDI_data_s {
	/// The name of this specific data
	char *name;
	
	/// Whether this represents data or metadata
	PDI_datakind_t kind;
	
	/// The type of the data
	PDI_datatype_t type;
	
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



/** Copy the content of the data <from> into the data <to>
 * \param[out] from an existing data whose content is replaced by the content of <to>
 * \param[in] to source of the copy
 * \return an error code.
 */
PDI_status_t PDI_EXPORT PDI_data_copy(PDI_data_t *to, const PDI_data_t *from);


/** Copy the content of the data <from> into the data <to>
 * \param[out] from an existing data whose content is replaced by the content of <to>
 * \param[in] to source of the copy
 * \return an error code.
 */
PDI_status_t PDI_EXPORT PDI_data_destroy(PDI_data_t *var);



#endif // PDI_DATA_H__
