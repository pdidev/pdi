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

#ifndef STATE_H__
#define STATE_H__

#include <string.h>

#include "pdi_datatype.h"

typedef struct PDI_plugin_impl_s PDI_plugin_impl_t;
typedef struct PDI_type_s PDI_type_t;

typedef struct loaded_plugin_s
{
	char *name;
	
	PDI_plugin_impl_t *impl;
	
} loaded_plugin_t;

typedef enum PDI_memstatus_e {
	PDI_UNALOCATED,
	PDI_SHARED,
	PDI_OWNED
} PDI_memstatus_t;

typedef struct PDI_metadata_s
{
	char *name;
	
	PDI_type_t *type;
	
	PDI_memstatus_t memstatus;
	
	void *value;
	
} PDI_metadata_t;

typedef struct PDI_dimension_s
{
	char *name;
	
	//TODO: change to be an expr
	PDI_metadata_t *value;
	
} PDI_dimension_t;

typedef struct PDI_data_s
{
	char *name;
	
	PDI_type_t *type;
	
	size_t nb_coord;
	
	PDI_metadata_t *coords;
	
	PDI_memstatus_t memstatus;
	
	void *value;
	
} PDI_data_t;

typedef struct PDI_state_s
{
	size_t nb_metadata;
	
	PDI_metadata_t *metadata;
	
	size_t nb_data;
	
	PDI_data_t *data;
	
	size_t nb_loaded_plugins;
	
	PDI_plugin_impl_t *loaded_plugins;
	
} PDI_state_t;

extern PDI_state_t PDI_state;

#endif // STATE_H__
