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

#ifndef PDI_PLUGIN_H__
#define PDI_PLUGIN_H__

#include "pdi.h"

typedef PDI_status_t (*PDI_init_f)(const yaml_node_t *conf, MPI_Comm *world);

typedef PDI_status_t (*PDI_finalize_f)();

typedef PDI_status_t (*PDI_event_f)(const char *event);

typedef PDI_status_t (*PDI_access_f)(const char *name, void *data);

typedef PDI_status_t (*PDI_share_f)(const char *name, const void *data);

typedef PDI_status_t (*PDI_release_f)(const char *name);

typedef PDI_status_t (*PDI_reclaim_f)(const char *name);

typedef PDI_status_t (*PDI_export_f)(const char *name, const void *data);

typedef PDI_status_t (*PDI_expose_f)(const char *name, const void *data);

typedef PDI_status_t (*PDI_import_f)(const char *name, void *data);

typedef struct PDI_state_s PDI_state_t;

/** Definition of a plugin
 */
typedef struct PDI_plugin_impl_s {
	
	PDI_init_f init;
	
	PDI_finalize_f finalize;
	
	PDI_event_f event;
	
	PDI_access_f access;
	
	PDI_reclaim_f reclaim;
	
	PDI_export_f export_;
	
	PDI_expose_f expose;
	
	PDI_import_f import;
	
} PDI_plugin_impl_t;

#endif // PDI_PLUGIN_H__
