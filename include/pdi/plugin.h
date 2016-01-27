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

#include <pdi.h>

#include <pdi/plugin_fwd.h>
#include <pdi/state_fwd.h>

typedef PDI_status_t (*PDI_finalize_f)();

typedef PDI_status_t (*PDI_event_f)(const char *event);

typedef PDI_status_t (*PDI_data_start_f)(PDI_variable_t *data);

typedef PDI_status_t (*PDI_data_end_f)(PDI_variable_t *data);

/** Definition of a plugin
 */
struct PDI_plugin_s {
	
	PDI_finalize_f finalize;
	
	PDI_event_f event;
	
	PDI_data_start_f data_start;
	
	PDI_data_end_f data_end;
	
};

#define PDI_PLUGIN(name)\
PDI_status_t PDI_EXPORT PDI_plugin_##name##_ctor(yaml_document_t* document, const yaml_node_t *conf, MPI_Comm *world, PDI_plugin_t* plugin) \
{\
	plugin->finalize = PDI_##name##_finalize;\
	plugin->event = PDI_##name##_event;\
	plugin->data_start = PDI_##name##_data_start;\
	plugin->data_end = PDI_##name##_data_end;\
	return PDI_##name##_init(document, conf, world);\
}

#endif // PDI_PLUGIN_H__
