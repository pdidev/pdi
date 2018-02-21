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

#ifndef PDI_PLUGIN_H_
#define PDI_PLUGIN_H_

#include <pdi/fwd.h>


/** Skeleton of the function called at PDI finalization
 * \return an exit status code
 */
typedef void (*PDI_finalize_f)(PDI::Context& ctx);

/** Skeleton of the function called to notify an event
 * \param[in] event the event name
 * \return an exit status code
 */
typedef void (*PDI_event_f)(PDI::Context& ctx, const char *event);

/** Skeleton of the function called to notify that some data becomes available
 * \param name the name of the data made available
 * \param ref available data
 * \return an exit status code
 */
typedef void (*PDI_data_f)(PDI::Context& ctx, const char *name, PDI::Data_ref ref);

struct PDI_plugin_s {

	/// The function called at PDI finalization
	PDI_finalize_f finalize;
	
	/// The function called to notify an event
	PDI_event_f event;
	
	/// The function called to notify that some data becomes available
	PDI_data_f data;
	
};

/** Declares a plugin.
 *
 * This should be called after having implemeted the five required functions
 * for a PDI plugin:
 * - PDI_&lt;name&gt;_finalize;\
 * - PDI_&lt;name&gt;_event;\
 * - PDI_&lt;name&gt;_data_start;\
 * - PDI_&lt;name&gt;_data_end;\
 * - PDI_&lt;name&gt;_init(conf, world);\
 *
 * \param name the name of the plugin
 */
#define PDI_PLUGIN(name)\
	extern "C" void PDI_EXPORT PDI_plugin_##name##_ctor(PDI::Context& ctx, PC_tree_t conf, MPI_Comm *world, PDI_plugin_t* plugin) \
	{\
		plugin->finalize = PDI_##name##_finalize;\
		plugin->event = PDI_##name##_event;\
		plugin->data = PDI_##name##_data;\
		PDI_##name##_init(ctx, conf, world);\
	}

#endif // PDI_PLUGIN_H_
