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
* \file plugin.h
* \brief private declaration of plugins functions and structures
* \details
* To create a plugin, one must define 4 functions:
*  - PDI_finalize
*  - PDI_event
*  - PDI_data_start
*  - PDI_data_end
*  Each function return a exit status code (PDI_status_t).
*  A macro is used for the initialization (see PDI_PLUGIN)
* \author J. Bigot (CEA)
*/

#ifndef PDI_PLUGIN_H__
#define PDI_PLUGIN_H__

#include <pdi.h>

#include <pdi/plugin_fwd.h>
#include <pdi/state_fwd.h>

/** Skeleton of the function called at PDI finalization
 * \return an exit status code
 */
typedef PDI_status_t (*PDI_finalize_f)();

/** Skeleton of the function called to notify an event
 * \param[in] the event name
 * \return an exit status code
 */
typedef PDI_status_t (*PDI_event_f)(const char *event);

/** Skeleton of the function called to notify that some data becomes available
 * \param[in] available data
 * \return an exit status code
 */
typedef PDI_status_t (*PDI_data_start_f)(PDI_data_t *data);

/** Skeleton of the function called to notify that some data becomes unavailable
 * \param[in] data the plugin cannot access anymore
 * \return an exit status code
 */
typedef PDI_status_t (*PDI_data_end_f)(PDI_data_t *data);

struct PDI_plugin_s {

	/// The function called at PDI finalization
	PDI_finalize_f finalize;
	
	/// The function called to notify an event
	PDI_event_f event;
	
	/// The function called to notify that some data becomes available
	PDI_data_start_f data_start;
	
	/// The function called to notify that some data becomes unavailable
	PDI_data_end_f data_end;
	
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
	PDI_status_t PDI_EXPORT PDI_plugin_##name##_ctor(PC_tree_t conf, MPI_Comm *world, PDI_plugin_t* plugin) \
	{\
		plugin->finalize = PDI_##name##_finalize;\
		plugin->event = PDI_##name##_event;\
		plugin->data_start = PDI_##name##_data_start;\
		plugin->data_end = PDI_##name##_data_end;\
		return PDI_##name##_init(conf, world);\
	}

#endif // PDI_PLUGIN_H__
