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

#ifndef PDI_PLUGIN_LOADER_H__
#define PDI_PLUGIN_LOADER_H__

#include "pdi.h"
#include "pdi/plugin_fwd.h"

/** Tries to load a given plugin
 * \param[in] conf the whole plugin configuration as a paraconf node
 * \param[in] plugin_id the ID of the plugin in the conf
 * \param[in,out] world the communicator containing all application processes
 * from which the plugin can reserve some ranks for its own use in which case
 * it replaces the communicator by a new one from which the reserved ranks have
 * been removed
 * \return an error code
 */
PDI_status_t plugin_loader_tryload( PC_tree_t conf, int plugin_id, MPI_Comm *world );

#endif // PDI_PLUGIN_LOADER_H__
