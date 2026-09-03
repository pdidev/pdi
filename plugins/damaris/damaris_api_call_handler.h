/*******************************************************************************
 * Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2024-2026 National Institute for Research in Digital Science and Technology (Inria)
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

#ifndef DAMARIS_API_CALL_HANDLER_H_
#define DAMARIS_API_CALL_HANDLER_H_

#include <mpi.h>
#include <list>
#include <map>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/context.h>

#include <Damaris.h>
#include "damaris_cfg.h"
#include "damaris_wrapper.h"

namespace damaris_pdi {

/** Dispatches Damaris API calls (init, start, write, finalize, ...) by event
 *  name, on behalf of the damaris_plugin.
 */
class Damaris_api_call_handler
{
	std::string xml_config_object;
	PDI::Expression m_communicator;
	/// NOTE: set by the constructor but currently never read
	std::string m_init_on_event = "";
	std::string m_start_on_event = "";
	std::string m_stop_on_event = "";
	/// NOTE: never set by any constructor, never read - fully unused
	std::string m_finalize_on_event = "";

public:
	Damaris_api_call_handler(
		std::string cfg_object,
		PDI::Expression comm,
		std::string init_on_event,
		std::string start_on_event,
		std::string stop_on_event
	);
	Damaris_api_call_handler(std::string cfg_object, PDI::Expression comm);
	Damaris_api_call_handler(std::string cfg_object);

	/** Returns the configured or default name of a Damaris event.
	 *
	 * \param event_type the Damaris event to name
	 * \return the event name, as it must be given to PDI_event() to trigger it
	 */
	std::string get_event_name(Event_type event_type);

	/** Tells whether a PDI event name directly names a Damaris API call
	 *  (i.e. it is one of the names returned by get_event_name()).
	 *
	 * \param event_name the PDI event name to check
	 * \return true if the event name triggers a Damaris API call
	 */
	bool is_damaris_api_call_event(std::string event_name);

	/** Executes the Damaris API call matching event_name.
	 *
	 * \param ctx the PDI context, used for logging
	 * \param m_damaris the Damaris client/server instance; lazily created on
	 *        the DAMARIS_INITIALIZE event
	 * \param event_name name of the Damaris API call to execute (see
	 *        damaris_event_names)
	 * \param expose_dataname names of data exposed as part of the ongoing
	 *        PDI_multi_expose transaction, used by calls that need extra
	 *        arguments (dataset name, position, block, data, ...)
	 * \param ... extra arguments required by some events (e.g. the dataset
	 *        name and data pointer for a write)
	 */
	void damaris_api_call_event(
		PDI::Context& ctx,
		std::unique_ptr<Damaris_wrapper>& m_damaris,
		std::string event_name,
		std::list<std::string> expose_dataname,
		...
	);

private:
	/** Creates the Damaris client/server instance (m_damaris) and initializes
	 *  the Damaris library.
	 *
	 * \param ctx the PDI context, used for logging
	 * \param m_damaris the Damaris client/server instance to create
	 * \param damaris_xml_object the Damaris XML configuration, as a string
	 */
	void damaris_pdi_init(PDI::Context& ctx, std::unique_ptr<Damaris_wrapper>& m_damaris, const char* damaris_xml_object);
}; // class Damaris_api_call_handler

} // namespace damaris_pdi

#endif // DAMARIS_API_CALL_HANDLER_H_
