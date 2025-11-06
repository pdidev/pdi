/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2024 National Institute for Research in Digital Science and Technology (Inria)
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
#include <string>
#include <map>
#include <unordered_map>
#include <unordered_set>
#include <list>

#include <pdi/context.h>

#include <Damaris.h>
#include "damaris_wrapper.h"
#include "damaris_cfg.h"

using PDI::Context;
using std::list;
using std::string;
using std::unique_ptr;

namespace damaris_pdi {

class Damaris_api_call_handler
{

	std::string xml_config_object;
	PDI::Expression m_communicator;
	std::string m_init_on_event = "";
	std::string m_start_on_event = "";
	std::string m_stop_on_event = "";
	std::string m_finalize_on_event = "";

public:
	Damaris_api_call_handler(std::string cfg_object, PDI::Expression comm, std::string init_on_event, std::string start_on_event, std::string stop_on_event);
	Damaris_api_call_handler(std::string cfg_object, PDI::Expression comm);
	Damaris_api_call_handler(std::string cfg_object);

    std::string get_event_name(Event_type event_type);

    bool is_damaris_api_call_event(std::string event_name);

    void damaris_api_call_event(Context& ctx, unique_ptr<Damaris_wrapper> &m_damaris, std::string event_name, list<string> expose_dataname, ...);

private:
	void damaris_pdi_init(Context& ctx, unique_ptr<Damaris_wrapper> &m_damaris, const char* damaris_xml_object) ;
}; // class Damaris_api_call_handler

} // namespace damaris_pdi

#endif // DAMARIS_API_CALL_HANDLER_H_