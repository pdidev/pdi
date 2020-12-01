/*******************************************************************************
 * Copyright (C) 2018-2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_FLOWVR_INPUT_PORT
#define PDI_FLOWVR_INPUT_PORT

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/paraconf_wrapper.h>

#include "payloads/input_payload.h"
#include "port.h"

namespace _flowvr_plugin {

/// Input port of a Module
class Input_port : public Port
{
	/// Payload of this port
	std::unique_ptr<Input_payload> m_payload;
	
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::InputPort* m_flowvr_input_port;
	
	/**
	 *  Reads configuration and return if this is event Port
	 *
	 *  \param[in] config the configuration to read (port root)
	 *  \return true if event port, false otherwise
	 */
	bool event_port(PC_tree_t config);
	
	/**
	 *  Reads configuration and load proper payload handler
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_payload(PC_tree_t config);
	
public:
	/** Creates new input port
	 * \param[in] ctx context of the input port
	 * \param[in] name name of the input port
	 * \param[in] config configuration node of the input port
	 */
	Input_port (PDI::Context& ctx, const std::string& name, PC_tree_t config);
	
	/** Deleted copy constructor
	 * \param[in] other input port to copy
	 */
	Input_port(const Input_port& other) = delete;
	
	/** Move constructor
	 * \param other input port to move
	 */
	Input_port(Input_port&& other);
	
	/** Deleted copy operator
	 * \param[in] other input port to copy
	 */
	Input_port& operator=(const Input_port& other) = delete;
	
	/** Move operator
	 * \param other input port to move
	 */
	Input_port& operator=(Input_port&& other);
	
	/** Get message from FlowVR port and updates stamps
	 */
	void get_message();
	
	/** Destroys input port
	 */
	~Input_port();
	
}; // Input_port

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PORT