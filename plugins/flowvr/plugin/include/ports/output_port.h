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

#ifndef PDI_FLOWVR_OUTPUT_PORT
#define PDI_FLOWVR_OUTPUT_PORT

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/paraconf_wrapper.h>

#include "payloads/output_payload.h"
#include "port.h"

namespace _flowvr_plugin {

/// Output port of a Module
class Output_port : public Port
{
	/// Payload of this port
	std::unique_ptr<Output_payload> m_payload;
	
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::OutputPort* m_flowvr_output_port;
	
	/// All port stamps excluding: it, num, source
	std::vector<std::reference_wrapper<const Stamp>> m_stamps_to_put;
	
	/** Fills stamps to put vector (all stamps excluding: it, num, source)
	 */
	void init_stamps_to_put();
	
	/** Reads configuration and load proper payload handler
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_payload(PC_tree_t config);
	
public:
	/** Creates new output port
	 * \param[in] ctx context of the output port
	 * \param[in] name name of the output port
	 * \param[in] config configuration node of the output port
	 */
	Output_port(PDI::Context& ctx, std::string name, PC_tree_t config);
	
	/** Deleted copy constructor
	 * \param[in] other output port to copy
	 */
	Output_port(const Output_port& other) = delete;
	
	/** Move constructor
	 * \param other output port to move
	 */
	Output_port(Output_port&& other);
	
	/** Deleted copy operator
	 * \param[in] other output port to copy
	 */
	Output_port& operator = (const Output_port& other) = delete;
	
	/** Move operator
	 * \param other output port to move
	 */
	Output_port& operator = (Output_port&& other);
	
	/** Prepares stamps to put, calls payload::put_message with this stamps
	 *  and updates stamps from sent message (sometimes it, num sent needed)
	 */
	void put_message();
	
	/** Destroys output port
	 */
	~Output_port();
	
}; // class Output_port

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PORT