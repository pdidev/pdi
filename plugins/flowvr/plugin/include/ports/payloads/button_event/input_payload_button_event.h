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

#ifndef PDI_FLOWVR_INPUT_PAYLOAD_BUTTON_EVENT
#define PDI_FLOWVR_INPUT_PAYLOAD_BUTTON_EVENT

#include <functional>
#include <memory>
#include <string>

#include <flowvr/module.h>
#include <ftl/chunkevents.h>

#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "../input_payload.h"
#include "payload_button_event.h"

namespace _flowvr_plugin {

/// Payload button event of input port
class Input_payload_button_event : public Payload_button_event, public Input_payload
{
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::InputPort* m_flowvr_input_port;
	
public:
	/** Creates input payload button event
	 * \param[in] ctx context of the input payload
	 * \param[in] name name of the port containing this input payload
	 * \param[in] config configuration node of the input payload
	 * \param[in] parent_port port that contains this input payload
	 */
	Input_payload_button_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other input payload to copy
	 */
	Input_payload_button_event(const Input_payload_button_event& other) = delete;
	
	/** Move constructor
	 * \param other[in] input payload to move
	 */
	Input_payload_button_event(Input_payload_button_event&& other);
	
	/** Deleted copy operator
	 * \param[in] other input payload to copy
	 */
	Input_payload_button_event& operator=(const Input_payload_button_event& other) = delete;
	
	/** Move operator
	 * \param[in] other input payload to move
	 */
	Input_payload_button_event& operator=(Input_payload_button_event&& other);
	
	/** Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference to shared data
	 */
	void data(const std::string& data_name, const PDI::Ref_w& ref) const;
	
	flowvr::Stamps get_message() override;
	
	/** Destroys input payload
	 */
	~Input_payload_button_event() override;
	
}; // class Input_payload_button_event

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_INPUT_PAYLOAD_BUTTON_EVENT
