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

#ifndef PDI_FLOWVR_INPUT_PAYLOAD_MOUSE_EVENT
#define PDI_FLOWVR_INPUT_PAYLOAD_MOUSE_EVENT

#include <string>

#include <flowvr/module.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "../input_payload.h"
#include "payload_mouse_event.h"

namespace _flowvr_plugin {

/// Mouse event input payload
class Input_payload_mouse_event : public Payload_mouse_event, public Input_payload
{
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::InputPort* m_flowvr_input_port;
	
public:
	/** Creates input payload mouse event
	 * \param[in] ctx context of the payload
	 * \param[in] name name of the port containing this payload
	 * \param[in] config configuration node of the payload
	 * \param[in] parent_port port that contains this payload
	 */
	Input_payload_mouse_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other payload to copy
	 */
	Input_payload_mouse_event(const Input_payload_mouse_event& other) = delete;
	
	/** Move constructor
	 * \param[in] other payload to move
	 */
	Input_payload_mouse_event(Input_payload_mouse_event&& other);
	
	/** Deleted copy operator
	 * \param[in] other payload to copy
	 */
	Input_payload_mouse_event& operator=(const Input_payload_mouse_event& other) = delete;
	
	/** Move operator
	 * \param[in] other payload to move
	 */
	Input_payload_mouse_event& operator=(Input_payload_mouse_event&& other);
	
	/** Called if user shares data descriptor, writes mouse position
	 *
	 * \param[in] data_name descriptor name
	 * \param[in] ref reference where write data
	 */
	void data_pos_xy(const std::string& data_name, const PDI::Ref_w& ref) const;
	
	/** Called if user shares data descriptor, writes mouse button status
	 *
	 * \param[in] data_name descriptor name
	 * \param[in] ref reference where write data
	 */
	void data(const std::string& data_name, const PDI::Ref_w& ref) const;
	
	flowvr::Stamps get_message() override;
	
	~Input_payload_mouse_event() override;
	
}; // class Input_payload_mouse_event

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_INPUT_PAYLOAD_MOUSE_EVENT
