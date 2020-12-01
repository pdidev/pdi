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

#ifndef PDI_FLOWVR_PAYLOAD_MOUSE_EVENT
#define PDI_FLOWVR_PAYLOAD_MOUSE_EVENT

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

#include <flowvr/module.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

namespace _flowvr_plugin {

/// Base class for mouse event input and output payload
class Payload_mouse_event
{
	std::unordered_map<std::string, unsigned char> m_name_to_flowvr_key {
		{"LEFT_BUTTON",   0x01},
		{"MIDDLE_BUTTON", 0x02},
		{"RIGHT_BUTTON",  0x04},
		{"POS_XY",        0x08}
	};
	
protected:
	/// Context of this payload
	PDI::Context& m_ctx;
	
	/// Port that contains this payload
	flowvr::Port* m_parent_port;
	
	/// Name of the port that contains this payload
	std::string m_name;
	
	/// Loaded keys (defined in config)
	std::unordered_map<unsigned char, std::string> m_key_desc;
	
	/// Position of the mouse
	std::pair<std::string, std::pair<unsigned char, float[2]>> m_desc_pos_xy;
	
	/// Map descriptors to key and its value
	std::unordered_map<std::string, std::pair<unsigned char, bool>> m_desc_value_map;
	
	/// Callbacks to remove on destruction
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/** Load defined descriptors keys
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_key_desc(PC_tree_t config);
	
	/** Creates payload mouse event
	 * \param[in] ctx context of the payload
	 * \param[in] name name of the port containing this payload
	 * \param[in] config configuration node of the payload
	 * \param[in] parent_port port that contains this payload
	 */
	Payload_mouse_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other payload to copy
	 */
	Payload_mouse_event(const Payload_mouse_event& other) = delete;
	
	/** Move constructor
	 * \param[in] other payload to move
	 */
	Payload_mouse_event(Payload_mouse_event&& other);
	
	/** Deleted copy operator
	 * \param[in] other payload to copy
	 */
	Payload_mouse_event& operator = (const Payload_mouse_event& other) = delete;
	
	/** Move operator
	 * \param[in] other payload to move
	 */
	Payload_mouse_event& operator = (Payload_mouse_event&& other);
	
public:
	/** Destroys payload
	 */
	virtual ~Payload_mouse_event();
	
}; // class Payload_mouse_event

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PAYLOAD_MOUSE_EVENT
