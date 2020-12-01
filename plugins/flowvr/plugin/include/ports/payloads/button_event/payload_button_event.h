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

#ifndef PDI_FLOWVR_PAYLOAD_BUTTON_EVENT
#define PDI_FLOWVR_PAYLOAD_BUTTON_EVENT

#include <functional>
#include <string>
#include <unordered_map>
#include <vector>

#include <flowvr/module.h>
#include <ftl/chunkevents.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

namespace _flowvr_plugin {

/// Base class for button event payload
class Payload_button_event
{
	/// Map string value to FlowVR key values
	std::unordered_map<std::string, unsigned char> m_name_to_flowvr_key {
		{"KEY_F1",        FLOWVR_KEY_F1},
		{"KEY_F2",        FLOWVR_KEY_F2},
		{"KEY_F3",        FLOWVR_KEY_F3},
		{"KEY_F4",        FLOWVR_KEY_F4},
		{"KEY_F5",        FLOWVR_KEY_F5},
		{"KEY_F6",        FLOWVR_KEY_F6},
		{"KEY_F7",        FLOWVR_KEY_F7},
		{"KEY_F8",        FLOWVR_KEY_F8},
		{"KEY_F9",        FLOWVR_KEY_F9},
		{"KEY_F10",       FLOWVR_KEY_F10},
		{"KEY_F11",       FLOWVR_KEY_F11},
		{"KEY_F12",       FLOWVR_KEY_F12},
		{"KEY_LEFT",      FLOWVR_KEY_LEFT},
		{"KEY_UP",        FLOWVR_KEY_UP},
		{"KEY_RIGHT",     FLOWVR_KEY_RIGHT},
		{"KEY_DOWN",      FLOWVR_KEY_DOWN},
		{"KEY_PAGE_UP",   FLOWVR_KEY_PAGE_UP},
		{"KEY_PAGE_DOWN", FLOWVR_KEY_PAGE_DOWN},
		{"KEY_HOME",      FLOWVR_KEY_HOME},
		{"KEY_END",       FLOWVR_KEY_END},
		{"KEY_INSERT",    FLOWVR_KEY_INSERT}
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
	
	/// Map descriptors to key and its value
	std::unordered_map<std::string, std::pair<unsigned char, bool>> m_desc_value_map;
	
	/// Callbacks to remove on destruction
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/** Load defined descriptors keys
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_key_desc(PC_tree_t config);
	
	/** Creates payload button event
	 * \param[in] ctx context of the payload
	 * \param[in] name name of the port containing this payload
	 * \param[in] config configuration node of the payload
	 * \param[in] parent_port port that contains this payload
	 */
	Payload_button_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other payload to copy
	 */
	Payload_button_event(const Payload_button_event& other) = delete;
	
	/** Move constructor
	 * \param[in] other payload to move
	 */
	Payload_button_event(Payload_button_event&& other);
	
	/** Deleted copy operator
	 * \param[in] other payload to copy
	 */
	Payload_button_event& operator=(const Payload_button_event& other) = delete;
	
	/** Move operator
	 * \param[in] other payload to move
	 */
	Payload_button_event& operator=(Payload_button_event&& other);
	
public:
	/** Destroys payload
	 */
	virtual ~Payload_button_event();
	
}; // class Payload_button_event

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PAYLOAD_BUTTON_EVENT
