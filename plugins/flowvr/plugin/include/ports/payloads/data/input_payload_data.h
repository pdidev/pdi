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

#ifndef PDI_FLOWVR_INPUT_PAYLOAD_DATA
#define PDI_FLOWVR_INPUT_PAYLOAD_DATA

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "payload_data.h"
#include "../input_payload.h"

namespace _flowvr_plugin {

/// Input data payload
class Input_payload_data : public Input_payload, public Payload_data
{
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::InputPort* m_parent_input_port;
	
	/// Flowvr buffer where the data is stored
	flowvr::Buffer m_flowvr_buffer;
	
	/// Name of the descritor to put the size of the received message
	std::string m_data_size_desc;
	
	/** Read config and sets m_data_size_desc
	 * \param[in] input_port_node config of input port node
	 */
	void load_data_size_desc(PC_tree_t input_port_node);
	
public:
	/** Creates input payload data
	 * \param[in] ctx context of the input payload
	 * \param[in] name name of the port containing this input payload
	 * \param[in] config configuration node of the input payload
	 * \param[in] parent_port port that contains this input payload
	 */
	Input_payload_data (PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::InputPort* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other input payload to copy
	 */
	Input_payload_data(const Input_payload_data& other) = delete;
	
	/** Move constructor
	 * \param other input payload to move
	 */
	Input_payload_data(Input_payload_data&& other);
	
	/** Deleted copy operator
	 * \param[in] other input payload to copy
	 */
	Input_payload_data& operator=(const Input_payload_data& other) = delete;
	
	/** Move operator
	 * \param[in] other input payload to move
	 */
	Input_payload_data& operator=(Input_payload_data&& other);
	
	/** Called if user shares data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference where to copy data
	 */
	void copy_data_to_ref(const std::string& data_name, const PDI::Ref_w& ref) const;
	
	/** Share the reference to the flowvr memory as data_name descriptor
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name);
	
	/** Get FlowVR message
	 *
	 *  \return stamp from flowvr::Message
	 */
	flowvr::Stamps get_message() override;
	
	/** Destroys input payload
	 */
	~Input_payload_data() override;
	
}; // Input_payload_data

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_INPUT_PAYLOAD_DATA
