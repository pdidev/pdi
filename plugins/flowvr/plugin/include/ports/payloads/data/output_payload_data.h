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

#ifndef PDI_FLOWVR_OUTPUT_PAYLOAD_DATA
#define PDI_FLOWVR_OUTPUT_PAYLOAD_DATA

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "payload_data.h"
#include "../output_payload.h"

namespace _flowvr_plugin {

/// Output data payload
class Output_payload_data : public Output_payload, public Payload_data
{
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::OutputPort* m_parent_output_port;
	
	/// Flowvr buffer where the data is written
	flowvr::BufferWrite m_flowvr_buffer;
	
	/// FlowVR pool, used if data size is const
	std::unique_ptr<flowvr::BufferPool> m_flowvr_buffer_pool;
	
	/** Allocates FlowVR shared memory buffer
	 */
	void alloc_buffer();
	
public:
	/** Creates output payload data
	 * \param[in] ctx context of the output payload
	 * \param[in] name name of the port containing this output payload
	 * \param[in] config configuration node of the output payload
	 * \param[in] parent_port port that contains this output payload
	 */
	Output_payload_data(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other output payload to copy
	 */
	Output_payload_data(const Output_payload_data& other) = delete;
	
	/** Move constructor
	 * \param[in] other output output payload to move
	 */
	Output_payload_data(Output_payload_data&& other);
	
	/** Deleted copy operator
	 * \param[in] other output payload to copy
	 */
	Output_payload_data& operator = (const Output_payload_data& other) = delete;
	
	/** Move operator
	 * \param other output payload to move
	 */
	Output_payload_data& operator = (Output_payload_data&& other);
	
	/** Called if user shares data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference from we make a copy data
	 */
	void copy_data_from_ref(const std::string& data_name, const PDI::Ref_r& ref);
	
	/** Allocate the flowvr memory and share the reference to it
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	void empty_desc_access(const std::string& data_name);
	
	/** Put a message with data payload and stamps from argument
	 *
	 *  \param[in] stamps flowvr::Stamps to put to the message
	 *  \return stamp from sent flowvr::Message
	 */
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override;
	
	/** Destroys output payload
	 */
	~Output_payload_data() override;
	
}; // class Output_payload_data

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_OUTPUT_PAYLOAD_DATA
