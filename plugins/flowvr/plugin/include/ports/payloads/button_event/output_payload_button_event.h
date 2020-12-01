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

#ifndef PDI_FLOWVR_OUTPUT_PAYLOAD_BUTTON_EVENT
#define PDI_FLOWVR_OUTPUT_PAYLOAD_BUTTON_EVENT

#include <string>

#include <flowvr/module.h>
#include <ftl/chunkevents.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "../output_payload.h"
#include "payload_button_event.h"

namespace _flowvr_plugin {

/// Payload button event of output port
class Output_payload_button_event : public Payload_button_event, public Output_payload
{
	/// FlowVR chunk writer
	ftl::ChunkEventWriter m_chunk_event_writer;
	
	/// Pointer to flowvr::Port (to avoid dynamic_cast every time)
	flowvr::OutputPort* m_flowvr_output_port;
	
public:
	/** Creates output payload button event
	 * \param[in] ctx context of the output payload
	 * \param[in] name name of the port containing this output payload
	 * \param[in] config configuration node of the output payload
	 * \param[in] parent_port port that contains this output payload
	 */
	Output_payload_button_event(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::OutputPort* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other output payload to copy
	 */
	Output_payload_button_event(const Output_payload_button_event& other) = delete;
	
	/** Move constructor
	 * \param other[in] output payload to move
	 */
	Output_payload_button_event(Output_payload_button_event&& other);
	
	/** Deleted copy operator
	 * \param[in] other output payload to copy
	 */
	Output_payload_button_event& operator = (const Output_payload_button_event& other) = delete;
	
	/** Move operator
	 * \param[in] other output payload to move
	 */
	Output_payload_button_event& operator = (Output_payload_button_event&& other);
	
	/** Called if user accessing data descriptor
	 *
	 *  \param[in] data_name descriptor name
	 *  \param[in] ref reference to shared data
	 */
	void data(const std::string& data_name, const PDI::Ref_r& ref);
	
	flowvr::Stamps put_message(const flowvr::StampsWrite& stamps) override;
	
	/** Destroys output payload
	 */
	~Output_payload_button_event() override;
	
}; // class Output_payload_button_event

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_OUTPUT_PAYLOAD_BUTTON_EVENT
