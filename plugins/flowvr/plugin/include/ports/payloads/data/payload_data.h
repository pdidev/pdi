/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_FLOWVR_PAYLOAD_DATA
#define PDI_FLOWVR_PAYLOAD_DATA

#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

namespace _flowvr_plugin {

/// Base class for data payload
class Payload_data
{
protected:
	/// Context of this payload
	PDI::Context& m_ctx;
	
	/// Port that contains this payload
	flowvr::Port* m_parent_port;
	
	/// Name of the port that contains this payload
	std::string m_name;
	
	/// Name of the descriptor where to read/write data
	std::string m_data_desc;
	
	/// True if plugin is sharing buffer
	bool m_sharing_buffer;
	
	/// Type for data_selection to copy
	PDI::Datatype_template_ptr m_data_selection;
	
	/// Callbacks to remove on payload destruction
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/** Creates payload data
	 * \param[in] ctx context of the payload
	 * \param[in] name name of the port containing this payload
	 * \param[in] config configuration node of the payload
	 * \param[in] parent_port port that contains this payload
	 */
	Payload_data(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other payload to copy
	 */
	Payload_data(const Payload_data& other) = delete;
	
	/** Move constructor
	 * \param other payload to move
	 */
	Payload_data(Payload_data&& other);
	
	/** Deleted copy operator
	 * \param[in] other payload to copy
	 */
	Payload_data& operator=(const Payload_data& other) = delete;
	
	/** Move operator
	 * \param other payload to move
	 */
	Payload_data& operator=(Payload_data&& other);
	
public:
	/** Destroys payload
	 */
	virtual ~Payload_data();
	
}; // Payload_data

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PAYLOAD_DATA
