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

#ifndef PDI_FLOWVR_PAYLOAD_CHUNK
#define PDI_FLOWVR_PAYLOAD_CHUNK

#include <algorithm>
#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>
#include <spdlog/spdlog.h>

namespace _flowvr_plugin {

/// Holds information about chunks sizes
struct Chunk_info {
	/// Chunk count
	size_t chunk_count;
	
	/// Size of each chunk
	std::unique_ptr<size_t[]> chunks_sizes; // array[chunk_count]
	
	/** Constructs empty chunk info
	 */
	Chunk_info();
};

/// Base class for chunk payload
class Payload_chunk
{
protected:
	/// Context of this payload
	PDI::Context& m_ctx;
	
	/// Port that contains this payload
	flowvr::Port* m_parent_port;
	
	/// Name of the port that contains this payload
	std::string m_name;
	
	/// Names of the descriptors where to read/write data
	std::vector<std::string> m_chunk_descs;
	
	/// Callbacks to remove on destruction
	std::vector<std::function<void()>> m_callbacks_remove;
	
	/// Chunks informations (how many and their sizes)
	Chunk_info m_chunk_info;
	
	/// True if plugin is sharing chunks, false otherwise
	bool m_sharing_chunks;
	
	/** Creates payload chunk event
	 * \param[in] ctx context of the payload
	 * \param[in] name name of the port containing this payload
	 * \param[in] config configuration node of the payload
	 * \param[in] parent_port port that contains this payload
	 */
	Payload_chunk(PDI::Context& ctx, const std::string& name, PC_tree_t config, flowvr::Port* parent_port);
	
	/** Deleted copy constructor
	 * \param[in] other payload to copy
	 */
	Payload_chunk(const Payload_chunk& other) = delete;
	
	/** Move constructor
	 * \param[in] other payload to move
	 */
	Payload_chunk(Payload_chunk&& other);
	
	/** Deleted copy operator
	 * \param[in] other payload to copy
	 */
	Payload_chunk& operator=(const Payload_chunk& other) = delete;
	
	/** Move operator
	 * \param[in] other payload to move
	 */
	Payload_chunk& operator=(Payload_chunk&& other);
	
	/** Share the reference to the flowvr memory to the data_name descriptor
	 *
	 *  \param[in] data_name name of shared descriptor
	 */
	virtual void empty_desc_access(const std::string& data_name) = 0;
	
public:
	/** Destroys payload
	 */
	virtual ~Payload_chunk();
	
}; // Payload_chunk

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PAYLOAD_CHUNK
