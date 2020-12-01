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

#ifndef PDI_FLOWVR_PORT
#define PDI_FLOWVR_PORT

#include <functional>
#include <memory>
#include <string>

#include <flowvr/module.h>

#include <pdi/paraconf_wrapper.h>

#include "stamps/stamp.h"

namespace _flowvr_plugin {

/// Base class for input and output ports
class Port
{
protected:
	/// Context of this port
	PDI::Context& m_ctx;
	
	/// Name of the port
	std::string m_name;
	
	// FlowVR port pointer
	std::unique_ptr<flowvr::Port> m_flowvr_port;
	
	/// Stamps signed to this port (every message is stamped with this stamps)
	std::vector<Stamp> m_stamps;
	
	/// Function to remove isConnected callback
	std::function<void()> m_remove_callback;
	
	/** Creates a new port
	 * \param[in] ctx context of the port
	 * \param[in] name name of the port
	 * \param[in] config configuration node of the port
	 */
	Port(PDI::Context& ctx, const std::string& name, PC_tree_t config);
	
	/** Deleted copy constructor
	 * \param[in] other port to copy
	 */
	Port(const Port& other) = delete;
	
	/** Move constructor
	 * \param other port to move
	 */
	Port(Port&& other);
	
	/** Deleted copy operator
	 * \param[in] other port to copy
	 */
	Port& operator=(const Port& other) = delete;
	
	/** Move operator
	 * \param other port to move
	 */
	Port& operator=(Port&& other);
	
	/** Reads configuration and creates stamps.
	 *  Can be called only after m_flowvr_port initialization.
	 *
	 *  \param[in] config the configuration to read (port root)
	 */
	void load_stamps(PC_tree_t config);
	
	/** Register stamps in flowvr::Port stamp list
	 */
	void fill_stamp_list();
	
	/** Updates Port stamps from received stamp
	 *
	 *  \param[in] read_stamps received stamps from flowvr::Message
	 */
	void update_stamps(const flowvr::Stamps& read_stamps);
	
	/** Writes the connection status
	 *
	 *  \param[in] data_name name of descriptor
	 *  \param[in] status_ref reference where to write connection status
	 */
	void isConnected(const std::string& data_name, PDI::Ref_w ref);
	
public:

	/** Returns name of the port
	 * \return name of the port
	 */
	const std::string& name() const;
	/**
	 *  \param[out] flowvr::Port pointer signed to this Port
	 */
	flowvr::Port* get_flowvr_port() const;
	
	/** Returns connection status of this port
	 * \return connection status of this port
	 */
	bool isConnected() const;
	
	/** Destroys port
	 */
	virtual ~Port();
	
}; // class Port

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_PORT
