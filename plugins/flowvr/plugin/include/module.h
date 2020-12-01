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

#ifndef PDI_FLOWVR_MODULE
#define PDI_FLOWVR_MODULE

#include <memory>
#include <string>
#include <utility>
#include <vector>

#include <flowvr/module.h>
#include <ftl/chunkwriter.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "component.h"
#include "ports/input_port.h"
#include "ports/output_port.h"
#include "traces/trace.h"

namespace _flowvr_plugin {

using Flowvr_module_uptr = std::unique_ptr<flowvr::ModuleAPI, std::function<void(flowvr::ModuleAPI*)>>;

/// Module component
class Module : public Component
{
	/// FlowVR module
	Flowvr_module_uptr m_flowvr_module;
	
	/// True if first wait wasn't called, false otherwise
	bool m_fisrt_wait;
	
	/// Name of the module (optional)
	std::string m_module_name;
	
	/// Name of the instance (optional)
	std::string m_instance_name;
	
	/// If true, getting abort signal is expected, throws exception otherwise (defalut = true)
	bool m_silent_abort;
	
	/// If true, send abort signal on finalize
	bool m_abort_on_finalze;
	
	/// Input ports of this module
	std::vector<Input_port> m_input_ports;
	
	/// Output ports of this module
	std::vector<Output_port> m_output_ports;
	
	/// Traces of this module
	std::vector<Trace> m_traces;
	
private:
	/** Loads user-defined descriptors names
	 *
	 * \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_desc_names(PC_tree_t config);
	
	/** Reads configuration and creates input ports
	 *
	 * \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_input_ports(PC_tree_t config);
	
	/** Reads configuration and creates output ports
	 *
	 * \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_output_ports(PC_tree_t config);
	
	/** Reads configuration and creates traces
	 *
	 * \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_traces(PC_tree_t config);
	
	/** Reads configuration and saves module name
	 *
	 * \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_module_name(PC_tree_t config);
	
	/** Create flowvr::Module. Calls ports method to sign to them this module.
	 */
	void initialize_flowvr_module();
	
	/** Updates logger to show module
	 */
	void update_logger();
	
public:
	/** Creates new flowvr module
	 * \param[in] ctx context of the module
	 * \param[in] config configuration node of the module
	 */
	Module(PDI::Context& ctx, PC_tree_t config);
	
	/** Deleted copy constructor
	 * \param[in] other module to copy
	 */
	Module(const Module& other) = delete;
	
	/** Move constructor
	 * \param[in] other module to move
	 */
	Module(Module&& other);
	
	/** Deleted copy operator
	 * \param[in] other module to copy
	 */
	Module& operator=(const Module& other) = delete;
	
	/** Move operator
	 * \param[in] other module to move
	 */
	Module& operator=(Module&& other);
	
private:
	/** Call flowvr::wait(), receive messages from all ports. If first wait, only send messages.
	 * \return wait value from flowvr
	 */
	int wait();
	
	/** Aborts flowvr
	 *
	 * \param[in] abort_event name of the abort event
	 */
	void abort(const std::string& abort_event);
	
	/** Writes status value to Ref
	 *
	 * \param[in] status_ref reference where to save status
	 */
	void status(PDI::Ref_w status_ref);
	
	/** Call flowvr::wait(), receive messages from all ports. If first wait, only send messages.
	 *
	 * \param[in] data_name name of the descriptor
	 * \param[in] wait_ref reference where to save wait status
	 */
	void wait(const std::string& data_name, PDI::Ref_w wait_ref);
	
	/** Writes parallel rank to Ref
	 *
	 * \param[in] data_name name of the descriptor
	 * \param[in] rank_ref reference where to write parallel rank
	 */
	void get_parallel_rank(const std::string& data_name, PDI::Ref_w rank_ref);
	
	/**
	 *  Saves parallel size to Ref
	 *
	 * \param[in] data_name name of the descriptor
	 * \param[in] size_ref reference where to write parallel size
	 */
	void get_parallel_size(const std::string& data_name, PDI::Ref_w size_ref);
	
public:
	/** Destroys module
	 */
	~Module() override;
	
}; // class Module

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_MODULE
