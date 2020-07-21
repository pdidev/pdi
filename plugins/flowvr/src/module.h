/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <pdi/context_proxy.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "component.h"
#include "input_port.h"
#include "output_port.h"
#include "trace.h"

namespace  {

using Flowvr_module_uptr = std::unique_ptr<flowvr::ModuleAPI, std::function<void(flowvr::ModuleAPI*)>>;

class Module : public Component
{
	/**************************************************************************
	 * FlowVR module members
	 */
	
	Flowvr_module_uptr m_flowvr_module;
	
	bool m_fisrt_wait;          // on first wait cannot read messages
	std::string m_module_name;  // name of the module (optional)
	bool m_silent_abort;
	bool m_abort_on_finalze; // if true call abort on finalize
	
	std::vector<Input_port> m_input_ports;   // input ports of this module
	std::vector<Output_port> m_output_ports; // output ports of this module
	std::vector<Trace> m_traces; // traces of this module
	
	/*
	 * End of "FlowVR module members"
	 **************************************************************************/
	
	/**************************************************************************
	 * FlowVR initialization
	 */
private:
	/**
	 *  Loads user-defined descriptors names
	 *
	 *  \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_desc_names(PC_tree_t config)
	{
		PC_tree_t wait_on_data_node = PC_get(config, ".wait_on_data");
		if (!PC_status(wait_on_data_node)) {
			std::string wait_data = PDI::to_string(wait_on_data_node);
			context().logger()->debug("(FlowVR) Module: wait_on_data = {}", wait_data);
			context().add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->wait(name, ref);
			}, wait_data);
		}
		
		PC_tree_t wait_on_node = PC_get(config, ".wait_on");
		if (!PC_status(wait_on_node)) {
			context().logger()->warn("(FlowVR) Module ({}): Wait is called as an event. Use `wait_on_data` if you can to check wait status.", m_module_name);
			if (!PC_status(PC_get(wait_on_node, "[0]"))) {
				int nb_event = PDI::len(wait_on_node);
				for (int event_id = 0; event_id < nb_event; event_id++) {
					std::string wait_event = PDI::to_string(PC_get(wait_on_node, "[%d]", event_id));
					context().add_event_callback([this](const std::string& name) {
						if (this->wait() == 0) {
							m_input_ports.clear();
							m_output_ports.clear();
							m_traces.clear();
							m_flowvr_module.reset();
							if (this->m_silent_abort == false) {
								throw PDI::System_error{"(FlowVR) Module ({}): Received abort signal from FlowVR", this->m_module_name};
							}
						}
					}, wait_event);
				}
			} else {
				std::string wait_event = PDI::to_string(wait_on_node);
				context().add_event_callback([this](const std::string& name) {
					if (this->wait() == 0) {
						m_input_ports.clear();
						m_output_ports.clear();
						m_traces.clear();
						m_flowvr_module.reset();
						if (this->m_silent_abort == false) {
							throw PDI::System_error{"(FlowVR) Module ({}): Received abort signal from FlowVR", this->m_module_name};
						}
					}
				}, wait_event);
			}
		}
		
		PC_tree_t status_node = PC_get(config, ".status");
		if (!PC_status(status_node)) {
			if (!PC_status(PC_get(status_node, "[0]"))) {
				int nb_status = PDI::len(status_node);
				for (int status_id = 0; status_id < nb_status; status_id++) {
					std::string status_data = PDI::to_string(PC_get(status_node, "[%d]", status_id));
					context().add_data_callback([this](const std::string& name, PDI::Ref ref) {
						this->status(ref);
					}, status_data);
				}
			} else {
				std::string status_data = PDI::to_string(status_node);
				context().add_data_callback([this](const std::string& name, PDI::Ref ref) {
					this->status(ref);
				}, status_data);
			}
		}
		
		PC_tree_t parallel_node = PC_get(config, ".parallel");
		if (!PC_status(parallel_node)) {
			PC_tree_t set_rank_node = PC_get(parallel_node, ".set_rank");
			PC_tree_t set_size_node = PC_get(parallel_node, ".set_size");
			if (!PC_status(set_rank_node)) {
				if (!PC_status(set_size_node)) {
					int set_rank = PDI::Expression(PDI::to_string(set_rank_node)).to_long(context());
					int set_size = PDI::Expression(PDI::to_string(set_size_node)).to_long(context());
					flowvr::Parallel::init(set_rank, set_size);
					context().logger()->debug("(FlowVR) Module: Parallel: rank = {}, size = {}", set_rank, set_size);
				} else {
					throw PDI::Config_error{"(FlowVR) Module: `set_rank' is defined, but `set_size' is not"};
				}
			} else {
				if (!PC_status(set_size_node)) {
					throw PDI::Config_error{"(FlowVR) Module: `set_size' is defined, but `set_rank' is not"};
				} else {
					flowvr::Parallel::init(true);
				}
			}
			
			PC_tree_t get_rank_node = PC_get(parallel_node, ".get_rank");
			if (!PC_status(get_rank_node)) {
				std::string get_parallel_rank_data = PDI::to_string(get_rank_node);
				context().logger()->debug("(FlowVR) Module: Parallel rank = {}", get_parallel_rank_data);
				context().add_data_callback([this](const std::string& name, PDI::Ref ref) {
					this->get_parallel_rank(name, ref);
				}, get_parallel_rank_data);
			}
			
			PC_tree_t get_size_node = PC_get(parallel_node, ".get_size");
			if (!PC_status(get_size_node)) {
				std::string get_parallel_size_data = PDI::to_string(get_size_node);
				context().logger()->debug("(FlowVR) Module: Parallel size = {}", get_parallel_size_data);
				context().add_data_callback([this](const std::string& name, PDI::Ref ref) {
					this->get_parallel_size(name, ref);
				}, get_parallel_size_data);
			}
		}
		
		PC_tree_t abort_node = PC_get(config, ".abort_on");
		if (!PC_status(abort_node)) {
			if (!PC_status(PC_get(abort_node, "[0]"))) {
				int nb_abort = PDI::len(abort_node);
				for (int abort_id = 0; abort_id < nb_abort; abort_id++) {
					std::string abort_event = PDI::to_string(PC_get(abort_node, "[%d]", abort_id));
					context().add_event_callback([this](const std::string& name) {
						this->abort(name);
					}, abort_event);
				}
			} else {
				std::string abort_event = PDI::to_string(abort_node);
				context().add_event_callback([this](const std::string& name) {
					this->abort(name);
				}, abort_event);
			}
		}
		
		PC_tree_t silent_abort_node = PC_get(config, ".silent_abort");
		if (!PC_status(silent_abort_node) && PDI::to_bool(silent_abort_node)) {
			m_silent_abort = true;
			context().logger()->debug("(FlowVR) Module: Silent abort = true");
		}
		
		PC_tree_t abort_on_fin_node = PC_get(config, ".abort_on_finalize");
		if (!PC_status(abort_on_fin_node) && PDI::to_bool(abort_on_fin_node)) {
			m_abort_on_finalze = true;
			context().logger()->debug("(FlowVR) Module: Abort on finalize = true");
		}
		context().logger()->debug("(FlowVR) Module: Loaded basic configuration");
	}
	
	/**
	 *  Reads configuration and creates input ports
	 *
	 *  \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_input_ports(PC_tree_t config)
	{
		PC_tree_t input_node = PC_get(config, ".input_ports");
		if (!PC_status(input_node)) {
			int nb_ports = PDI::len(input_node, 0);
			for (int port_id = 0; port_id < nb_ports; port_id++) {
				std::string port_name = PDI::to_string(PC_get(input_node, "{%d}", port_id));
				PC_tree_t port_node = PC_get(input_node, "<%d>", port_id);
				m_input_ports.emplace_back(Input_port(context(), port_name, port_node));
			}
		}
		context().logger()->debug("(FlowVR) Module: Loaded input ports");
	}
	
	/**
	 *  Reads configuration and creates output ports
	 *
	 *  \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_output_ports(PC_tree_t config)
	{
		PC_tree_t output_node = PC_get(config, ".output_ports");
		if (!PC_status(output_node)) {
			int nb_ports = PDI::len(output_node, 0);
			for (int port_id = 0; port_id < nb_ports; port_id++) {
				std::string port_name = PDI::to_string(PC_get(output_node, "{%d}", port_id));
				PC_tree_t port_node = PC_get(output_node, "<%d>", port_id);
				m_output_ports.emplace_back(Output_port(context(), port_name, port_node));
			}
		}
		context().logger()->debug("(FlowVR) Module: Loaded output ports");
	}
	
	/**
	 *  Reads configuration and creates traces
	 *
	 *  \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_traces(PC_tree_t config)
	{
		PC_tree_t traces_node = PC_get(config, ".traces");
		if (!PC_status(traces_node)) {
			int nb_traces = PDI::len(traces_node, 0);
			for (int trace_id = 0; trace_id < nb_traces; trace_id++) {
				std::string trace_name = PDI::to_string(PC_get(traces_node, "{%d}", trace_id));
				PC_tree_t trace_node = PC_get(traces_node, "<%d>", trace_id);
				m_traces.emplace_back(Trace(context(), trace_name, trace_node));
			}
		}
		context().logger()->debug("(FlowVR) Module: Loaded traces");
	}
	
	/**
	 *  Reads configuration and saves module name
	 *
	 *  \param[in] config the configuration to read (flowvr plugin root)
	 */
	void load_module_name(PC_tree_t config)
	{
		PC_tree_t name_node = PC_get(config, ".name");
		if (!PC_status(name_node)) {
			m_module_name = PDI::to_string(name_node);
		}
		context().logger()->debug("(FlowVR) Module ({}): Loaded name", m_module_name);
	}
	
	/**
	 *  Prepare ports addresses vector, prepare traces addresses (passed to initModule).
	 *  Create flowvr::Module. Calls ports method to sign to them this module.
	 */
	void initialize_flowvr_module()
	{
		std::vector<flowvr::Port*> m_ports_addresses;
		for (const auto& port : m_input_ports) {
			m_ports_addresses.push_back(port.get_flowvr_port());
		}
		for (const auto& port : m_output_ports) {
			m_ports_addresses.push_back(port.get_flowvr_port());
		}
		
		std::vector<flowvr::Trace*> m_traces_addresses;
		for (const auto& trace : m_traces) {
			m_traces_addresses.push_back(trace.get());
		}
		
		m_flowvr_module = Flowvr_module_uptr(flowvr::initModule(m_ports_addresses, m_traces_addresses, m_module_name), [](flowvr::ModuleAPI* module) {
			module->close();
		});
		context().logger()->debug("(FlowVR) Module ({}): Initialized flowvr module", m_module_name);
	}
	
	/**
	 *  Updates logger to show module
	 */
	void update_logger()
	{
		char format[128];
		snprintf(format, 128, "[PDI][%s][%%T] *** %%^%%l%%$: %%v", m_flowvr_module->getID().c_str());
		context().logger()->set_pattern(std::string(format));
		
		if (m_module_name.empty()) {
			m_module_name = m_flowvr_module->getID().substr(m_flowvr_module->getID().find_last_of("/") + 1);
		}
		context().logger()->debug("(FlowVR) Module ({}): Logger updated", m_module_name);
	}
	
public:
	Module(PDI::Context& ctx, PC_tree_t config):
		Component{ctx},
		m_fisrt_wait{true},
		m_silent_abort{false},
		m_abort_on_finalze{false}
	{
		load_desc_names(config);
		load_input_ports(config);
		load_output_ports(config);
		load_traces(config);
		load_module_name(config);
		initialize_flowvr_module();
		update_logger();
		context().logger()->info("(FlowVR) Module ({}): Module initialized", m_module_name);
	}
	
	Module(const Module& other) = delete;
	
	Module(Module&& other):
		Component(std::move(other)),
		m_fisrt_wait{other.m_fisrt_wait},
		m_flowvr_module{std::move(other.m_flowvr_module)},
		m_abort_on_finalze{other.m_abort_on_finalze},
		m_input_ports{std::move(other.m_input_ports)},
		m_output_ports{std::move(other.m_output_ports)},
		m_traces{std::move(other.m_traces)}
	{}
	
	Module& operator = (const Module& other) = delete;
	
	Module& operator = (Module&& other)
	{
		Component::operator = (std::move(other));
		m_fisrt_wait = other.m_fisrt_wait;
		m_flowvr_module = std::move(other.m_flowvr_module);
		m_abort_on_finalze = other.m_abort_on_finalze;
		m_input_ports = std::move(other.m_input_ports);
		m_output_ports = std::move(other.m_output_ports);
		m_traces = std::move(other.m_traces);
		return *this;
	}
	
	operator bool()
	{
		if (m_flowvr_module && m_flowvr_module->getStatus()) {
			return true;
		}
		return false;
	}
	/*
	 * End of "FlowVR initialization"
	 **************************************************************************/
	
	/**************************************************************************
	 * Handle events: abort and wait
	 */
private:
	/**
	 *  If not first wait, send messages from all ports. Call flowvr::wait(), receive messages from all ports.
	 *
	 *  \param[in] wait_ref reference where to save wait status
	 */
	int wait()
	{
		if (!m_flowvr_module) return 0;
		if (!m_fisrt_wait) {
			//put all messages
			for (auto& port : m_output_ports) {
				if (m_flowvr_module->getStatus()) { //} && port.get_flowvr_port()->isConnected()) {
					port.put_message();
				}
			}
		}
		
		context().logger()->debug("(FlowVR) Module ({}): Calling flowvr_module->wait()", m_module_name);
		int wait_status = m_flowvr_module->wait();
		if (!wait_status) {
			context().logger()->warn("(FlowVR) Module ({}): `wait' returned status: {}", m_module_name, wait_status);
		} else {
			context().logger()->debug("(FlowVR) Module ({}): `wait' returned status: {}", m_module_name, wait_status);
		}
		
		//get all messages
		if (wait_status) {
			for (auto& port : m_input_ports) {
				if (m_flowvr_module->getStatus() && port.get_flowvr_port()->isConnected()) {
					port.get_message();
				}
			}
		}
		m_fisrt_wait = false;
		return wait_status;
	}
	
	/**
	 *  Aborts flowvr
	 *
	 *  \param[in] abort_event name of the abort event
	 */
	void abort(const std::string& abort_event)
	{
		if (!m_flowvr_module) return;
		context().logger()->warn("(FlowVR) Module ({}): Got `{}' abort event. Aborting...", m_module_name, abort_event);
		m_flowvr_module->abort();
	}
	
	/*
	 * End of "Handle flowvr abort and wait"
	 **************************************************************************/
	
	/**************************************************************************
	 * Handle all exposes to flowvr: wait, status, parallel ranks
	 */
private:
	/**
	 *  Saves status to Ref
	 *
	 *  \param[in] status_ref reference where to save status
	 */
	void status(PDI::Ref_w status_ref)
	{
		if (!m_flowvr_module) {
			if (status_ref) *static_cast<int*>(status_ref.get()) = 0;
			return;
		}
		if (status_ref) {
			*static_cast<int*>(status_ref.get()) = m_flowvr_module->getStatus();
		} else {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to get write permissions for `status'", m_module_name};
		}
	}
	
	/**
	 *  If not first wait, send messages from all ports. Call flowvr::wait(), receive messages from all ports.
	 *
	 *  \param[in] wait_ref reference where to save wait status
	 */
	void wait(const std::string& data_name, PDI::Ref_w wait_ref)
	{
		if (!wait_ref) {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to get write permissions for `{}'", m_module_name, data_name};
		}
		*static_cast<int*>(wait_ref.get()) = wait();
	}
	
	/**
	 *  Saves parallel rank to Ref
	 *
	 *  \param[in] data_name name of the descriptor
	 *  \param[in] rank_ref_w reference where to write parallel rank
	 */
	void get_parallel_rank(const std::string& data_name, PDI::Ref_w rank_ref_w)
	{
		if (!m_flowvr_module) return;
		if (flowvr::Parallel::isParallel() == false) {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to write parallel rank to `{}', because flowVR is not set to parallel", m_module_name, data_name};
		}
		if (rank_ref_w) {
			*static_cast<int*>(rank_ref_w.get()) = flowvr::Parallel::getRank();
		} else {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to get write permissions for `{}'", m_module_name, data_name};
		}
	}
	
	/**
	 *  Saves parallel size to Ref
	 *
	 *  \param[in] data_name name of the descriptor
	 *  \param[in] size_ref_w reference where to write parallel size
	 */
	void get_parallel_size(const std::string& data_name, PDI::Ref_w size_ref_w)
	{
		if (!m_flowvr_module) return;
		if (flowvr::Parallel::isParallel() == false) {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to write parallel rank, because flowVR is not set to parallel", m_module_name};
		}
		if (size_ref_w) {
			*static_cast<int*>(size_ref_w.get()) = flowvr::Parallel::getNbProc();
		} else {
			throw PDI::Right_error{"(FlowVR) Module ({}): Unable to get write permissions for `{}'", m_module_name, data_name};
		}
	}
	
	/*
	 * End of "Handle all exposes to flowvr: wait, status, parallel ranks"
	 **************************************************************************/
	
public:

	~Module()
	{
		if (m_abort_on_finalze && m_flowvr_module) {
			context().logger()->debug("(FlowVR) Module ({}): Abort on finalize", m_module_name);
			m_flowvr_module->abort();
		}
		context().logger()->debug("(FlowVR) Module ({}): Close flowvr module ", m_module_name);
		//must be before flowvr (correct destroy order)
		m_flowvr_module.reset();
		context().logger()->debug("(FlowVR) Module ({}): Clear traces", m_module_name);
		m_traces.clear();
		context().logger()->debug("(FlowVR) Module ({}): Clear plugins intput ports", m_module_name);
		m_input_ports.clear();
		context().logger()->debug("(FlowVR) Module ({}): Clear plugins output ports", m_module_name);
		m_output_ports.clear();
		context().logger()->info("(FlowVR) Module ({}): Module finalized", m_module_name);
	}
	
}; // class Module

} // namespace <anonymous>

#endif // PDI_FLOWVR_MODULE
