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

#include <pdi/error.h>

#include "module.h"

namespace _flowvr_plugin {

void Module::load_desc_names(PC_tree_t config)
{
	PC_tree_t wait_on_data_node = PC_get(config, ".wait_on_data");
	if (!PC_status(wait_on_data_node)) {
		std::string wait_data = PDI::to_string(wait_on_data_node);
		context().logger()->debug("wait_on_data = {}", wait_data);
		context().callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
			this->wait(name, ref);
		}, wait_data);
	}
	
	PC_tree_t wait_on_node = PC_get(config, ".wait_on");
	if (!PC_status(wait_on_node)) {
		context().logger()->warn("Wait is called as an event. Use `wait_on_data` if possible to check wait status.");
		if (!PC_status(PC_get(wait_on_node, "[0]"))) {
			int nb_event = PDI::len(wait_on_node);
			for (int event_id = 0; event_id < nb_event; event_id++) {
				std::string wait_event = PDI::to_string(PC_get(wait_on_node, "[%d]", event_id));
				context().callbacks().add_event_callback([this](const std::string& name) {
					if (this->wait() == 0) {
						m_input_ports.clear();
						m_output_ports.clear();
						m_traces.clear();
						m_flowvr_module.reset();
						if (this->m_silent_abort == false) {
							throw PDI::System_error{"Received abort signal from FlowVR"};
						}
					}
				}, wait_event);
			}
		} else {
			std::string wait_event = PDI::to_string(wait_on_node);
			context().callbacks().add_event_callback([this](const std::string& name) {
				if (this->wait() == 0) {
					m_input_ports.clear();
					m_output_ports.clear();
					m_traces.clear();
					m_flowvr_module.reset();
					if (this->m_silent_abort == false) {
						throw PDI::System_error{"Received abort signal from FlowVR"};
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
				context().callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
					this->status(ref);
				}, status_data);
			}
		} else {
			std::string status_data = PDI::to_string(status_node);
			context().callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
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
				context().logger()->debug("Parallel: rank = {}, size = {}", set_rank, set_size);
			} else {
				throw PDI::Config_error{set_rank_node, "`set_rank' is defined, but `set_size' is not"};
			}
		} else {
			if (!PC_status(set_size_node)) {
				throw PDI::Config_error{set_size_node, "`set_size' is defined, but `set_rank' is not"};
			} else {
				flowvr::Parallel::init(true);
			}
		}
		
		PC_tree_t get_rank_node = PC_get(parallel_node, ".get_rank");
		if (!PC_status(get_rank_node)) {
			std::string get_parallel_rank_data = PDI::to_string(get_rank_node);
			context().logger()->debug("Parallel rank desc = {}", get_parallel_rank_data);
			context().callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
				this->get_parallel_rank(name, ref);
			}, get_parallel_rank_data);
		}
		
		PC_tree_t get_size_node = PC_get(parallel_node, ".get_size");
		if (!PC_status(get_size_node)) {
			std::string get_parallel_size_data = PDI::to_string(get_size_node);
			context().logger()->debug("Parallel size desc = {}", get_parallel_size_data);
			context().callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
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
				context().callbacks().add_event_callback([this](const std::string& name) {
					this->abort(name);
				}, abort_event);
			}
		} else {
			std::string abort_event = PDI::to_string(abort_node);
			context().callbacks().add_event_callback([this](const std::string& name) {
				this->abort(name);
			}, abort_event);
		}
	}
	
	PC_tree_t silent_abort_node = PC_get(config, ".silent_abort");
	if (!PC_status(silent_abort_node)) {
		m_silent_abort = PDI::to_bool(silent_abort_node);
		context().logger()->debug("Silent abort changed to: {}", m_silent_abort);
	}
	
	PC_tree_t abort_on_fin_node = PC_get(config, ".abort_on_finalize");
	if (!PC_status(abort_on_fin_node)) {
		m_abort_on_finalze = PDI::to_bool(abort_on_fin_node);
		context().logger()->debug("Abort on finalize changed to: {}", m_abort_on_finalze);
	}
	context().logger()->debug("Loaded basic configuration");
}

void Module::load_input_ports(PC_tree_t config)
{
	PC_tree_t input_node = PC_get(config, ".input_ports");
	if (!PC_status(input_node)) {
		int nb_ports = PDI::len(input_node, 0);
		for (int port_id = 0; port_id < nb_ports; port_id++) {
			std::string port_name = PDI::to_string(PC_get(input_node, "{%d}", port_id));
			PC_tree_t port_node = PC_get(input_node, "<%d>", port_id);
			m_input_ports.emplace_back(Input_port(context(), port_name, port_node));
		}
		context().logger()->info("Loaded {} input ports", nb_ports);
	} else {
		context().logger()->debug("No input ports to load");
	}
	
}

void Module::load_output_ports(PC_tree_t config)
{
	PC_tree_t output_node = PC_get(config, ".output_ports");
	if (!PC_status(output_node)) {
		int nb_ports = PDI::len(output_node, 0);
		for (int port_id = 0; port_id < nb_ports; port_id++) {
			std::string port_name = PDI::to_string(PC_get(output_node, "{%d}", port_id));
			PC_tree_t port_node = PC_get(output_node, "<%d>", port_id);
			m_output_ports.emplace_back(Output_port(context(), port_name, port_node));
		}
		context().logger()->info("Loaded {} output ports", nb_ports);
	} else {
		context().logger()->debug("No output ports to load");
	}
}

void Module::load_traces(PC_tree_t config)
{
	PC_tree_t traces_node = PC_get(config, ".traces");
	if (!PC_status(traces_node)) {
		int nb_traces = PDI::len(traces_node, 0);
		for (int trace_id = 0; trace_id < nb_traces; trace_id++) {
			std::string trace_name = PDI::to_string(PC_get(traces_node, "{%d}", trace_id));
			PC_tree_t trace_node = PC_get(traces_node, "<%d>", trace_id);
			m_traces.emplace_back(Trace(context(), trace_name, trace_node));
		}
		context().logger()->info("Loaded {} traces", nb_traces);
	} else {
		context().logger()->debug("No traces to load");
	}
	
}

void Module::load_module_name(PC_tree_t config)
{
	PC_tree_t name_node = PC_get(config, ".name");
	if (!PC_status(name_node)) {
		m_module_name = PDI::Expression{PDI::to_string(name_node)}.to_string(context());
		
		PC_tree_t instance_name_node = PC_get(config, ".instance_instance_name");
		if (!PC_status(instance_name_node)) {
			m_instance_name = PDI::Expression{PDI::to_string(instance_name_node)}.to_string(context());
			context().logger()->info("New module name set: {}/{}", m_module_name, m_instance_name);
		} else {
			context().logger()->info("New module name set: {}", m_module_name);
		}
	}
}

void Module::initialize_flowvr_module()
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
	
	if (!m_traces.empty()) {
		m_flowvr_module = Flowvr_module_uptr(flowvr::initModule(m_ports_addresses, m_traces_addresses, m_instance_name, m_module_name),
		[](flowvr::ModuleAPI* module) {
			module->close();
		});
	} else {
		m_flowvr_module = Flowvr_module_uptr(flowvr::initModule(m_ports_addresses, m_instance_name, m_module_name),
		[](flowvr::ModuleAPI* module) {
			module->close();
		});
	}
	
	context().logger()->debug("Initialized flowvr module");
}

void Module::update_logger(PC_tree_t logging_tree)
{
	char format[256];
	if (m_flowvr_module && m_module_name.empty()) {
		snprintf(format, 256, "FlowVR/%s", m_flowvr_module->getID().substr(m_flowvr_module->getID().find_last_of("/") + 1));
	} else if (!m_module_name.empty()) {
		snprintf(format, 256, "FlowVR/%s", m_module_name);
	} else {
		return;
	}
	context().logger()->default_pattern("[%T][" + std::string(format) + "] *** %^%l%$: %v");
	context().logger()->debug("Logger updated");
}

Module::Module(PDI::Context& ctx, PC_tree_t config):
	Component{ctx},
	m_fisrt_wait{true},
	m_silent_abort{false},
	m_abort_on_finalze{false}
{
	load_module_name(config);
	update_logger(PC_get(config, ".logging"));
	
	load_desc_names(config);
	load_input_ports(config);
	load_output_ports(config);
	load_traces(config);
	
	initialize_flowvr_module();
	
	// update logger again in case of flowvr default name
	update_logger(PC_get(config, ".logging"));
	context().logger()->info("Module initialization succeed");
}

Module::Module(Module&& other):
	Component(other),
	m_fisrt_wait{other.m_fisrt_wait},
	m_flowvr_module{std::move(other.m_flowvr_module)},
	m_abort_on_finalze{other.m_abort_on_finalze},
	m_input_ports{std::move(other.m_input_ports)},
	m_output_ports{std::move(other.m_output_ports)},
	m_traces{std::move(other.m_traces)}
{}

Module& Module::operator=(Module&& other)
{
	Component::operator=(other);
	m_fisrt_wait = other.m_fisrt_wait;
	m_flowvr_module = std::move(other.m_flowvr_module);
	m_abort_on_finalze = other.m_abort_on_finalze;
	m_input_ports = std::move(other.m_input_ports);
	m_output_ports = std::move(other.m_output_ports);
	m_traces = std::move(other.m_traces);
	return *this;
}

int Module::wait()
{
	if (!m_fisrt_wait && m_flowvr_module->getStatus()) {
		//put all messages
		for (auto& port : m_output_ports) {
			if (port.isConnected()) {
				context().logger()->debug("Putting message to `{}' output port", port.name());
				port.put_message();
			} else {
				context().logger()->warn("Cannot put message to `{}' output port. Port not connected");
			}
		}
	}
	
	context().logger()->debug("Calling flowvr_module->wait()");
	int wait_status = m_flowvr_module->wait();
	context().logger()->debug("flowvr_module->wait() returned status: {}", wait_status);
	
	//get all messages
	if (wait_status) {
		for (auto& port : m_input_ports) {
			if (port.isConnected()) {
				context().logger()->debug("Getting message from `{}' input port", port.name());
				port.get_message();
			} else {
				context().logger()->warn("Cannot get message from `{}' input port. Port not connected");
			}
		}
	}
	m_fisrt_wait = false;
	return wait_status;
}

void Module::abort(const std::string& abort_event)
{
	if (!m_flowvr_module || !m_flowvr_module->getStatus()) {
		throw PDI::State_error{"Cannot call abort() on closed module"};
	}
	context().logger()->info("Got `{}' abort event. Aborting FlowVR application...", abort_event);
	m_flowvr_module->abort();
}

void Module::status(PDI::Ref_w status_ref)
{
	if (status_ref) {
		throw PDI::Right_error{"Unable to get write permissions to write module status"};
	}
	
	if (!m_flowvr_module) {
		*static_cast<int*>(status_ref.get()) = 0;
	} else {
		*static_cast<int*>(status_ref.get()) = m_flowvr_module->getStatus();
	}
	
}

void Module::wait(const std::string& data_name, PDI::Ref_w wait_ref)
{
	if (!wait_ref) {
		throw PDI::Right_error{"Unable to get write permissions to write status of wait() for `{}' data", data_name};
	}
	*static_cast<int*>(wait_ref.get()) = wait();
}

void Module::get_parallel_rank(const std::string& data_name, PDI::Ref_w rank_ref)
{
	if (!m_flowvr_module || !m_flowvr_module->getStatus()) {
		throw PDI::State_error{"Cannot call get_parallel_rank() on closed module"};
	}
	
	if (flowvr::Parallel::isParallel() == false) {
		throw PDI::Right_error{"Unable to write parallel rank to `{}', because FlowVR is not set to parallel", data_name};
	}
	
	if (rank_ref) {
		*static_cast<int*>(rank_ref.get()) = flowvr::Parallel::getRank();
	} else {
		throw PDI::Right_error{"Unable to get write permissions for `{}'", data_name};
	}
}

void Module::get_parallel_size(const std::string& data_name, PDI::Ref_w size_ref)
{
	if (!m_flowvr_module || !m_flowvr_module->getStatus()) {
		throw PDI::State_error{"Cannot call get_parallel_size() on closed module"};
	}
	
	if (flowvr::Parallel::isParallel() == false) {
		throw PDI::Right_error{"Unable to write parallel rank, because flowVR is not set to parallel"};
	}
	
	if (size_ref) {
		*static_cast<int*>(size_ref.get()) = flowvr::Parallel::getNbProc();
	} else {
		throw PDI::Right_error{"Unable to get write permissions for `{}'", data_name};
	}
}

Module::~Module()
{
	if (m_abort_on_finalze) {
		context().logger()->info("Closing module. Aborting on finalize...");
		m_flowvr_module->abort();
	} else {
		context().logger()->info("Closing module...");
	}
	//must be before flowvr (correct destroy order)
	m_flowvr_module.reset();
	context().logger()->debug("Closing traces");
	m_traces.clear();
	context().logger()->debug("Closing plugins intput ports");
	m_input_ports.clear();
	context().logger()->debug("Closing plugins output ports");
	m_output_ports.clear();
	context().logger()->info("Module closed successfully");
}

} // namespace _flowvr_plugin
