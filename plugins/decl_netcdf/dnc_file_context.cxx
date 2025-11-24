/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2019-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <set>
#include <netcdf.h>

#include "dnc_netcdf_file.h"

#include "dnc_file_context.h"

namespace decl_netcdf {

Dnc_file_context::Dnc_file_context(PDI::Context& ctx, PC_tree_t config)
	: m_ctx{ctx}
{
	PC_tree_t file_node = PC_get(config, ".file");
	if (PC_status(file_node)) {
		throw PDI::Error{PDI_ERR_CONFIG, "Decl_netcdf plugin: `file' node is mandatory"};
	}
	m_file_path = PDI::Expression{PDI::to_string(file_node)};
	m_ctx.logger().trace("Creating file info");

	PC_tree_t comm_node = PC_get(config, ".communicator");
	if (!PC_status(comm_node)) {
		m_communicator = PDI::Expression{PDI::to_string(comm_node)};
		m_ctx.logger().trace("Communicator defined");
	}

	std::vector<std::string> events;
	PC_tree_t on_event_node = PC_get(config, ".on_event");
	if (!PC_status(on_event_node)) {
		if (PDI::is_list(on_event_node)) {
			int len = PDI::len(on_event_node);
			for (int i = 0; i < len; i++) {
				std::string event_name = PDI::to_string(PC_get(on_event_node, "[%d]", i));
				events.emplace_back(event_name);
				m_ctx.logger().trace("Adding to trigger list a new event: {}", event_name);
			}
		} else {
			std::string event_name = PDI::to_string(on_event_node);
			events.emplace_back(event_name);
			m_ctx.logger().trace("Adding to trigger list a new event: {}", event_name);
		}
	}

	PC_tree_t when_node = PC_get(config, ".when");
	if (!PC_status(when_node)) {
		m_when = PDI::Expression{PDI::to_string(when_node)};
		m_ctx.logger().trace("When defined");
	} else {
		m_when = PDI::Expression{1L}; // if when not defined -> always true
	}

	PC_tree_t groups_node = PC_get(config, ".groups");
	if (!PC_status(groups_node)) {
		PDI::each(groups_node, [this](PC_tree_t group_path_node, PC_tree_t group_value) {
			std::string group_path = PDI::to_string(group_path_node);
			this->m_ctx.logger().trace("Creating new group info: {}", group_path);
			this->m_groups.emplace(group_path, Dnc_group{this->m_ctx, group_path, group_value});
		});
	} else {
		m_ctx.logger().debug("No group defined");
	}

	PC_tree_t variables_node = PC_get(config, ".variables");
	if (!PC_status(variables_node)) {
		PDI::each(variables_node, [this](PC_tree_t variable_path_node, PC_tree_t variable_value) {
			std::string variable_path = PDI::to_string(variable_path_node);
			this->m_ctx.logger().trace("Creating new variable info: {}", variable_path);
			this->m_variables.emplace(variable_path, Dnc_variable{this->m_ctx, variable_path, variable_value});
		});
	} else {
		m_ctx.logger().trace("No variable defined");
	}

	PC_tree_t read_node = PC_get(config, ".read");
	if (!PC_status(read_node)) {
		if (PDI::is_scalar(read_node)) {
			std::string read_desc = PDI::to_string(read_node);
			m_ctx.logger().trace("Creating new empty read info for: {}", read_desc);
			m_read.emplace(read_desc, Dnc_io{this->m_ctx, PC_tree_t{}});
		} else if (PDI::is_list(read_node)) {
			int len = PDI::len(read_node);
			for (int i = 0; i < len; i++) {
				std::string read_desc = PDI::to_string(PC_get(read_node, "[%d]", i));
				m_ctx.logger().trace("Creating new empty read info for: {}", read_desc);
				m_read.emplace(read_desc, Dnc_io{this->m_ctx, PC_tree_t{}});
			}
		} else {
			PDI::each(read_node, [this](PC_tree_t desc_name, PC_tree_t read_value) {
				std::string read_desc = PDI::to_string(desc_name);
				m_ctx.logger().trace("Creating new read info for: {}", read_desc);
				// if we read a variable with "size_of" key
				if (!PC_status(PC_get(read_value, ".size_of"))) {
					this->m_sizeof.emplace(read_desc, Dnc_io{this->m_ctx, read_value});
				}
				// if we read a regular variable
				else {
					this->m_read.emplace(read_desc, Dnc_io{this->m_ctx, read_value});
				}
			});
		}
	}

	PC_tree_t write_node = PC_get(config, ".write");
	if (!PC_status(write_node)) {
		if (PDI::is_scalar(write_node)) {
			std::string write_desc = PDI::to_string(write_node);
			m_ctx.logger().trace("Creating new empty write info for: {}", write_desc);
			m_write.emplace(write_desc, Dnc_io{this->m_ctx, PC_tree_t{}});
		} else if (PDI::is_list(write_node)) {
			int len = PDI::len(write_node);
			for (int i = 0; i < len; i++) {
				std::string write_desc = PDI::to_string(PC_get(write_node, "[%d]", i));
				m_ctx.logger().trace("Creating new empty write info for: {}", write_desc);
				m_write.emplace(write_desc, Dnc_io{this->m_ctx, PC_tree_t{}});
			}
		} else if (PDI::is_map(write_node)) {
			PDI::each(write_node, [this](PC_tree_t desc_name, PC_tree_t write_value) {
				std::string write_desc = PDI::to_string(desc_name);
				m_ctx.logger().trace("Creating new write info for: {}", write_desc);
				this->m_write.emplace(PDI::to_string(desc_name), Dnc_io{this->m_ctx, write_value});
			});
		} else {
			throw PDI::Error{PDI_ERR_CONFIG, "write node is not parsed correctly"};
		}
	}

	for (auto&& event: events) {
		m_ctx.callbacks().add_event_callback([this](const std::string&) { this->execute(); }, event);
	}

	if (events.empty()) {
		m_ctx.logger().debug("No on_event event defined, triggering on data share");
		std::set<std::string> desc_triggers;
		for (auto&& desc_io_pair: m_read) {
			desc_triggers.emplace(desc_io_pair.first);
		}
		for (auto&& desc_io_pair: m_write) {
			desc_triggers.emplace(desc_io_pair.first);
		}
		for (auto&& desc_io_pair: m_sizeof) {
			desc_triggers.emplace(desc_io_pair.first);
		}
		for (auto&& desc_trigger: desc_triggers) {
			m_ctx.callbacks().add_data_callback([this](const std::string& desc_name, PDI::Ref r) { this->execute(desc_name, r); }, desc_trigger);
		}
	}
}

Dnc_file_context::Dnc_file_context(Dnc_file_context&& other) noexcept
	: m_ctx{other.m_ctx}
	, m_file_path{std::move(other.m_file_path)}
	, m_communicator{std::move(other.m_communicator)}
	, m_when{std::move(other.m_when)}
	, m_groups{std::move(other.m_groups)}
	, m_variables{std::move(other.m_variables)}
	, m_read{std::move(other.m_read)}
	, m_write{std::move(other.m_write)}
{}

Dnc_variable* Dnc_file_context::variable(const std::string& desc_name, const std::string& variable_path, std::list<Dnc_variable>& variables_holder)
{
	auto it = m_variables.find(variable_path);
	if (it == m_variables.end()) {
		// try desc_name as variable
		it = m_variables.find(desc_name);
		if (it == m_variables.end()) {
			// variable not defined yet -> create it and add to variables_holder
			if (variable_path.empty()) {
				variables_holder.emplace_back(m_ctx, desc_name, PC_tree_t{});
			} else {
				variables_holder.emplace_back(m_ctx, variable_path, PC_tree_t{});
			}
			return &variables_holder.back();
		} else {
			return &it->second;
		}
	} else {
		return &it->second;
	}
}

void Dnc_file_context::execute(const std::string& desc_name, PDI::Ref ref)
{
	if (m_when.to_long(m_ctx)) {
		std::list<Dnc_variable> variables_holder; // memory for Variables created from descriptor

		auto write_it = m_write.find(desc_name);
		if (write_it != m_write.end() && write_it->second.when()) {
			Dnc_netcdf_file nc_file{m_ctx, m_file_path.to_string(m_ctx), NC_WRITE, m_communicator};

			// define all groups
			for (auto&& group: m_groups) {
				nc_file.define_group(group.second);
			}

			// get variable of shared descriptor
			Dnc_variable* variable = this->variable(write_it->first, write_it->second.variable_path(), variables_holder);

			// ensure that variable has a type
			if (!variable->type()) {
				// TODO: undo the type for defined variables
				variable->type(ref.type());
			}

			// define variable
			nc_file.define_variable(*variable);

			// end NetCDF definition mode
			nc_file.enddef();

			// execute write
			nc_file.put_variable(*variable, write_it->second, ref);
		}

		auto read_it = m_read.find(desc_name);
		if (read_it != m_read.end() && read_it->second.when()) {
			Dnc_netcdf_file nc_file{m_ctx, m_file_path.to_string(m_ctx), NC_NOWRITE, m_communicator};

			// read all groups
			for (auto&& group: m_groups) {
				nc_file.read_group(group.second);
			}

			// get variable of shared descriptor
			Dnc_variable* variable = this->variable(read_it->first, read_it->second.variable_path(), variables_holder);

			// ensure that variable has a type
			if (!variable->type()) {
				// TODO: undo the type for defined variables
				variable->type(ref.type());
			}

			// read variable (could be done in get_variable, but we want to be coherent with write)
			nc_file.read_variable(*variable);

			// execute read
			nc_file.get_variable(*variable, read_it->second, ref);
		}

		auto size_it = m_sizeof.find(desc_name);
		if (size_it != m_sizeof.end() && size_it->second.when()) {
			Dnc_netcdf_file nc_file{m_ctx, m_file_path.to_string(m_ctx), NC_NOWRITE, m_communicator};
			std::string dataset_name = size_it->second.variable_path();
			m_ctx.logger().trace("Getting size of `{}' dataset", dataset_name);
			nc_file.get_sizeof_variable(size_it->first, dataset_name, ref);
		}
	}
}

void Dnc_file_context::execute()
{
	if (m_when.to_long(m_ctx)) {
		std::list<Dnc_variable> variables_holder;
		std::vector<Dnc_variable*> variables_to_get;
		std::vector<Dnc_variable*> variables_to_put;

		std::unique_ptr<Dnc_netcdf_file> nc_file;
		if (m_write.empty()) {
			nc_file.reset(new Dnc_netcdf_file{m_ctx, m_file_path.to_string(m_ctx), NC_NOWRITE, m_communicator});

			// read all groups
			for (auto&& group: m_groups) {
				nc_file->read_group(group.second);
			}

			// read all variables
			for (auto&& read: m_read) {
				Dnc_variable* variable = this->variable(read.first, read.second.variable_path(), variables_holder);
				variables_to_get.emplace_back(variable);

				// ensure that variable has a type
				if (!variable->type()) {
					// TODO: undo the type for defined variables
					variable->type(m_ctx.desc(read.first).ref().type());
				}

				// read this variable
				nc_file->read_variable(*variable);
			}

			for (auto&& size_of: m_sizeof) {
				nc_file->get_sizeof_variable(size_of.first, size_of.second.variable_path(), m_ctx.desc(size_of.first).ref());
			}
		} else {
			nc_file.reset(new Dnc_netcdf_file{m_ctx, m_file_path.to_string(m_ctx), NC_WRITE, m_communicator});

			// define all groups
			for (auto&& group: m_groups) {
				nc_file->define_group(group.second);
			}

			// define all variables
			for (auto&& write: m_write) {
				Dnc_variable* variable = this->variable(write.first, write.second.variable_path(), variables_holder);
				variables_to_put.emplace_back(variable);

				// ensure that variable has a type
				if (!variable->type()) {
					// TODO: undo the type for defined variables
					variable->type(m_ctx.desc(write.first).ref().type());
				}

				// define this variable
				nc_file->define_variable(*variable);
			}

			// end NetCDF definition mode
			nc_file->enddef();
		}

		int i = 0;
		for (auto&& write: m_write) {
			Dnc_variable* variable = variables_to_put[i]; // order of loop iteration is the same as was on define loop
			m_ctx.logger().trace("{}: Putting desc `{}' to variable `{}'", i, write.first, variables_to_put[i]->path());
			nc_file->put_variable(*variable, write.second, m_ctx.desc(write.first).ref());
			i++;
		}

		i = 0;
		for (auto&& read: m_read) {
			Dnc_variable* variable = variables_to_get[i]; // order of loop iteration is the same as was on define loop
			nc_file->get_variable(*variable, read.second, m_ctx.desc(read.first).ref());
			i++;
		}
	}
}

} // namespace decl_netcdf
