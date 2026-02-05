/*******************************************************************************
 * Copyright (C) 2020-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <netcdf.h>
#include <netcdf_meta.h> // includes NC_HAS_PARALLEL4 define
#if NC_HAS_PARALLEL4
#include <mpi.h>
#include <netcdf_par.h>
#endif
#include <iostream>
#include <sstream>
#include <string>
#include <vector>

#include <spdlog/spdlog.h>

#include <pdi/array_datatype.h>
#include <pdi/scalar_datatype.h>

#include "dnc_netcdf_file.h"

namespace decl_netcdf {

namespace { // tools

std::pair<std::string, std::string> split_group_and_variable(const std::string variable_path)
{
	// get group path and variable name
	std::string group_path;
	std::string variable_name;
	size_t pos = variable_path.find_last_of("/");
	if (pos != std::string::npos) {
		// path has groups
		group_path = variable_path.substr(0, pos);
		variable_name = variable_path.substr(pos + 1);
	} else {
		// variable is in root
		group_path = "/";
		variable_name = variable_path;
	}

	// ensure that group begins with /
	if (group_path.empty() || group_path[0] != '/') {
		group_path.insert(group_path.begin(), '/');
	}

	return {group_path, variable_name};
}

std::vector<std::string> split_to_groups(const std::string& path)
{
	std::vector<std::string> result;
	std::string group;
	std::istringstream path_stream(path);
	while (std::getline(path_stream, group, '/')) {
		if (!group.empty()) {
			result.emplace_back(group);
		}
	}
	return result;
}

/** Converts PDI data type to hyperslab vectors (start, count)
 *
 * \param[in] type PDI data type to convert
 * \param[out] vector with stride
 *
 * \return scalar type of the type
 */
const PDI::Datatype_sptr get_variable_stride(PDI::Datatype_sptr type, std::vector<size_t>& stride)
{
	while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
		stride.emplace_back(array_type->size());
		type = array_type->subtype();
	}
	return type;
}

/** Wraps the calling of netcdf call with status checking
 *
 * \param status status returned from netcdf call
 * \param message a context message to use in case of error
 */
template <typename... Args>
void nc_try(int status, const char* message_if_error, const Args&... args)
{
	if (status != NC_NOERR) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: {} : ({}) {}", fmt::format(message_if_error, args...), status, nc_strerror(status)};
	}
}

nc_type nc_scalar_type(const PDI::Scalar_datatype& scalar_type)
{
	switch (scalar_type.kind()) {
	case PDI::Scalar_kind::SIGNED:
		switch (scalar_type.buffersize()) {
		case 1L:
			return NC_CHAR;
		case 2L:
			return NC_SHORT;
		case 4L:
			return NC_INT;
		case 8L:
			return NC_INT64;
		default:
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Unsupported signed scalar datatype"};
		}
	case PDI::Scalar_kind::UNSIGNED:
		switch (scalar_type.buffersize()) {
		case 1L:
			return NC_CHAR;
		default:
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Unsupported unsigned scalar datatype"};
		}
	case PDI::Scalar_kind::FLOAT:
		switch (scalar_type.buffersize()) {
		case 4L:
			return NC_FLOAT;
		case 8L:
			return NC_DOUBLE;
		default:
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Unsupported float scalar datatype"};
		}
		break;
	default:
		throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Unsupported unknown scalar datatype: {}", scalar_type.debug_string()};
	}
}

} // namespace

Dnc_netcdf_file::Dnc_netcdf_file(PDI::Context& ctx, const std::string& filename, int rights_flag, PDI::Expression mpi_comm_expr)
	: m_ctx{ctx}
	, m_filename{filename}
	, m_communicator{std::move(mpi_comm_expr)}
{
	if (m_communicator) {
#if NC_HAS_PARALLEL4
		// open/create in parallel
		const MPI_Comm* communicator = static_cast<const MPI_Comm*>(PDI::Ref_r{m_communicator.to_ref(m_ctx)}.get());
		MPI_Info mpi_info = MPI_INFO_NULL;
		m_ctx.logger().debug("Openning `{}' file in parallel mode", m_filename);
		if (nc_open_par(m_filename.c_str(), rights_flag | NC_NETCDF4, *communicator, mpi_info, &m_file_id) != NC_NOERR) {
			m_ctx.logger().trace("Cannot open file, creating", m_filename);
			nc_try(
				nc_create_par(m_filename.c_str(), rights_flag | NC_NETCDF4, *communicator, mpi_info, &m_file_id),
				"Cannot open or create file: {}",
				m_filename
			);
		} else {
			if (rights_flag == NC_WRITE) {
				nc_try(nc_redef(m_file_id), "File opened to write, but cannot get define mode");
			}
		}
#else
		throw PDI::Error{PDI_ERR_SYSTEM, "Decl_netcdf plugin: MPI communicator defined, but NetCDF is not parallel"};
#endif
	} else {
		m_ctx.logger().debug("Openning `{}' file in serial mode", m_filename);
		if (nc_open(m_filename.c_str(), rights_flag | NC_NETCDF4, &m_file_id) != NC_NOERR) {
			m_ctx.logger().trace("Cannot open `{}' file, creating", m_filename);
			nc_try(nc_create(m_filename.c_str(), rights_flag | NC_NETCDF4 | NC_NOCLOBBER, &m_file_id), "Cannot open or create file: {}", m_filename);
		} else {
			if (rights_flag == NC_WRITE) {
				nc_try(nc_redef(m_file_id), "File opened to write, but cannot get define mode");
			}
		}
	}

	m_ctx.logger().trace("File opened. (nc_id = {})", m_file_id);

	// add file id as a root group
	m_groups.emplace("/", m_file_id);
}

Dnc_netcdf_file::Dnc_netcdf_file(Dnc_netcdf_file&& other) noexcept
	: m_ctx{other.m_ctx}
	, m_filename{std::move(other.m_filename)}
	, m_file_id{std::move(other.m_file_id)}
	, m_groups{std::move(other.m_groups)}
	, m_variables{std::move(other.m_variables)}
{}

void Dnc_netcdf_file::read_group(const Dnc_group& group)
{
	int dest_id = m_file_id;
	std::string dest_path;
	std::vector<std::string> groups_names = split_to_groups(group.path());
	int group_id = m_file_id;
	for (auto&& group_name: groups_names) {
		nc_try(nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id), "Cannot read {} group from (nc_id = {})", group_name, dest_id);

		m_ctx.logger().trace("Read `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);

		dest_id = group_id;
		dest_path += "/" + group_name;
		m_groups.emplace(dest_path, dest_id);
	}

	for (auto&& attribute: group.attributes()) {
		this->get_attribute(group_id, NC_GLOBAL, attribute);
	}
}

void Dnc_netcdf_file::define_group(const Dnc_group& group)
{
	nc_id dest_id = m_file_id;
	std::string dest_path;
	std::vector<std::string> groups_names = split_to_groups(group.path());
	int group_id = m_file_id;
	for (auto&& group_name: groups_names) {
		if (nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id) != NC_NOERR) {
			nc_try(nc_def_grp(dest_id, group_name.c_str(), &group_id), "Cannot define group {} in nc_id = {}", group_name, dest_id);
			m_ctx.logger().trace("Defined `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);
		}

		dest_id = group_id;
		dest_path += "/" + group_name;
		m_groups.emplace(dest_path, dest_id);
	}

	for (auto&& attribute: group.attributes()) {
		this->put_attribute(group_id, NC_GLOBAL, attribute);
	}
}

void Dnc_netcdf_file::read_variable(const Dnc_variable& variable)
{
	auto it = m_variables.find(variable.path());
	if (it == m_variables.end()) {
		// get group path and variable name
		auto [group_path, variable_name] = split_group_and_variable(variable.path());

		// get src_id
		auto group_it = m_groups.find(group_path);
		if (group_it == m_groups.end()) {
			// group not defined -> read it from file
			int dest_id = m_file_id;
			std::string dest_path;
			std::vector<std::string> groups_names = split_to_groups(group_path);
			int group_id = m_file_id;
			for (auto&& group_name: groups_names) {
				nc_try(nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id), "Cannot read {} group from (nc_id = {})", group_name, dest_id);

				m_ctx.logger().trace("Read `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);

				dest_id = group_id;
				dest_path += "/" + group_name;
				m_groups.emplace(dest_path, dest_id);
			}
			group_it = m_groups.find(group_path);
		}
		int src_id = group_it->second;

		int nc_var_id;
		nc_try(nc_inq_varid(src_id, variable_name.c_str(), &nc_var_id), "Cannot inquire variable {} from (nc_id = {})", variable_name, src_id);

		m_ctx.logger().trace("Inquired `{}' variable (nc_id = {}) from (nc_id = {})", variable.path(), nc_var_id, src_id);
		m_variables.emplace(variable.path(), nc_var_id);

		m_ctx.logger().trace("Getting attributes of `{}' variable", variable_name);
		for (auto&& attribute: variable.attributes()) {
			get_attribute(src_id, nc_var_id, attribute);
		}
	}
}

namespace {

int get_dimension_id(PDI::Context& ctx, int nc_dest_id, const std::string& dim_name, int type_dim)
{
	int dim_id;
	if (nc_inq_dimid(nc_dest_id, dim_name.c_str(), &dim_id) == NC_NOERR) {
		ctx.logger().debug("`{}' dimension is already defined", dim_name);
		size_t dim_len;
		nc_try(nc_inq_dimlen(nc_dest_id, dim_id, &dim_len), "Cannot inquire dimension length");
	} else {
		ctx.logger().debug("Defining `{}' dimension", dim_name);
		nc_try(nc_def_dim(nc_dest_id, dim_name.c_str(), type_dim, &dim_id), "Cannot define {} dimension", dim_name);
	}
	return dim_id;
}

} // namespace

nc_type Dnc_netcdf_file::define_compound_type(std::shared_ptr<const PDI::Record_datatype> record_type)
{
	nc_type type_id;
	std::string compound_type_name;
	try {
		compound_type_name = record_type->attribute("decl_netcdf.type").to_string(m_ctx);
	} catch (const std::exception& e) {
		throw PDI::Value_error{"Cannot get `decl_netcdf.type' attribute from: {}", record_type->debug_string()};
	}

	int status = nc_inq_typeid(m_file_id, compound_type_name.c_str(), &type_id);
	if (status == NC_NOERR) {
		m_ctx.logger().trace("{} type already defined: (nc_type = {})", compound_type_name, type_id);
		return type_id;
	}


	/* support types in non-root groups? - netCDF 4.7.4 does not support it correctly
	    auto [group_path, compound_type_name] = split_group_and_variable(compound_type_path);
	    define_group(Dnc_group{m_ctx, group_path});
	    // get dest_id
	    auto group_it = m_groups.find(group_path);
	    if (group_it == m_groups.end()) {
	    throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find group that should be created: {}", group_path};
	    }
	    nc_id dest_id = group_it->second;
	*/

	// HAVE TO DEFINE SUB COMPOUND TYPE BEFOER CALLING nc_def_compound
	for (auto&& member: record_type->members()) {
		PDI::Datatype_sptr type = member.type();
		while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
			type = array_type->subtype();
		}
		if (auto&& member_record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
			m_ctx.logger().debug("From {}: defining compound member: {}", compound_type_name, member.name());
			define_compound_type(member_record_type);
		}
	}

	m_ctx.logger().debug("Defining new compound type: {} ({} B)", compound_type_name, record_type->buffersize());
	nc_try(nc_def_compound(m_file_id, record_type->buffersize(), compound_type_name.c_str(), &type_id), "Cannot define record type");

	for (auto&& member: record_type->members()) {
		if (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(member.type())) {
			// member is an array
			m_ctx.logger().trace("Inserting array member: {}, disp: {}", member.name(), member.displacement());
			std::vector<int> sizes;
			PDI::Datatype_sptr type = member.type();
			while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
				sizes.emplace_back(array_type->size());
				type = array_type->subtype();
			}
			nc_type member_type_id;
			if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type)) {
				member_type_id = nc_scalar_type(*scalar_type);
			} else if (auto&& member_record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
				member_type_id = define_compound_type(member_record_type);
			} else {
				throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Not supported datatype: {}", type->debug_string()};
			}
			nc_insert_array_compound(m_file_id, type_id, member.name().c_str(), member.displacement(), member_type_id, sizes.size(), sizes.data());
		} else if (auto&& member_record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(member.type())) {
			// member is a record, HAVE TO INSERT ALL MEMBERS THE SAME ORDER AS IT IS IN members VECTOR
			m_ctx.logger().trace("Inserting record member: {}, disp: {}", member.name(), member.displacement());
			nc_insert_compound(m_file_id, type_id, member.name().c_str(), member.displacement(), define_compound_type(member_record_type));
		} else if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(member.type())) {
			// member is a scalar
			m_ctx.logger().trace("Inserting scalar member: {}, disp: {}", member.name(), member.displacement());
			nc_insert_compound(m_file_id, type_id, member.name().c_str(), member.displacement(), nc_scalar_type(*scalar_type));
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Not supported datatype: {}", member.type()->debug_string()};
		}
	}
	m_ctx.logger().trace("Complete defining new compound type: {} (nc_type = {}): ", compound_type_name, type_id, record_type->debug_string());
	return type_id;
}

void Dnc_netcdf_file::define_variable(const Dnc_variable& variable)
{
	if (m_variables.find(variable.path()) != m_variables.end()) {
		// variable already defined
		m_ctx.logger().trace("Variable `{}' already defined", variable.path());
		return;
	}

	// get group path and variable name
	auto [group_path, variable_name] = split_group_and_variable(variable.path());
	m_ctx.logger().trace("Variable path `{}' splitted to `{}' group and `{}` variable name", variable.path(), group_path, variable_name);

	// get dest_id
	auto group_it = m_groups.find(group_path);
	if (group_it == m_groups.end()) {
		// group not defined -> define empty
		m_ctx.logger().trace("Group `{}' not defined. Defining...", group_path);
		Dnc_group group{m_ctx, group_path, PC_tree_t{}};
		this->define_group(group);
		group_it = m_groups.find(group_path);
	}
	int dest_id = group_it->second;

	nc_id var_id;
	if (nc_inq_varid(dest_id, variable_name.c_str(), &var_id) == NC_NOERR) {
		// variable already defined
		m_ctx.logger().debug("Variable `{}' already defined in (nc_id = {})", variable.path(), dest_id);
	} else {
		// get variable type
		PDI::Datatype_sptr variable_type = variable.type();
		if (!variable_type) {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Variable {}: No type defined", variable.path()};
		}
		if (!variable_type->dense()) {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Variable {}: Data must be dense (continuous memory)", variable.path()};
		}
		m_ctx.logger().trace("Preparing variable `{}'", variable_name);

		PDI::Datatype_sptr type = variable_type;
		std::vector<size_t> sizes;
		while (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(type)) {
			sizes.emplace_back(array_type->size());
			type = array_type->subtype();
		}
		nc_type type_id;
		if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(type)) {
			type_id = nc_scalar_type(*scalar_type);
		} else if (auto&& record_type = std::dynamic_pointer_cast<const PDI::Record_datatype>(type)) {
			type_id = define_compound_type(record_type);
		} else {
			throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Not supported datatype: {}", type->debug_string()};
		}

		// get variable dimensions
		m_ctx.logger().trace("Defining dimensions of {} variable:", variable.path());
		std::vector<nc_id> dimensions_ids;
		std::vector<std::string> dimensions_names = variable.dimensions_names();
		if (dimensions_names.empty()) {
			m_ctx.logger().trace("\t (arbitrary names)");
			// define arbitrary dimensions names
			for (int i = 0; i < sizes.size(); i++) {
				std::string dim_name = variable_name + "_" + std::to_string(i);
				m_ctx.logger().trace("\t {}[{}]", dim_name, sizes[i]);
				dimensions_ids.emplace_back(get_dimension_id(m_ctx, dest_id, dim_name, sizes[i]));
			}
		} else {
			if (sizes.size() != dimensions_names.size()) {
				throw PDI::Error{
					PDI_ERR_VALUE,
					"Decl_netcdf plugin: Variable type dimension ({}) != defined variable dimensions ({})",
					sizes.size(),
					dimensions_names.size()
				};
			}

			for (int i = 0; i < dimensions_names.size(); i++) {
				std::string dim_name = dimensions_names[i];
				m_ctx.logger().trace("\t {}[{}]", dim_name, sizes[i]);
				dimensions_ids.emplace_back(get_dimension_id(m_ctx, dest_id, dim_name, sizes[i]));
			}
		}

		m_ctx.logger().trace("Defining variable `{}' in (nc_id = {}) of type (nc_id = {})", variable.path(), dest_id, type_id);
		nc_try(
			nc_def_var(dest_id, variable_name.c_str(), type_id, dimensions_ids.size(), dimensions_ids.data(), &var_id),
			"Cannot define `{}' variable in (nc_id = {})",
			variable_name,
			dest_id
		);
		m_ctx.logger().trace("Variable `{}' defined (var_id = {})", variable.path(), var_id);
	}

	m_variables.emplace(variable.path(), var_id);

#if NC_HAS_PARALLEL4
	if (m_communicator) {
		nc_try(nc_var_par_access(dest_id, var_id, NC_COLLECTIVE), "Cannot change the access of `{}' variable to parallel", variable_name);
	}
#endif

	// set the attributes
	m_ctx.logger().trace("Putting attributes ({}), to `{}' variable", variable.attributes().size(), variable_name);
	for (auto&& attribute: variable.attributes()) {
		m_ctx.logger().trace("Putting attribute {} to `{}' variable", attribute.name(), variable_name);
		this->put_attribute(dest_id, var_id, attribute);
	}
}

void Dnc_netcdf_file::get_attribute(nc_id src_id, nc_id var_id, const Dnc_attribute& attribute)
{
	if (PDI::Ref_w ref_w = attribute.value()) {
		m_ctx.logger().trace("Getting `{}' attribute from (nc_id = {}/{})", attribute.name(), src_id, var_id);
		if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(ref_w.type())) {
			// get scalar attribute
			nc_try(
				nc_get_att(src_id, var_id, attribute.name().c_str(), ref_w.get()),
				"Cannot get attribute  `{}' from (nc_id = {}/{})",
				attribute.name(),
				src_id,
				var_id
			);
		} else if (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(ref_w.type())) {
			// get array attribute
			if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(array_type->subtype())) {
				if (!array_type->dense()) {
					throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Attribute type must be dense (continuous memory)"};
				}
				if (nc_get_att(src_id, var_id, attribute.name().c_str(), ref_w.get())) {
					throw PDI::Error{
						PDI_ERR_SYSTEM,
						"Decl_netcdf plugin: Cannot get attribute `{}' from (nc_id = {}/{})",
						attribute.name(),
						src_id,
						var_id
					};
				}
			} else {
				throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Multi dimensional array for attribute not supported"};
			}
		} else {
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Record datatype for attribute not supported"};
		}
	} else {
		m_ctx.logger().debug("Decl_netcdf plugin: No write access to read data from file attribute: {}", attribute.name());
	}
}

void Dnc_netcdf_file::put_attribute(nc_id dest_id, nc_id var_id, const Dnc_attribute& attribute)
{
	m_ctx.logger().trace("Putting `{}' attribute to (nc_id = {}/{})", attribute.name(), dest_id, var_id);
	nc_del_att(dest_id, var_id, attribute.name().c_str()); // try to delete old attribute, if fails nothing happens
	if (PDI::Ref_r ref_r = attribute.value()) {
		if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(ref_r.type())) {
			// set scalar attribute
			nc_try(
				nc_put_att(dest_id, var_id, attribute.name().c_str(), nc_scalar_type(*scalar_type), 1, ref_r.get()),
				"Cannot put attribute  `{}' to (nc_id = {}/{})",
				attribute.name(),
				dest_id,
				var_id
			);
		} else if (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(ref_r.type())) {
			// set array attribute
			if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(array_type->subtype())) {
				if (!array_type->dense()) {
					throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Attribute type must be dense (continuous memory)"};
				} else {
					nc_try(
						nc_put_att(dest_id, var_id, attribute.name().c_str(), nc_scalar_type(*scalar_type), array_type->size(), ref_r.get()),
						"Cannot put attribute  `{}' to (nc_id = {}/{})",
						attribute.name(),
						dest_id,
						var_id
					);
				}
			} else {
				throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Multi dimensional array for attribute not supported"};
			}
		} else {
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Record datatype for attribute not supported"};
		}
	} else {
		m_ctx.logger().debug("Decl_netcdf plugin: No read access to write data to attribute: {}", attribute.name());
	}
}

void Dnc_netcdf_file::enddef() const
{
	nc_try(nc_enddef(m_file_id), "Cannot end define mode");
	m_ctx.logger().debug("Define mode end in file {} (nc_id = {})", m_filename, m_file_id);
}

void Dnc_netcdf_file::put_variable(const Dnc_variable& variable, const Dnc_io& write, PDI::Ref_r ref_r)
{
	// check access
	if (!ref_r) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Cannot write `{}'. Need read access to write it to file", variable.path()};
	}

	// get group path and variable name
	auto [group_path, variable_name] = split_group_and_variable(variable.path());
	// get dest_id
	auto group_it = m_groups.find(group_path);
	if (group_it == m_groups.end()) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find group that should be created: {}", group_path};
	}
	nc_id dest_id = group_it->second;

	// get var_id
	auto var_it = m_variables.find(variable.path());
	if (var_it == m_variables.end()) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find variable that should be created: {}", variable.path()};
	}
	nc_id var_id = var_it->second;

	// get variable stride to calculate start and count
	std::vector<size_t> var_stride;
	get_variable_stride(variable.type(), var_stride);
	std::vector<size_t> var_start = write.get_dims_start(var_stride);
	std::vector<size_t> var_count = write.get_dims_count(var_stride);

	// write variable
	m_ctx.logger().trace("Putting variable `{}' (var_id = {})", variable_name, var_id);

	if (var_stride.empty()) {
		nc_try(nc_put_var(dest_id, var_id, ref_r.get()), "Decl_netcdf plugin: Cannot write `{}' to (nc_id = {})", dest_id);
	} else {
		nc_try(
			nc_put_vara(dest_id, var_id, var_start.data(), var_count.data(), ref_r.get()),
			"Decl_netcdf plugin: Cannot write `{}' to (nc_id = {})",
			dest_id
		);
	}
	m_ctx.logger().trace("Variable `{}' written", variable_name);
}

void Dnc_netcdf_file::get_variable(const Dnc_variable& variable, const Dnc_io& read, PDI::Ref_w ref_w)
{
	if (!ref_w) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Cannot read `{}'. Need write access to read it from file", variable.path()};
	}

	// get variable stride to calculate start and count
	std::vector<size_t> var_stride;
	get_variable_stride(variable.type(), var_stride);
	std::vector<size_t> var_start = read.get_dims_start(var_stride);
	std::vector<size_t> var_count = read.get_dims_count(var_stride);

	// get group path and variable name
	auto [group_path, variable_name] = split_group_and_variable(variable.path());

	// get src_id
	auto group_it = m_groups.find(group_path);
	if (group_it == m_groups.end()) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find group that should be created: {}", group_path};
	}
	nc_id src_id = group_it->second;

	// get var_id
	auto var_it = m_variables.find(variable.path());
	if (var_it == m_variables.end()) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find variable that should be created: {}", variable.path()};
	}
	nc_id var_id = var_it->second;

	// read variable
	nc_type var_nc_type;
	nc_inq_vartype(src_id, var_id, &var_nc_type);
	if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(ref_w.type())) {
		auto buffersize = scalar_type->buffersize();
		if (scalar_type->kind() == PDI::Scalar_kind::SIGNED) {
			switch (var_nc_type) {
			case NC_BYTE:
				if (buffersize != 1) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_BYTE for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			case NC_SHORT:
				if (buffersize != 2) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_SHORT for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			case NC_INT:
				if (buffersize != 4) {
					// throw PDI::Error{
					// 	PDI_ERR_CONFIG,
					// 	"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_INT32 for a buffer of size {}",
					// 	variable_name,
					// 	buffersize
					// };
					// TO DO: what shall we put here? an error or just a warning?
				}
				break;
			case NC_INT64:
				if (buffersize != 8) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_64 for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			}
		} else if (scalar_type->kind() == PDI::Scalar_kind::UNSIGNED) {
			switch (var_nc_type) {
			case NC_UBYTE:
				if (buffersize != 1) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_UBYTE for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			case NC_USHORT:
				if (buffersize != 2) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_USHORT for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			case NC_UINT:
				if (buffersize != 4) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_UINT(32) for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			case NC_UINT64:
				if (buffersize != 8) {
					throw PDI::Error{
						PDI_ERR_CONFIG,
						"Decl_netcdf plugin: Datatype mismatch: read '{}' of type NC_UINT64 for a buffer of size {}",
						variable_name,
						buffersize
					};
				}
				break;
			}
		}
	} else {
		m_ctx.logger().warn(
			"Inquired `{}' variable (nc_id = {}) from (nc_id = {}). Please be careful to match the buffer with the variable type",
			variable.path(),
			var_id,
			src_id
		);
		// TODO : check other data types
	}


	m_ctx.logger().trace("Getting variable `{}'", variable.path());
	if (var_stride.empty()) {
		nc_try(nc_get_var(src_id, var_id, ref_w.get()), "Cannot read `{}' from file", variable.path());
	} else {
		nc_try(nc_get_vara(src_id, var_id, var_start.data(), var_count.data(), ref_w.get()), "Cannot read `{}' from file", variable.path());
	}
}

void Dnc_netcdf_file::get_sizeof_variable(const std::string& variable, const std::string& sizeof_var, PDI::Ref ref)
{
	if (!PDI::Ref_w(ref)) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Cannot read `{}'. Need write access to read it from file", sizeof_var};
	}

	// get group path and variable name
	auto [group_path, variable_name] = split_group_and_variable(sizeof_var);

	// get src_id
	auto group_it = m_groups.find(group_path);
	if (group_it == m_groups.end()) {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Cannot find group that should be created: {}", group_path};
	}
	nc_id src_id = group_it->second;

	nc_id var_id;
	nc_try(nc_inq_varid(src_id, variable_name.c_str(), &var_id), "Cannot inquire variable {} from (nc_id = {})", variable_name, src_id);

	int var_dim;
	nc_try(nc_inq_varndims(src_id, var_id, &var_dim), "Cannot inquire variable dimension counts from (var_id= {}, nc_id = {})", sizeof_var, src_id);

	std::vector<int> dimid(var_dim);
	std::vector<size_t> dimlen(var_dim);

	nc_try(nc_inq_vardimid(src_id, var_id, &dimid[0]), "cannot get size of `{}", sizeof_var);

	if (auto&& scalar_type = std::dynamic_pointer_cast<const PDI::Scalar_datatype>(ref.type())) {
		if (var_dim != 1) {
			throw PDI::Error{
				PDI_ERR_VALUE,
				"Decl_netcdf plugin: Incoherent data size. Data {} defined with size=1 , but {} has size {}",
				variable,
				sizeof_var,
				var_dim
			};
		}
		nc_try(nc_inq_dimlen(src_id, dimid[0], &dimlen[0]), "Cannot inquire dimension length");

		PDI::Ref_w(ref).scalar_assign(dimlen[0]);
	}

	else if (auto&& array_type = std::dynamic_pointer_cast<const PDI::Array_datatype>(ref.type()))
	{
		if (var_dim != array_type->size()) {
			throw PDI::Error{
				PDI_ERR_VALUE,
				"Decl_netcdf plugin: Incoherent data size. Data {} defined with size {}, but provided with {}",
				variable,
				array_type->size(),
				var_dim
			};
		}
		for (int i = 0; i < var_dim; i++) {
			nc_try(nc_inq_dimlen(src_id, dimid[i], &dimlen[i]), "Cannot inquire dimension length");

			PDI::Ref_w(ref[i]).scalar_assign(dimlen[i]);
		}
	} else {
		throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Incompatible data type for {}. Expecting scalar or array", sizeof_var};
	}
}

Dnc_netcdf_file::~Dnc_netcdf_file()
{
	m_ctx.logger().debug("Closing file {} (nc_id = {})", m_filename, m_file_id);
	nc_try(nc_close(m_file_id), "Cannot close the file {}", m_filename);
}

} // namespace decl_netcdf
