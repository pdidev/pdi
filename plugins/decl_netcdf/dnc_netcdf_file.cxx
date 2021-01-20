/*******************************************************************************
 * Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
	#include <netcdf_par.h>
	#include <mpi.h>
#endif
#include <spdlog/spdlog.h>
#include <sstream>
#include <string>
#include <vector>

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
const PDI::Scalar_datatype* get_variable_stride(const PDI::Datatype* type, std::vector<size_t>& stride)
{
	while (auto&& array_type = dynamic_cast<const PDI::Array_datatype*>(type)) {
		stride.emplace_back(array_type->size());
		type = &array_type->subtype();
	}
	
	if (auto&& scalar_type = dynamic_cast<const PDI::Scalar_datatype*>(type)) {
		return scalar_type;
	} else {
		throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Record datatype is not supported"};
	}
}

/** Wraps the calling of netcdf call with status checking
 *
 * \param status status returned from netcdf call
 * \param message a context message to use in case of error
 */
template<typename... Args>
void nc_try(int status, const char* message_if_error = NULL, const Args& ... args)
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
		throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Unsupported unknown scalar datatype"};
	}
}

} // namespace <anonymous> tools

Dnc_netcdf_file::Dnc_netcdf_file(PDI::Context& ctx, const std::string& filename, int rights_flag, PDI::Expression mpi_comm_expr):
	m_ctx{ctx},
	m_filename{filename},
	m_communicator{std::move(mpi_comm_expr)}
{
	if (m_communicator) {
#if NC_HAS_PARALLEL4
		// open/create in parallel
		const MPI_Comm* communicator = static_cast<const MPI_Comm*>(PDI::Ref_r{m_communicator.to_ref(m_ctx)}.get());
		MPI_Info mpi_info = MPI_INFO_NULL;
		m_ctx.logger()->debug("Openning `{}' file in parallel mode", m_filename);
		if (nc_open_par(m_filename.c_str(), rights_flag | NC_NETCDF4, *communicator, mpi_info, &m_file_id) != NC_NOERR) {
			m_ctx.logger()->trace("Cannot open file, creating", m_filename);
			nc_try(nc_create_par(m_filename.c_str(), rights_flag | NC_NETCDF4, *communicator, mpi_info, &m_file_id),
			    "Cannot open or create file: {}", m_filename);
		} else {
			if (rights_flag == NC_WRITE) {
				nc_try(nc_redef(m_file_id), "File opened to write, but cannot get define mode");
			}
		}
#else
		throw PDI::Error {PDI_ERR_SYSTEM, "Decl_netcdf plugin: MPI communicator defined, but NetCDF is not parallel"};
#endif
	} else {
		m_ctx.logger()->debug("Openning `{}' file in serial mode", m_filename);
		if (nc_open(m_filename.c_str(), rights_flag | NC_NETCDF4, &m_file_id) != NC_NOERR) {
			m_ctx.logger()->trace("Cannot open `{}' file, creating", m_filename);
			nc_try(nc_create(m_filename.c_str(), rights_flag | NC_NETCDF4 | NC_NOCLOBBER, &m_file_id),
			    "Cannot open or create file: {}", m_filename);
		} else {
			if (rights_flag == NC_WRITE) {
				nc_try(nc_redef(m_file_id), "File opened to write, but cannot get define mode");
			}
		}
	}
	
	m_ctx.logger()->trace("File opened. (nc_id = {})", m_file_id);
	
	// add file id as a root group
	m_groups.emplace("/", m_file_id);
}

Dnc_netcdf_file::Dnc_netcdf_file(Dnc_netcdf_file&& other) noexcept:
	m_ctx{other.m_ctx},
      m_filename{std::move(other.m_filename)},
      m_file_id{std::move(other.m_file_id)},
      m_groups{std::move(other.m_groups)},
      m_variables{std::move(other.m_variables)}
{}

void Dnc_netcdf_file::read_group(const Dnc_group& group)
{
	int dest_id = m_file_id;
	std::string dest_path;
	std::vector<std::string> groups_names = split_to_groups(group.path());
	int group_id = m_file_id;
	for (auto&& group_name : groups_names) {
		nc_try(nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id),
		    "Cannot read {} group from (nc_id = {})", group_name, dest_id);
		    
		m_ctx.logger()->trace("Read `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);
		
		dest_id = group_id;
		dest_path += "/" + group_name;
		m_groups.emplace(dest_path, dest_id);
	}
	
	for (auto&& attribute : group.attributes()) {
		this->get_attribute(group_id, NC_GLOBAL, attribute);
	}
}

void Dnc_netcdf_file::define_group(const Dnc_group& group)
{
	nc_id dest_id = m_file_id;
	std::string dest_path;
	std::vector<std::string> groups_names = split_to_groups(group.path());
	int group_id = m_file_id;
	for (auto&& group_name : groups_names) {
		if (nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id) != NC_NOERR) {
			nc_try(nc_def_grp(dest_id, group_name.c_str(), &group_id),
			    "Cannot define group {} in nc_id = {}", group_name, dest_id);
			m_ctx.logger()->trace("Defined `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);
		}
		
		dest_id = group_id;
		dest_path += "/" + group_name;
		m_groups.emplace(dest_path, dest_id);
	}
	
	for (auto&& attribute : group.attributes()) {
		this->put_attribute(group_id, NC_GLOBAL, attribute);
	}
}

void Dnc_netcdf_file::read_variable(const Dnc_variable& variable)
{
	auto it = m_variables.find(variable.path());
	if (it == m_variables.end()) {
		// get group path and variable name
		std::string group_path;
		std::string variable_name;
		std::tie(group_path, variable_name) = split_group_and_variable(variable.path());
		
		// get src_id
		auto group_it = m_groups.find(group_path);
		if (group_it == m_groups.end()) {
			// group not defined -> read it from file
			int dest_id = m_file_id;
			std::string dest_path;
			std::vector<std::string> groups_names = split_to_groups(group_path);
			int group_id = m_file_id;
			for (auto&& group_name : groups_names) {
				nc_try(nc_inq_grp_ncid(dest_id, group_name.c_str(), &group_id),
				    "Cannot read {} group from (nc_id = {})", group_name, dest_id);
				    
				m_ctx.logger()->trace("Read `{}' group (nc_id = {}) in (nc_id = {})", dest_path + "/" + group_name, group_id, dest_id);
				
				dest_id = group_id;
				dest_path += "/" + group_name;
				m_groups.emplace(dest_path, dest_id);
			}
			group_it = m_groups.find(group_path);
		}
		int src_id = group_it->second;
		
		int nc_var_id;
		nc_try(nc_inq_varid(src_id, variable_name.c_str(), &nc_var_id),
		    "Cannot inquire variable {} from (nc_id = {})", variable_name, src_id);
		    
		m_ctx.logger()->trace("Inquired `{}' variable (nc_id = {}) from (nc_id = {})", variable.path(),nc_var_id, src_id);
		m_variables.emplace(variable.path(), nc_var_id);
		
		m_ctx.logger()->trace("Getting attributes of `{}' variable", variable_name);
		for (auto&& attribute : variable.attributes()) {
			get_attribute(src_id, nc_var_id, attribute);
		}
	}
}

namespace {

int get_dimension_id(PDI::Context& ctx, int nc_dest_id, const std::string& dim_name, int type_dim)
{
	int dim_id;
	if (nc_inq_dimid(nc_dest_id, dim_name.c_str(), &dim_id) == NC_NOERR) {
		ctx.logger()->debug("`{}' dimension is already defined", dim_name);
		size_t dim_len;
		nc_try(nc_inq_dimlen (nc_dest_id, dim_id, &dim_len),
		    "Cannot inquire dimension length");
	} else {
		ctx.logger()->debug("Defining `{}' dimension", dim_name);
		nc_try(nc_def_dim(nc_dest_id, dim_name.c_str(), type_dim, &dim_id),
		    "Cannot define {} dimension", dim_name);
	}
	return dim_id;
}

} // namespace <anonymous> for Dnc_netcdf_file::define_variable

void Dnc_netcdf_file::define_variable(const Dnc_variable& variable)
{
	if (m_variables.find(variable.path()) != m_variables.end()) {
		// variable already defined
		m_ctx.logger()->trace("Variable `{}' already defined", variable.path());
		return;
	}
	
	// get group path and variable name
	std::string group_path;
	std::string variable_name;
	std::tie(group_path, variable_name) = split_group_and_variable(variable.path());
	m_ctx.logger()->trace("Variable path `{}' splitted to `{}' group and `{}` variable name", variable.path(), group_path, variable_name);
	
	// get dest_id
	auto group_it = m_groups.find(group_path);
	if (group_it == m_groups.end()) {
		// group not defined -> define empty
		m_ctx.logger()->trace("Group `{}' not defined. Defining...", group_path);
		Dnc_group group {m_ctx, group_path, PC_tree_t{}};
		this->define_group(group);
		group_it = m_groups.find(group_path);
	}
	int dest_id = group_it->second;
	
	// get variable type
	PDI::Datatype_uptr variable_type = variable.type();
	if (!variable_type) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Variable {}: No type defined", variable.path()};
	}
	if (!variable_type->dense()) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Variable {}: Data must be dense (continuous memory)", variable.path()};
	}
	m_ctx.logger()->trace("Variable `{}' type: {}", variable_name, variable_type->debug_string());
	
	// get variable stride and scalar datatype
	std::vector<size_t> var_stride;
	const PDI::Scalar_datatype* var_scalar_type = get_variable_stride(variable_type.get(), var_stride);
	nc_type type_id = nc_scalar_type(*var_scalar_type);
	
	// get variable dimensions
	m_ctx.logger()->trace("Defining dimensions of {} variable:", variable.path());
	std::vector<nc_id> dimensions_ids;
	std::vector<std::string> dimensions_names = variable.dimensions_names();
	if (dimensions_names.empty()) {
		m_ctx.logger()->trace("\t (arbitrary names)");
		// define arbitrary dimensions names
		for (int i = 0; i < var_stride.size(); i++) {
			std::string dim_name = variable_name + "_" + std::to_string(i);
			m_ctx.logger()->trace("\t {}[{}]", dim_name, var_stride[i]);
			dimensions_ids.emplace_back(get_dimension_id(m_ctx, dest_id, dim_name, var_stride[i]));
		}
	} else {
		if (var_stride.size() != dimensions_names.size()) {
			throw PDI::Error{PDI_ERR_VALUE, "Decl_netcdf plugin: Variable type dimension ({}) != defined variable dimensions ({})", var_stride.size(), dimensions_names.size()};
		}
		
		for (int i = 0; i < dimensions_names.size(); i++) {
			std::string dim_name = dimensions_names[i];
			m_ctx.logger()->trace("\t {}[{}]", dim_name, var_stride[i]);
			dimensions_ids.emplace_back(get_dimension_id(m_ctx, dest_id, dim_name, var_stride[i]));
		}
	}
	
	// define NetCDF variable
	nc_id var_id;
	if (nc_inq_varid(dest_id, variable_name.c_str(), &var_id) == NC_NOERR) {
		m_ctx.logger()->debug("Variable `{}' already defined in (nc_id = {}) of scalar_type (nc_id = {})", variable.path(), dest_id, type_id);
	} else {
		m_ctx.logger()->trace("Defining variable `{}' in (nc_id = {}) of scalar_type (nc_id = {})", variable.path(), dest_id, type_id);
		nc_try(nc_def_var(dest_id, variable_name.c_str(), type_id, dimensions_ids.size(), dimensions_ids.data(), &var_id),
		    "Cannot define `{}' variable in (nc_id = {})", variable_name, dest_id);
	}
	
	m_variables.emplace(variable.path(), var_id);
	
#if NC_HAS_PARALLEL4
	if (m_communicator) {
		nc_try(nc_var_par_access(dest_id, var_id, NC_COLLECTIVE),
		    "Cannot change the access of `{}' variable to parallel", variable_name);
	}
#endif
	
	// set the attributes
	m_ctx.logger()->trace("Putting attributes ({}), to `{}' variable", variable.attributes().size(), variable_name);
	for (auto&& attribute : variable.attributes()) {
		m_ctx.logger()->trace("Putting attribute {} to `{}' variable", attribute.name(), variable_name);
		this->put_attribute(dest_id, var_id, attribute);
	}
}

void Dnc_netcdf_file::get_attribute(nc_id src_id, nc_id var_id, const Dnc_attribute& attribute)
{
	if (PDI::Ref_w ref_w = attribute.value()) {
		m_ctx.logger()->trace("Getting `{}' attribute from (nc_id = {}/{})", attribute.name(), src_id, var_id);
		if (const PDI::Scalar_datatype* scalar_type = dynamic_cast<const PDI::Scalar_datatype*>(&ref_w.type())) {
			// get scalar attribute
			nc_try(nc_get_att(src_id, var_id, attribute.name().c_str(), ref_w.get()),
			    "Cannot get attribute  `{}' from (nc_id = {}/{})", attribute.name(), src_id, var_id);
		} else if (const PDI::Array_datatype* array_type = dynamic_cast<const PDI::Array_datatype*>(&ref_w.type())) {
			// get array attribute
			if (const PDI::Scalar_datatype* scalar_type = dynamic_cast<const PDI::Scalar_datatype*>(&array_type->subtype())) {
				if (!array_type->dense()) {
					throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Attribute type must be dense (continuous memory)"};
				}
				if (nc_get_att(src_id, var_id, attribute.name().c_str(), ref_w.get())) {
					throw PDI::Error{PDI_ERR_SYSTEM, "Decl_netcdf plugin: Cannot get attribute `{}' from (nc_id = {}/{})", attribute.name(), src_id, var_id};
				}
			} else {
				throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Multi dimensional array for attribute not supported"};
			}
		} else {
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Record datatype for attribute not supported"};
		}
	} else {
		m_ctx.logger()->debug("Decl_netcdf plugin: No write access to read data from file attribute: {}", attribute.name());
	}
}

void Dnc_netcdf_file::put_attribute(nc_id dest_id, nc_id var_id, const Dnc_attribute& attribute)
{
	m_ctx.logger()->trace("Putting `{}' attribute to (nc_id = {}/{})", attribute.name(), dest_id, var_id);
	nc_del_att(dest_id, var_id, attribute.name().c_str()); // try to delete old attribute, if fails nothing happens
	if (PDI::Ref_r ref_r = attribute.value()) {
		if (const PDI::Scalar_datatype* scalar_type = dynamic_cast<const PDI::Scalar_datatype*>(&ref_r.type())) {
			// set scalar attribute
			nc_try(nc_put_att(dest_id, var_id, attribute.name().c_str(), nc_scalar_type(*scalar_type), 1, ref_r.get()),
			    "Cannot put attribute  `{}' to (nc_id = {}/{})", attribute.name(), dest_id, var_id);
		} else if (const PDI::Array_datatype* array_type = dynamic_cast<const PDI::Array_datatype*>(&ref_r.type())) {
			// set array attribute
			if (const PDI::Scalar_datatype* scalar_type = dynamic_cast<const PDI::Scalar_datatype*>(&array_type->subtype())) {
				if (!array_type->dense()) {
					throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Attribute type must be dense (continuous memory)"};
				} else {
					nc_try(nc_put_att(dest_id, var_id, attribute.name().c_str(), nc_scalar_type(*scalar_type), array_type->size(), ref_r.get()),
					    "Cannot put attribute  `{}' to (nc_id = {}/{})", attribute.name(), dest_id, var_id);
				}
			} else {
				throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Multi dimensional array for attribute not supported"};
			}
		} else {
			throw PDI::Error{PDI_ERR_TYPE, "Decl_netcdf plugin: Record datatype for attribute not supported"};
		}
	} else {
		m_ctx.logger()->debug("Decl_netcdf plugin: No read access to write data to attribute: {}", attribute.name());
	}
}

void Dnc_netcdf_file::enddef() const
{
	nc_try(nc_enddef(m_file_id), "Cannot end define mode");
	m_ctx.logger()->debug("Define mode end in file {} (nc_id = {})", m_filename, m_file_id);
}

void Dnc_netcdf_file::put_variable(const Dnc_variable& variable, const Dnc_io& write, PDI::Ref_r ref_r)
{
	// check access
	if (!ref_r) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Cannot write `{}'. Need read access to write it to file", variable.path()};
	}
	
	// get group path and variable name
	std::string group_path;
	std::string variable_name;
	std::tie(group_path, variable_name) = split_group_and_variable(variable.path());
	
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
	get_variable_stride(variable.type().get(), var_stride);
	std::vector<size_t> var_start = write.get_dims_start(var_stride);
	std::vector<size_t> var_count = write.get_dims_count(var_stride);
	
	// write variable
	m_ctx.logger()->trace("Putting variable `{}'", variable_name);
	
	if (var_stride.empty()) {
		nc_try(nc_put_var(dest_id, var_id, ref_r.get()),
		    "Decl_netcdf plugin: Cannot write `{}' to (nc_id = {})", dest_id);
	} else {
		nc_try(nc_put_vara(dest_id, var_id, var_start.data(), var_count.data(), ref_r.get()),
		    "Decl_netcdf plugin: Cannot write `{}' to (nc_id = {})", dest_id);
	}
}

void Dnc_netcdf_file::get_variable(const Dnc_variable& variable, const Dnc_io& read, PDI::Ref_w ref_w)
{
	if (!ref_w) {
		throw PDI::Error{PDI_ERR_RIGHT, "Decl_netcdf plugin: Cannot read `{}'. Need write access to read it from file", variable.path()};
	}
	
	// get variable stride to calculate start and count
	std::vector<size_t> var_stride;
	get_variable_stride(variable.type().get(), var_stride);
	std::vector<size_t> var_start = read.get_dims_start(var_stride);
	std::vector<size_t> var_count = read.get_dims_count(var_stride);
	
	// get group path and variable name
	std::string group_path;
	std::string variable_name;
	std::tie(group_path, variable_name) = split_group_and_variable(variable.path());
	
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
	m_ctx.logger()->trace("Getting variable `{}'", variable.path());
	if (var_stride.empty()) {
		nc_try(nc_get_var(src_id, var_id, ref_w.get()),
		    "Cannot read `{}' from file", variable.path());
	} else {
		nc_try(nc_get_vara(src_id, var_id, var_start.data(), var_count.data(), ref_w.get()),
		    "Cannot read `{}' from file", variable.path());
	}
}

Dnc_netcdf_file::~Dnc_netcdf_file()
{
	m_ctx.logger()->debug("Closing file {} (nc_id = {})", m_filename, m_file_id);
	nc_try(nc_close(m_file_id),
	    "Cannot close the file {}", m_filename);
}

} // namespace decl_netcdf
