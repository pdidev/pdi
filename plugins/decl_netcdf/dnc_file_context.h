/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * SPDX-FileCopyrightText: 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef DECL_NETCDF_DNC_FILE_CONTEXT_H_
#define DECL_NETCDF_DNC_FILE_CONTEXT_H_

#include <list>
#include <map>

#include <pdi/pdi_fwd.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

#include "dnc_group.h"
#include "dnc_io.h"
#include "dnc_variable.h"

namespace decl_netcdf {

/// Represents NetCDF file. Created from config.
class Dnc_file_context
{
	/// Context of this file
	PDI::Context& m_ctx;

	/// File path
	PDI::Expression m_file_path;

	/// MPI communicator of this file
	PDI::Expression m_communicator;

	/// I/O done only when m_when is evaluates to true
	PDI::Expression m_when;

	/// Groups in NetCDF file
	std::unordered_map<std::string, Dnc_group> m_groups;

	/// Variables in NetCDF file
	std::unordered_map<std::string, Dnc_variable> m_variables;

	/// Map of desc name to Read operation on NetCDF file
	std::unordered_map<std::string, Dnc_io> m_read;

	/// Map of desc name to Write operation on NetCDF file
	std::unordered_map<std::string, Dnc_io> m_write;

	/// Map of desc name to Size_of operation on NetCDF file
	std::unordered_map<std::string, Dnc_io > m_sizeof;

	/** Execute all I/O operations (called on event)
	 *
	 */
	void execute();

	/** Execute I/O operation only for shared descriptor (called on data share)
	 *
	 * \param desc_name shared descriptor name
	 * \param ref Ref to the shared descriptor
	 */
	void execute(const std::string& desc_name, PDI::Ref ref);

	/** Returns variable of given path, can be temporary if not found in m_variables
	 *
	 * \param desc_name descriptor name corelated with this variable
	 * \param variable_path path of the variable
	 * \param variables_holder list of temporary variables created from desc_name
	 * \return variable of given path, nullptr if variable doesn't exist
	 */
	Dnc_variable* variable(const std::string& desc_name, const std::string& variable_path, std::list<Dnc_variable>& variables_holder);

public:
	/** Creates a NetCDF file information from yaml
	 *
	 * \param ctx Context of this file
	 * \param config Configuration node of this file
	 */
	Dnc_file_context(PDI::Context& ctx, PC_tree_t config);

	/** Copy constructor - deleted
	 *
	 * \param other Dnc_file_context to copy
	 */
	Dnc_file_context(const Dnc_file_context& other) = delete;

	/** Move constructor
	 *
	 * \param other Dnc_file_context to move
	 */
	Dnc_file_context(Dnc_file_context&& other) noexcept;
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_FILE_CONTEXT_H_
