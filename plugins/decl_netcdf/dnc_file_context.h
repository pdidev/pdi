/*******************************************************************************
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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