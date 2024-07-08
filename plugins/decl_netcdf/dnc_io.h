/*******************************************************************************
 * Copyright (C) 2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef DECL_NETCDF_DNC_IO_H_
#define DECL_NETCDF_DNC_IO_H_

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

namespace decl_netcdf {

/// I/O operation base class (derived: dnc_read and dnc_write)
class Dnc_io
{
protected:
	/// Context of this I/O
	PDI::Context& m_ctx;
	
	/// NetCDF variable path of this I/O
	PDI::Expression m_variable_path;
	
	/// I/O done only when m_when is evaluates to true
	PDI::Expression m_when;
	
	/// Starts of hyperslab of this I/O
	std::vector<PDI::Expression> m_start;
	
	/// Counts of hyperslab of this I/O
	std::vector<PDI::Expression> m_subsize;
	
	PDI::Expression m_sizeof_var;
	
public:
	/** Creates I/O operation (read or write) on NetCDF file
	 *
	 * \param ctx Context of this I/O
	 * \param file File associated with this I/O operation
	 * \param config Configuration node of this I/O
	 */
	Dnc_io(PDI::Context& ctx, PC_tree_t config);
	
	/** Getter for variable path
	 *
	 *  \return variable path of this I/O, empty if no variable path
	 */
	std::string variable_path() const;
	
	/** Getter for size_of variable path
	 *
	 *  \return size_of variable path of this I/O, empty if no variable path
	 */
	std::string sizeof_variable_path() const;
	
	/** Getter for variable name
	 *
	 *  \return variable name of this I/O, empty if no variable name
	 */
	bool when() const;
	
	/** Creates vector with start for variable hyperslab
	 *
	 * \param stride vector with stride of variable
	 *
	 * \return vector with start for variable hyperslab
	 */
	std::vector<size_t> get_dims_start(const std::vector<size_t>& stride) const;
	
	/** Creates vector with count for variable hyperslab
	 *
	 * \param stride vector with stride of variable
	 *
	 * \return vector with count for variable hyperslab
	 */
	std::vector<size_t> get_dims_count(const std::vector<size_t>& stride) const;
	
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_IO_H_
