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

#ifndef DECL_NETCDF_DNC_NETCDF_FILE_H_
#define DECL_NETCDF_DNC_NETCDF_FILE_H_

#include <map>

#include <pdi/context.h>
#include <pdi/expression.h>

#include "dnc_group.h"
#include "dnc_io.h"
#include "dnc_variable.h"

namespace decl_netcdf {

/// Represents NetCDF opened file.
class Dnc_netcdf_file
{
    using nc_id = int;

    /// Context of this opened file
    PDI::Context& m_ctx;

    std::string m_filename;

    nc_id m_file_id;

    /// Groups in NetCDF file
    std::unordered_map<std::string, nc_id> m_groups;

    /// Variables in NetCDF file
    std::unordered_map<std::string, nc_id> m_variables;

    /** Puts attribute to the file
     * 
     * \param dest_id file nc_id or group nc_id in which the attribute will be put
     * \param var_id variable nc_id in which the attribute will be put (NC_GLOBAL if attribute to group)
     * \param attribute attribute to put
     */
    void put_attribute(nc_id dest_id, nc_id var_id, const Dnc_attribute& attribute);

    /** Gets attribute from the file
     * 
     * \param src_id file nc_id or group nc_id from which the attribute will be gotten
     * \param var_id variable nc_id from which the attribute will be gotten (NC_GLOBAL if attribute to group)
     * \param attribute attribute to get
     */
    void get_attribute(nc_id src_id, nc_id var_id, const Dnc_attribute& attribute);

public:
    /** Opens NetCDF files with given right flag.
     * 
     * \param ctx context for this file
     * \param filename path to the file to be opened
     * \param right_flag right flag which be used to open file
     * \param mpi_comm_expr if not empty, opens NetCDF file in parallel using this MPI_Comm
     */
    Dnc_netcdf_file(PDI::Context& ctx, const std::string& filename, int rights_flag, PDI::Expression mpi_comm_expr);

    /// Deleted copy constructor
    Dnc_netcdf_file(const Dnc_netcdf_file& other) = delete;

    /// Move constructor
    Dnc_netcdf_file(Dnc_netcdf_file&& other) noexcept;
    
    /** Reads group nc_id from file and all attributes defined in yaml
     * 
     * \param group group to be read
     */
    void read_group(const Dnc_group& group);

    /** Defines group in the file and all attributes defined in yaml
     * 
     * \param group group to be defined
     */
    void define_group(const Dnc_group& group);

    /** Reads variable nc_id from file and all attributes defined in yaml
     * 
     * \param variable variable to be read
     */
    void read_variable(const Dnc_variable& variable);
    
    /** Defines variable in the file and all attributes defined in yaml
     * 
     * \param variable variable to be defined
     */
    void define_variable(const Dnc_variable& variable);

    /// Ends definion mode in NetCDF file
    void enddef() const;

    /** Puts variable to the file
     * 
     * \param variable variable to put
     * \param write Dnc_io that deteremines the write operation
     * \param ref_r reference where from get data to put
     */
    void put_variable(const Dnc_variable& variable, const Dnc_io& write, PDI::Ref_r ref_r);

    /** Gets variable from the file
     * 
     * \param variable variable to get
     * \param write Dnc_io that deteremines the read operation
     * \param ref_w reference where the data will be written
     */
    void get_variable(const Dnc_variable& variable, const Dnc_io& read, PDI::Ref_w ref_w);
    
    /// Destructor
    ~Dnc_netcdf_file();

};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_NETCDF_FILE_H_