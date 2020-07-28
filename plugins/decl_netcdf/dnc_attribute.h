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


#ifndef DECL_NETCDF_DNC_ATTRIBUTE_H_
#define DECL_NETCDF_DNC_ATTRIBUTE_H_

#include <pdi/pdi_fwd.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

namespace decl_netcdf {

/// Represents NetCDF attribute. Created from config.
class Dnc_attribute
{
    /// Context of this attribute
    PDI::Context& m_ctx;

    /// Name of this attribute
    std::string m_name;

    /// Value of this attribute
    PDI::Expression m_value;
    
public:

    /** Creates NetCDF attribute information from yaml
     *
     * \param ctx Context of this I/O
     * \param name Name of this attribute
     * \param config Configuration node of this attribute
     */
    Dnc_attribute(PDI::Context& ctx, const std::string& name, PC_tree_t config);

    /** Getter for attribute name
     *  
     * \return name of the attribute
     */
    const std::string& name() const;

    /** Getter for value
     *  
     * \return Ref created from value
     */
    PDI::Ref value() const;
};

} // namespace decl_netcdf

#endif // DECL_NETCDF_DNC_ATTRIBUTE_H_
