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

#ifndef PDI_FLOWVR_STAMP_EXPR_FLOAT_ARRAY
#define PDI_FLOWVR_STAMP_EXPR_FLOAT_ARRAY

#include <string>

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include <flowvr/module.h>
#include <flowvr/stamp.h>

#include "../stamp_base.h"

namespace _flowvr_plugin {

/// Expression float array stamp
class Stamp_expr_float_array : public Stamp_base
{
	std::vector<PDI::Expression> m_value;
public:
	/** Creates float array expression stamp
	 * \param[in] ctx context of the stamp
	 * \param[in] parent_port port that contains this stamp
	 * \param[in] name name of the stamp
	 * \param[in] expression vector of expressions value of this stamp
	 */
	Stamp_expr_float_array(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, std::vector<PDI::Expression> expression);
	
	void read_from_flowvr_stamp(const flowvr::Stamps& read_stamp) override;
	
	void write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const override;
}; // Stamp_expr_float_array

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_STAMP_EXPR_FLOAT_ARRAY
