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
#include <spdlog/spdlog.h>

#include "ports/stamps/expr/stamp_expr_int_array.h"

namespace _flowvr_plugin {

Stamp_expr_int_array::Stamp_expr_int_array(PDI::Context& ctx, const flowvr::Port* parent_port, const std::string& name, std::vector<PDI::Expression> expression):
	Stamp_base{ctx, parent_port, name},
	m_value{expression}
{
	m_stamp_info = new flowvr::StampInfo(m_name, flowvr::TypeArray::create(m_value.size(), flowvr::TypeInt::create()));
	m_ctx.logger()->debug("{} stamp: Int array expression created", m_name);
}

void Stamp_expr_int_array::read_from_flowvr_stamp(const flowvr::Stamps& read_stamp)
{}

void Stamp_expr_int_array::write_to_flowvr_stamp(flowvr::StampsWrite& write_stamp) const
{
	for (int stamp_id = 0; stamp_id < m_value.size(); stamp_id++) {
		bool status = write_stamp.write((*m_stamp_info)[stamp_id], static_cast<int>(m_value[stamp_id].to_long(m_ctx)));
		if (status) {
			m_ctx.logger()->debug("{} stamp: Message update: Message.stamps.{}[{}] = {}", m_name, m_name, stamp_id, m_value[stamp_id].to_long(m_ctx));
		} else {
			throw PDI::Unavailable_error{"{} stamp: Cannot write stamp to message", m_name};
		}
	}
}

} // namespace _flowvr_plugin
