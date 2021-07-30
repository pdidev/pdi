/*******************************************************************************
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include "traces/traces_impl/trace_int.h"

namespace _flowvr_plugin {

Trace_int::Trace_int(PDI::Context& ctx, const std::string& name):
	Trace_base{ctx},
	m_flowvr_trace{new flowvr::TypedTrace<int>(name)}
{
	m_ctx.logger().debug("{} trace: Int trace created", m_flowvr_trace->getName());
}

flowvr::Trace* Trace_int::get() const
{
	return m_flowvr_trace.get();
}

void Trace_int::write(const PDI::Ref_r& ref_r)
{
	const int value = *static_cast<const int*>(ref_r.get());
	m_ctx.logger().debug("{} trace: Writing {}", m_flowvr_trace->getName(), value);
	m_flowvr_trace->write(value);
}
	
} // namespace _flowvr_plugin
