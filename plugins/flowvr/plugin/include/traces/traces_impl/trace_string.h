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

#ifndef PDI_FLOWVR_TRACE_STRING
#define PDI_FLOWVR_TRACE_STRING

#include <string>

#include <flowvr/trace.h>

#include <pdi/context.h>
#include <pdi/ref_any.h>

#include "trace_base.h"

namespace _flowvr_plugin {

/// String value trace
class Trace_string : public Trace_base
{
	/// FlowVR string trace
	std::unique_ptr<flowvr::TypedTrace<std::string>> m_flowvr_trace;
	
public:
	/** Creates new string trace
	 * \param[in] ctx context of this trace
	 * \param[in] name name of the trace
	 */
	Trace_string(PDI::Context& ctx, const std::string& name);
	
	flowvr::Trace* get() const override;
	
	void write(const PDI::Ref_r& ref_r) override;
	
};

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_TRACE_STRING
