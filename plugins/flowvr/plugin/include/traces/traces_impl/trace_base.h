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

#ifndef PDI_FLOWVR_TRACE_BASE
#define PDI_FLOWVR_TRACE_BASE

#include <string>

#include <flowvr/trace.h>

#include <pdi/context.h>
#include <pdi/ref_any.h>

namespace _flowvr_plugin {

/// Base class for traces
class Trace_base
{
protected:
	/// Context of this trace
	PDI::Context& m_ctx;
	
	/** Creates new trace
	 * \param[in] ctx context of this trace
	 */
	Trace_base(PDI::Context& ctx);
	
public:
	/** Returns flowvr::Trace pointer
	 *  \return flowvr::Trace pointer
	 */
	virtual flowvr::Trace* get() const = 0;
	
	/** Writes the trace from the reference
	 *
	 *  \param[in] ref_r Ref where from get the trace value
	 */
	virtual void write(const PDI::Ref_r& ref_r) = 0;
};

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_TRACE_BASE
