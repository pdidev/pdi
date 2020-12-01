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

#ifndef PDI_FLOWVR_TRACE
#define PDI_FLOWVR_TRACE

#include <string>

#include <flowvr/trace.h>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "traces_impl/trace_base.h"
#include "traces_impl/trace_float.h"
#include "traces_impl/trace_int.h"
#include "traces_impl/trace_string.h"

namespace _flowvr_plugin {

/// Trace used to debug flowvr application
class Trace
{
	/// Descriptor name on which make write a trace
	std::string m_on_data;

	/// Trace holder
	std::unique_ptr<Trace_base> m_trace;
	
	/** Reads config, set on_data
	 *
	 *  \param[in] config the configuration to read (trace root)
	 */
	void load_on_data(PC_tree_t config);
	
	/** Reads config, create proper trace type.
	 *
	 *  \param[in] ctx the PDI context for this plugin instance
	 *  \param[in] name name of the trace
	 */
	void load_trace(PDI::Context& ctx, const std::string& name);
	
public:
	/** Creates new trace
	 *
	 *  \param[in] ctx the PDI context for this plugin instance
	 *  \param[in] name name of the trace
	 *  \param[in] config the configuration to read (trace root)
	 */
	Trace(PDI::Context& ctx, const std::string& name, PC_tree_t config);
	
	/** Returns flowvr::Trace pointer
	 *  \return flowvr::Trace pointer
	 */
	flowvr::Trace* get() const;
	
	/** Writes the trace from the reference
	 *
	 * \param[in] data_name name of shared data
	 * \param[in] ref_r Ref where from get the trace value
	 */
	void write(const std::string& data_name, const PDI::Ref_r ref_r);
	
};

} // namespace _flowvr_plugin

#endif // PDI_FLOWVR_TRACE
