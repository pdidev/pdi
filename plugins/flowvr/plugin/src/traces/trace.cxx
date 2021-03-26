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
#include <pdi/scalar_datatype.h>

#include "traces/trace.h"

namespace _flowvr_plugin {

void Trace::load_on_data(PC_tree_t config)
{
	PC_tree_t on_data_node = PC_get(config, ".on_data");
	if (PC_status(on_data_node)) {
		throw PDI::Config_error{config, "Trace must have defined `on_data'"};
	}
	m_on_data = PDI::to_string(on_data_node);
}

void Trace::load_trace(PDI::Context& ctx, const std::string& name)
{
	const PDI::Datatype_uptr trace_datatype = ctx[m_on_data].default_type()->evaluate(ctx);
	const PDI::Scalar_datatype* scalar_datatype = dynamic_cast<PDI::Scalar_datatype*>(trace_datatype.get());
	const PDI::Array_datatype* array_datatype = dynamic_cast<PDI::Array_datatype*>(trace_datatype.get());
	
	if (array_datatype) {
		scalar_datatype = dynamic_cast<const PDI::Scalar_datatype*>(&array_datatype->subtype());
	}
	
	if (scalar_datatype) {
		PDI::Scalar_kind trace_kind = scalar_datatype->kind();
		if (scalar_datatype->kind() == PDI::Scalar_kind::SIGNED && scalar_datatype->buffersize() == sizeof(int)) {
			m_trace.reset(new Trace_int(ctx, name));
			return;
		} else if (scalar_datatype->kind() == PDI::Scalar_kind::FLOAT  && scalar_datatype->buffersize() == sizeof(float)) {
			m_trace.reset(new Trace_float(ctx, name));
			return;
		} else if (array_datatype && scalar_datatype->kind() == PDI::Scalar_kind::UNSIGNED && scalar_datatype->buffersize() == sizeof(char)) {
			m_trace.reset(new Trace_string(ctx, name));
		}
	}
	throw PDI::Type_error{"{} trace has not supported type for Trace", name};
}

Trace::Trace(PDI::Context& ctx, const std::string& name, PC_tree_t config)
{
	load_on_data(config);
	load_trace(ctx, name);
	ctx.callbacks().add_data_callback([this](const std::string& name, PDI::Ref ref) {
		this->write(name, ref);
	}, m_on_data);
}

flowvr::Trace* Trace::get() const
{
	return m_trace->get();
}

void Trace::write(const std::string& data_name, const PDI::Ref_r ref_r)
{
	if (ref_r) {
		m_trace->write(ref_r);
	} else {
		throw PDI::Right_error{"Trace: Unable to get read permissions for `{}'", data_name};
	}
}

} // namespace _flowvr_plugin
