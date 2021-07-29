/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/ref_any.h>

#include "expose_operation.h"

namespace set_value {

Expose_operation::Expose_operation(PDI::Context& ctx, PC_tree_t list_of_values):
    Operation{ctx}
{
	size_t list_size = PDI::len(list_of_values);
	context().logger()->debug("Expose operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		PC_tree_t value_element = PC_get(list_of_values, "[%d]", i);
		std::string data_name {PDI::to_string(PC_get(value_element, "{0}"))};
		context().logger()->trace("\t {}: {}", i, data_name);
		m_data_to_expose.emplace_back(std::move(data_name), PC_get(value_element, "<0>"));
	}
}

void Expose_operation::execute()
{
    for (auto& data_to_expose : m_data_to_expose) {
		PDI::Data_descriptor& data_desc = context().desc(data_to_expose.first);
		PDI::Ref value_ref {PDI::Expression{data_to_expose.second}.to_ref(context(), *data_desc.default_type()->evaluate(context()))};
		context().logger()->trace("Exposing {} with size {} B", data_to_expose.first, value_ref.type().buffersize());
		data_desc.share(value_ref, false, false);
		context().desc(data_to_expose.first).release();
	}
}

}  // namespace set_value
