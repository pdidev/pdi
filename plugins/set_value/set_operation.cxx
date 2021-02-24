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

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/ref_any.h>

#include "set_operation.h"

namespace set_value {

Set_operation::Set_operation(PDI::Context& ctx, PC_tree_t list_of_values):
    Operation{ctx}
{
	size_t list_size = PDI::len(list_of_values);
		context().logger()->debug("Set operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		PC_tree_t value_element = PC_get(list_of_values, "[%d]", i);
		std::string data_name {PDI::to_string(PC_get(value_element, "{0}"))};
		context().logger()->trace("\t {}: {}", i, data_name);
		m_data_to_set.emplace_back(std::move(data_name), PC_get(value_element, "<0>"));
	}
}

void Set_operation::execute()
{
    for (auto& data_to_set : m_data_to_set) {
		PDI::Ref existing_ref {context().desc(data_to_set.first).ref()}; // let Expression get Ref_r
		PDI::Ref_r value_ref {PDI::Expression{data_to_set.second}.to_ref(context(), existing_ref.type())};
		if (PDI::Ref_w existing_ref_w {existing_ref}) {
			if (existing_ref_w.type().buffersize() != value_ref.type().buffersize()) {
				throw PDI::Value_error{"Cannot set value to exisitng reference. Existing buffersize = {}, value buffersize = {}",
					existing_ref_w.type().buffersize(),
					value_ref.type().buffersize()};
			}
			context().logger()->trace("Copy value to {} with size {} B", data_to_set.first, value_ref.type().buffersize());
			memcpy(existing_ref_w.get(), value_ref.get(), existing_ref_w.type().buffersize());
		} else {
			throw PDI::Right_error{"Cannot get write access for `{}' to set values", data_to_set.first};
		}
	}
}

}  // namespace <anonymous>
