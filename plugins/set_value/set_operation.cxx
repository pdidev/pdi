// SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/ref_any.h>

#include "set_operation.h"

namespace set_value {

Set_operation::Set_operation(PDI::Context& ctx, PC_tree_t list_of_values)
	: Operation{ctx}
{
	size_t list_size = PDI::len(list_of_values);
	context().logger().debug("Set operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		PC_tree_t value_element = PC_get(list_of_values, "[%d]", i);
		std::string data_name{PDI::to_string(PC_get(value_element, "{0}"))};
		context().logger().trace("\t {}: {}", i, data_name);
		m_data_to_set.emplace_back(std::move(data_name), PC_get(value_element, "<0>"));
	}
}

void Set_operation::execute()
{
	for (auto& data_to_set: m_data_to_set) {
		PDI::Ref existing_ref{context().desc(data_to_set.first).ref()}; // let Expression get Ref_r
		PDI::Ref_r value_ref{PDI::Expression{data_to_set.second}.to_ref(context(), existing_ref.type())};
		if (PDI::Ref_w existing_ref_w{existing_ref}) {
			if (existing_ref_w.type()->buffersize() != value_ref.type()->buffersize()) {
				throw PDI::Value_error{
					"Cannot set value to exisitng reference. Existing buffersize = {}, value buffersize = {}",
					existing_ref_w.type()->buffersize(),
					value_ref.type()->buffersize()
				};
			}
			context().logger().trace("Copy value to {} with size {} B", data_to_set.first, value_ref.type()->buffersize());
			memcpy(existing_ref_w.get(), value_ref.get(), existing_ref_w.type()->buffersize());
		} else {
			throw PDI::Right_error{"Cannot get write access for `{}' to set values", data_to_set.first};
		}
	}
}

} // namespace set_value
