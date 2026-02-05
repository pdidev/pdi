// SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
// SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/ref_any.h>

#include "expose_operation.h"

namespace set_value {

Expose_operation::Expose_operation(PDI::Context& ctx, PC_tree_t list_of_values)
	: Operation{ctx}
{
	size_t list_size = PDI::len(list_of_values);
	context().logger().debug("Expose operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		PC_tree_t value_element = PC_get(list_of_values, "[%d]", i);
		std::string data_name{PDI::to_string(PC_get(value_element, "{0}"))};
		context().logger().trace("\t {}: {}", i, data_name);
		m_data_to_expose.emplace_back(std::move(data_name), PC_get(value_element, "<0>"));
	}
}

void Expose_operation::execute()
{
	for (auto& data_to_expose: m_data_to_expose) {
		PDI::Data_descriptor& data_desc = context().desc(data_to_expose.first);
		PDI::Ref value_ref{PDI::Expression{data_to_expose.second}.to_ref(context(), data_desc.default_type()->evaluate(context()))};
		context().logger().trace("Exposing {} with size {} B", data_to_expose.first, value_ref.type()->buffersize());
		data_desc.share(value_ref, false, false);
		context().desc(data_to_expose.first).release();
	}
}

} // namespace set_value
