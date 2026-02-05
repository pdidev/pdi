// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <pdi/context.h>

#include "release_operation.h"

namespace set_value {

Release_operation::Release_operation(PDI::Context& ctx, PC_tree_t release_value_node)
	: Operation{ctx}
{
	size_t list_size = PDI::len(release_value_node);
	context().logger().debug("Release operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		std::string data_to_release = PDI::to_string(PC_get(release_value_node, "[%d]", i));
		context().logger().trace("\t {}: {}", i, data_to_release);
		m_data_to_release.emplace_back(data_to_release);
	}
}

void Release_operation::execute()
{
	for (auto&& data_to_release: m_data_to_release) {
		context().logger().trace("Calling {} data release", data_to_release);
		context().desc(data_to_release).release();
	}
}

} // namespace set_value
