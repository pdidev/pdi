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

#include <pdi/error.h>

#include "event_operation.h"
#include "expose_operation.h"
#include "set_operation.h"
#include "share_operation.h"
#include "release_operation.h"

#include "trigger.h"

namespace set_value {

Trigger::Trigger(PDI::Context& ctx, PC_tree_t operation_list_node):
    m_ctx{ctx}
{
	if (!PDI::is_list(operation_list_node)) {
		throw PDI::Config_error{operation_list_node, "Operations must be defined as a list"};
	}

	size_t operations_count = PDI::len(operation_list_node);
	for (int i = 0; i < operations_count; i++) {
		PC_tree_t value_map = PC_get(operation_list_node, "[%d]", i);
		std::string operation = PDI::to_string(PC_get(value_map, "{0}"));
		PC_tree_t operation_value = PC_get(value_map, "<0>");

		if (operation == "set") {
			m_operations.emplace_back(new Set_operation{m_ctx, operation_value});
		} else if (operation == "share") {
			m_operations.emplace_back(new Share_operation{m_ctx, operation_value});
		} else if (operation == "expose") {
			m_operations.emplace_back(new Expose_operation{m_ctx, operation_value});
		} else if (operation == "event") {
			m_operations.emplace_back(new Event_operation{m_ctx, operation_value});
		} else if (operation == "release") {
			m_operations.emplace_back(new Release_operation{m_ctx, operation_value});
		} else {
			throw PDI::Config_error{PC_get(value_map, "{0}"), "Unknown operation: {}", operation};
		}
	}
}

void Trigger::execute()
{
	for (auto& operation : m_operations) {
		operation->execute();
	}
}

}  // namespace set_value
