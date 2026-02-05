/*
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_TRIGGER_H_
#define SET_VALUE_TRIGGER_H_

#include <memory>
#include <vector>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

class Trigger
{
	PDI::Context& m_ctx;

	// operations to execute
	std::vector<std::unique_ptr<Operation>> m_operations;

public:
	Trigger(PDI::Context& ctx, PC_tree_t value_node);

	/** Sets/shares/exposes all values given in on_init/on_event/on_data
     */
	void execute();
};

} // namespace set_value

#endif //SET_VALUE_TRIGGER_H_
