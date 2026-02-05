/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_EVENT_OPERATION_H_
#define SET_VALUE_EVENT_OPERATION_H_

#include <map>
#include <string>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

/// Operation that calls an event when triggered
class Event_operation: public Operation
{
	/// event to call when operation triggered
	std::string m_event_to_call;

public:
	/** Creates event operation
     * \param[in] ctx context of the operation
     * \param[in] event_value_node yaml config tree of operation
     */
	Event_operation(PDI::Context& ctx, PC_tree_t event_value_node);

	void execute() override;
};

} // namespace set_value

#endif //SET_VALUE_EVENT_OPERATION_H_
