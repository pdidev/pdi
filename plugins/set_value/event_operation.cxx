// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <pdi/context.h>

#include "event_operation.h"

namespace set_value {

Event_operation::Event_operation(PDI::Context& ctx, PC_tree_t event_value_node)
	: Operation{ctx}
{
	m_event_to_call = PDI::to_string(event_value_node);
	context().logger().debug("Event operation loaded: {}", m_event_to_call);
}

void Event_operation::execute()
{
	context().logger().trace("Calling {} event", m_event_to_call);
	context().event(m_event_to_call.c_str());
}

} // namespace set_value
