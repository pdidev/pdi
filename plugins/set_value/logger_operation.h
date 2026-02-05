/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_LOGGER_OPERATION_H_
#define SET_VALUE_LOGGER_OPERATION_H_
#include <map>
#include <string>

#include <pdi/context.h>
#include <pdi/expression.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

class Logger_operation: public Operation
{
	/// level to set to the logger on execute
	PDI::Expression m_level;

	/// pattern to set to the logger on execute
	std::string m_pattern;

	/// evaluate pattern on execute
	bool m_evaluate = false;

public:
	/** Creates logger operation
     * \param[in] ctx context of the operation
     * \param[in] logger_node yaml config tree of logger operation
     */
	Logger_operation(PDI::Context& ctx, PC_tree_t logger_node);

	void execute() override;
};

} // namespace set_value

#endif //SET_VALUE_LOGGER_OPERATION_H_
