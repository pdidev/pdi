/*
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_RELEASE_OPERATION_H_
#define SET_VALUE_RELEASE_OPERATION_H_

#include <map>
#include <string>
#include <vector>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

/// Operation that releases data when triggered
class Release_operation: public Operation
{
	/// data to release when operation triggered
	std::vector<std::string> m_data_to_release;

public:
	/** Creates release operation
     * \param[in] ctx context of the operation
     * \param[in] release_value_node yaml config tree of operation
     */
	Release_operation(PDI::Context& ctx, PC_tree_t release_value_node);

	void execute() override;
};

} // namespace set_value

#endif //SET_VALUE_RELEASE_OPERATION_H_
