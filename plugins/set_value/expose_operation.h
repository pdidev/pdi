/*
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_EXPOSE_OPERATION_H_
#define SET_VALUE_EXPOSE_OPERATION_H_

#include <map>
#include <string>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

class Expose_operation: public Operation
{
	/// map of data to expose (expose new Ref (created from Expression))
	std::vector<std::pair<std::string, PC_tree_t>> m_data_to_expose;

public:
	/** Creates expose operation
     * \param[in] ctx context of the operation
     * \param[in] list_of_values yaml config tree of operation
     */
	Expose_operation(PDI::Context& ctx, PC_tree_t list_of_values);

	void execute() override;
};

} // namespace set_value

#endif //SET_VALUE_EXPOSE_OPERATION_H_
