/*
 * SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef SET_VALUE_OPERATION_H_
#define SET_VALUE_OPERATION_H_

#include <pdi/context.h>

namespace set_value {

class Operation
{
	/// Context of the operation
	PDI::Context& m_ctx;

protected:
	/** Getter of context
     * \return context of the operation
     */
	PDI::Context& context();

public:
	/** Creates operation
     * \param[in] ctx context of the operation
     * \param[in] release_value_node yaml config tree of operation
     */
	Operation(PDI::Context& ctx);

	/// Triggers an operation
	virtual void execute() = 0;

	/// Destroys operation
	virtual ~Operation() = default;
};

} // namespace set_value

#endif //SET_VALUE_OPERATION_H_
