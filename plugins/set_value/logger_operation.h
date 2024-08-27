/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
