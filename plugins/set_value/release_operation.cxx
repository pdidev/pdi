/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <pdi/context.h>
#include <pdi/logger.h>

#include "release_operation.h"

namespace set_value {

Release_operation::Release_operation(PDI::Logger& logger, PC_tree_t release_value_node)
{
	size_t list_size = PDI::len(release_value_node);
	logger.debug("Release operation count: {}", list_size);
	for (int i = 0; i < list_size; i++) {
		std::string data_to_release = PDI::to_string(PC_get(release_value_node, "[%d]", i));
		logger.trace("\t {}: {}", i, data_to_release);
		m_data_to_release.emplace_back(data_to_release);
	}
}

void Release_operation::execute(PDI::Logger& logger, PDI::Context& ctx)
{
	for (auto&& data_to_release: m_data_to_release) {
		logger.trace("Calling {} data release", data_to_release);
		ctx.desc(data_to_release).release();
	}
}

} // namespace set_value
