// SPDX-FileCopyrightText: 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <pdi/context.h>

#include "operation.h"

namespace set_value {

Operation::Operation(PDI::Context& ctx)
	: m_ctx{ctx}
{}

PDI::Context& Operation::context()
{
	return m_ctx;
}


} // namespace set_value
