/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include "config.h"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include "pdi.h"
#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype_template_alias.h"
#include "pdi/error.h"
#include "pdi/expression.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/scalar_datatype.h"
#include "pdi/tuple_datatype.h"

#include "pdi/datatype_expression.h"

namespace PDI {

using std::exception;
using std::make_shared;
using std::max;
using std::move;
using std::string;
using std::transform;
using std::unique_ptr;
using std::vector;

Datatype_expression::Datatype_expression(PC_tree_t datatype_tree)
{
	if (PC_status(datatype_tree)) {
		throw Config_error{datatype_tree, "Invalid (empty) datatype"};
	}
	if (is_scalar(datatype_tree)) {
		m_referenced_datatype_name = to_string(datatype_tree);
		return;
	}
	PC_tree_t type_tree = PC_get(datatype_tree, ".type");
	if (PC_status(type_tree)) {
		throw Config_error{datatype_tree, "Missing `.type` in datatype"};
	}
	each(datatype_tree, [](PC_tree_t key, PC_tree_t value){
		
	});
}


} // namespace PDI
