/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <algorithm>
#include <iostream>
#include <string>

#include <pdi/context.h>
#include <pdi/error.h>

#include "subfiling.h"

using PDI::each;
using PDI::opt_each;
using PDI::Spectree_error;
using PDI::to_string;
using std::string;

namespace decl_hdf5 {

Subfiling::Subfiling(PC_tree_t sf_tree)
{
	if (PDI::is_scalar(sf_tree)) {
		m_sf_count = PDI::to_string(sf_tree);
	} else if (PDI::is_map(sf_tree)) {
		PDI::each(sf_tree, [&](PC_tree_t key_tree, PC_tree_t value) {
			string key = to_string(key_tree);
			if (key == "policy") {
				m_sf_policy = value;
			} else if (key == "count") {
				m_sf_count = value;
			} else if (key == "stripe_size") {
				m_sf_stripe_size = value;
			} else {
				throw Spectree_error{key_tree, "Invalid configuration key in subfiling: `{}'", key};
			}
		});
	} else {
		throw Spectree_error{sf_tree, "subfiling node is not parsed correctly"};
	}
}

} // namespace decl_hdf5
