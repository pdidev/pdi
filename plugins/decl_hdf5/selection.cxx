/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
#include <mpi.h>
#endif

#include <algorithm>
#include <string>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>

#include "hdf5_wrapper.h"

#include "selection.h"

using PDI::Config_error;
using PDI::Context;
using PDI::each;
using PDI::opt_each;
using PDI::to_string;
using std::string;
using std::transform;
using std::vector;

namespace decl_hdf5 {

Selection::Selection(PC_tree_t tree)
	: m_selection_tree{tree}
{
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		string key = to_string(key_tree);
		if (key == "size") {
			PDI::opt_one_or_each(value, [&](PC_tree_t size) { m_size.emplace_back(to_string(size)); });
		} else if (key == "start") {
			PDI::opt_one_or_each(value, [&](PC_tree_t start) { m_start.emplace_back(to_string(start)); });
		} else {
			throw Config_error{key_tree, "Invalid configuration key in selection: `{}'", key};
		}
	});
}

void Selection::apply(Context& ctx, hid_t h5_space, hid_t dflt_space) const
{
	int rank = H5Sget_simple_extent_ndims(h5_space);
	if (0 > rank) handle_hdf5_err();
	if (0 == rank) return;

	vector<hsize_t> h5_subsize(rank);
	vector<hsize_t> h5_start(rank);
	if (0 > H5Sget_select_bounds(h5_space, &h5_start[0], &h5_subsize[0])) handle_hdf5_err();
	transform(h5_start.begin(), h5_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end) { return end - start + 1; });

	if (!m_size.empty()) {
		if (m_size.size() != static_cast<size_t>(rank)) {
			throw Config_error{PC_get(m_selection_tree, ".size"), "Invalid selection: {} selection in {} array", m_size.size(), rank};
		}
		for (int size_id = 0; size_id < rank; ++size_id) {
			h5_subsize[size_id] = m_size[size_id].to_long(ctx);
		}
	} else if (dflt_space != -1) {
		int dflt_rank = H5Sget_simple_extent_ndims(dflt_space);
		if (dflt_rank != rank) {
			if (H5Sget_select_npoints(h5_space) == H5Sget_select_npoints(dflt_space)) {
				// if ranks differ but number of elements are the same, select whole dataset
			} else {
				throw Config_error{m_selection_tree, "Invalid default selection: {} selection in {} array", dflt_rank, rank};
			}
		} else {
			// ranks match, get memory size selection as dataset size selection
			vector<hsize_t> dflt_start(rank);
			if (0 > H5Sget_select_bounds(dflt_space, &dflt_start[0], &h5_subsize[0])) handle_hdf5_err();
			transform(dflt_start.begin(), dflt_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end) {
				return end - start + 1;
			});
		}
	}

	if (!m_start.empty()) {
		if (m_start.size() != static_cast<size_t>(rank)) {
			throw Config_error{PC_get(m_selection_tree, ".start"), "Invalid selection: {} start in {} array", m_size.size(), rank};
		}
		for (int size_id = 0; size_id < rank; ++size_id) {
			h5_start[size_id] += m_start[size_id].to_long(ctx);
		}
	}

	if (0 > H5Sselect_hyperslab(h5_space, H5S_SELECT_SET, &h5_start[0], NULL, &h5_subsize[0], NULL)) handle_hdf5_err();
}

} // namespace decl_hdf5
