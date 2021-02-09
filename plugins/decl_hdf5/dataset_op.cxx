/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <hdf5.h>
#ifdef H5_HAVE_PARALLEL
	#include <mpi.h>
#endif

#include <algorithm>
#include <sstream>
#include <tuple>
#include <vector>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/datatype_template.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "hdf5_wrapper.h"
#include "selection.h"

#include "dataset_op.h"

using PDI::Context;
using PDI::each;
using PDI::Datatype;
using PDI::Datatype_template_uptr;
using PDI::Datatype_uptr;
using PDI::Config_error;
using PDI::Expression;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::to_string;
using std::string;
using std::stringstream;
using std::tie;
using std::transform;
using std::tuple;
using std::unordered_map;
using std::vector;

namespace {

using namespace decl_hdf5;

tuple<vector<hsize_t>, vector<hsize_t>, vector<hsize_t>> get_selection(hid_t selection)
{
	int rank = H5Sget_simple_extent_ndims(selection);
	if ( 0>rank ) handle_hdf5_err();
	vector<hsize_t> size(rank);
	if ( 0>H5Sget_simple_extent_dims(selection, &size[0], NULL) ) handle_hdf5_err();
	vector<hsize_t> start(rank);
	vector<hsize_t> subsize(rank);
	if ( 0>H5Sget_select_bounds(selection, &start[0], &subsize[0] ) ) handle_hdf5_err();
	transform( start.begin(), start.end(), subsize.begin(), subsize.begin(), [](hsize_t start, hsize_t end) {
		return end-start+1;
	});
	return make_tuple(move(size), move(start), move(subsize));
}

} // namespace <anonymous>

namespace decl_hdf5 {

Dataset_op::Dataset_op(Direction dir, string name, Expression default_when, PC_tree_t tree):
	Dataset_op{dir, name, default_when}
{
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		if (!PC_status(value)) {
			string key = to_string(key_tree);
			if ( key == "dataset" ) {
				m_dataset= to_string(value);
			} else if ( key == "when" ) {
				m_when = to_string(value);
			} else if ( key == "communicator" ) {
#ifdef H5_HAVE_PARALLEL
				m_communicator = to_string(value);
#else
				throw Config_error {"Used HDF5 is not parallel. Invalid communicator: `{}'", to_string(value)};
#endif
			} else if ( key == "memory_selection" ) {
				m_memory_selection = value;
			} else if ( key == "dataset_selection" ) {
				m_dataset_selection = value;
			} else {
				throw Config_error{"Unknown key for HDF5 dataset configuration: `{}'", key};
			}
		}
	});
}

void Dataset_op::execute(Context& ctx, hid_t h5_file, hid_t xfer_lst, const unordered_map<string, Datatype_template_uptr>& dsets)
{
	if (m_direction == READ) do_read(ctx, h5_file, xfer_lst);
	else do_write(ctx, h5_file, xfer_lst, dsets);
}

void Dataset_op::do_read(Context& ctx, hid_t h5_file, hid_t read_lst)
{
	Ref_w ref = ctx[m_value].ref();
	if ( !ref ) {
		ctx.logger()->warn("Reference to read not available: `{}'", m_value);
		return;
	}
	
	string dataset_name = m_dataset.to_string(ctx);
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	m_memory_selection.apply(ctx, h5_mem_space);
	
	Raii_hid h5_set = make_raii_hid(H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose);
	
	Raii_hid h5_file_space = make_raii_hid(H5Dget_space(h5_set), H5Sclose);
	
	m_dataset_selection.apply(ctx, h5_file_space, h5_mem_space);
	
	if ( 0>H5Dread(h5_set, h5_mem_type, h5_mem_space, h5_file_space, read_lst, ref) ) handle_hdf5_err();
	
}

void Dataset_op::do_write(Context& ctx, hid_t h5_file, hid_t write_lst, const unordered_map<string, PDI::Datatype_template_uptr>& dsets)
{
	Ref_r ref = ctx[m_value].ref();
	if ( !ref ) {
		ctx.logger()->warn("Reference to write not available: `{}'", m_value);
		return;
	}
	string dataset_name = m_dataset.to_string(ctx);
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	m_memory_selection.apply(ctx, h5_mem_space);
	
	hssize_t n_data_pts = H5Sget_select_npoints(h5_mem_space);
	if ( 0>n_data_pts ) handle_hdf5_err();
	
	auto&& dataset_type_iter = dsets.find(dataset_name);
	Raii_hid h5_file_type, h5_file_space;
	if ( dataset_type_iter != dsets.end() ) {
		tie(h5_file_space, h5_file_type) = space(*dataset_type_iter->second->evaluate(ctx));
		m_dataset_selection.apply(ctx, h5_file_space, h5_mem_space);
	} else {
		if ( !m_dataset_selection.size().empty() ) {
			throw Config_error{"Dataset selection is invalid in implicit dataset `{}'", dataset_name};
		}
		tie(h5_file_space, h5_file_type) = space(ref.type(), true);
	}
	
	
	hssize_t n_file_pts = H5Sget_select_npoints(h5_file_space);
	if ( 0>n_file_pts ) handle_hdf5_err();
	
	if ( n_data_pts != n_file_pts ) {
		vector<hsize_t> pr_size;
		vector<hsize_t> pr_subsize;
		vector<hsize_t> pr_start;
		
		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_mem_space);
		stringstream mem_desc;
		for ( size_t ii=0; ii<pr_size.size(); ++ii) mem_desc << " ("<< pr_start[ii]<<"-"<<(pr_start[ii]+pr_subsize[ii]-1)<<"/0-"<< (pr_size[ii]-1)<<")";
		
		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_file_space);
		stringstream file_desc;
		for ( size_t ii=0; ii< pr_size.size(); ++ii) file_desc << " ("<< pr_start[ii]<<"-"<<(pr_start[ii]+pr_subsize[ii]-1)<<"/0-"<< (pr_size[ii]-1)<<")";
		
		throw Config_error{"Incompatible selections while writing `{}': [{} ] -> [{} ]", dataset_name, mem_desc.str(), file_desc.str()};
	}
	
	Raii_hid set_lst = make_raii_hid(H5Pcreate(H5P_LINK_CREATE), H5Pclose);
	if ( 0>H5Pset_create_intermediate_group(set_lst, 1) ) handle_hdf5_err();
	
	hid_t h5_set_raw = H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT);
	if ( 0 > h5_set_raw ) {
		h5_set_raw = H5Dcreate2(h5_file, dataset_name.c_str(), h5_file_type, h5_file_space, set_lst, H5P_DEFAULT, H5P_DEFAULT);
	}
	Raii_hid h5_set = make_raii_hid(h5_set_raw, H5Dclose);
	
	if ( 0>H5Dwrite(h5_set, h5_mem_type, h5_mem_space, h5_file_space, write_lst, ref) ) handle_hdf5_err();
}

} // namespace decl_hdf5
