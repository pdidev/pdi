/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021-2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/datatype.h>
#include <pdi/datatype_template.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

#include "hdf5_wrapper.h"
#include "selection.h"

#include "dataset_op.h"

using PDI::Array_datatype;
using PDI::Config_error;
using PDI::Context;
using PDI::Datatype;
using PDI::Datatype_sptr;
using PDI::Datatype_template_sptr;
using PDI::each;
using PDI::Expression;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Scalar_datatype;
using PDI::System_error;
using PDI::to_string;
using PDI::Tuple_datatype;
using PDI::Type_error;
using PDI::Value_error;
using std::dynamic_pointer_cast;
using std::function;
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
	if (0 > rank) handle_hdf5_err();
	vector<hsize_t> size(rank);
	if (0 > H5Sget_simple_extent_dims(selection, &size[0], NULL)) handle_hdf5_err();
	vector<hsize_t> start(rank);
	vector<hsize_t> subsize(rank);
	if (0 > H5Sget_select_bounds(selection, &start[0], &subsize[0])) handle_hdf5_err();
	transform(start.begin(), start.end(), subsize.begin(), subsize.begin(), [](hsize_t start, hsize_t end) { return end - start + 1; });
	return make_tuple(move(size), move(start), move(subsize));
}

/** Validates that memory space and dataset space number of elements match
 */
void validate_dataspaces(PC_tree_t selectree, hid_t h5_mem_space, hid_t h5_file_space, const std::string dataset_name)
{
	hssize_t n_data_pts = H5Sget_select_npoints(h5_mem_space);
	if (0 > n_data_pts) handle_hdf5_err();

	hssize_t n_file_pts = H5Sget_select_npoints(h5_file_space);
	if (0 > n_file_pts) handle_hdf5_err();

	if (n_data_pts != n_file_pts) {
		vector<hsize_t> pr_size;
		vector<hsize_t> pr_subsize;
		vector<hsize_t> pr_start;

		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_mem_space);
		stringstream mem_desc;
		for (size_t ii = 0; ii < pr_size.size(); ++ii)
			mem_desc << " (" << pr_start[ii] << "-" << (pr_start[ii] + pr_subsize[ii] - 1) << "/0-" << (pr_size[ii] - 1) << ")";

		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_file_space);
		stringstream file_desc;
		for (size_t ii = 0; ii < pr_size.size(); ++ii)
			file_desc << " (" << pr_start[ii] << "-" << (pr_start[ii] + pr_subsize[ii] - 1) << "/0-" << (pr_size[ii] - 1) << ")";


		throw Config_error{selectree, "Incompatible selections while writing `{}': [{} ] -> [{} ]", dataset_name, mem_desc.str(), file_desc.str()};
	}
}

} // namespace

namespace decl_hdf5 {

Dataset_op::Dataset_op(Direction dir, std::string name, PDI::Expression default_when, Collision_policy file_collision_policy)
	: m_collision_policy{file_collision_policy}
	, m_direction{dir}
	, m_dataset{name}
	, m_value{name}
	, m_when{default_when}
{}

Dataset_op::Dataset_op(Direction dir, string name, Expression default_when, PC_tree_t tree, Collision_policy file_collision_policy)
	: Dataset_op{dir, name, default_when, file_collision_policy}
{
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		if (!PC_status(value)) {
			string key = to_string(key_tree);
			if (key == "dataset") {
				m_dataset = to_string(value);
			} else if (key == "when") {
				m_when = to_string(value);
			} else if (key == "communicator") {
#ifdef H5_HAVE_PARALLEL
				m_communicator = to_string(value);
#else
				throw Config_error {value, "Used HDF5 is not parallel. Invalid communicator: `{}'", to_string(value)};
#endif
			} else if (key == "memory_selection") {
				m_memory_selection = value;
			} else if (key == "dataset_selection") {
				m_dataset_selection = value;
			} else if (key == "chunking") {
				m_chunking = value;
			} else if (key == "deflate") {
				m_deflate = value;
			} else if (key == "fletcher") {
				m_fletcher = value;
			} else if (key == "attributes") {
				// pass
			} else if (key == "mpio") {
				if (to_string(value) == "INDEPENDENT") {
					m_mpio = H5FD_MPIO_INDEPENDENT;
				} else if (to_string(value) == "COLLECTIVE") {
					m_mpio = H5FD_MPIO_COLLECTIVE;
				} else {
					throw Config_error{key_tree, "Not valid mpio value: `{}'. Expecting INDEPENDENT or COLLECTIVE.", to_string(value)};
				}
			} else if (key == "collision_policy") {
				m_collision_policy = to_collision_policy(to_string(value));
			} else {
				throw Config_error{key_tree, "Unknown key for HDF5 dataset configuration: `{}'", key};
			}
		}
	});

	// need to know the final dataset expression
	PC_tree_t attribute_tree = PC_get(tree, ".attributes");
	if (!PC_status(attribute_tree)) {
		each(attribute_tree, [&](PC_tree_t attr_key, PC_tree_t attr_value) {
			Attribute_op::Direction attr_dir = Attribute_op::Direction::WRITE;
			if (dir == Direction::READ) {
				attr_dir = Attribute_op::Direction::READ;
			}
			m_attributes.emplace_back(attr_dir, m_dataset, to_string(attr_key), Expression{to_string(attr_value)});
		});
	}
}

void Dataset_op::deflate(Context& ctx, Expression level)
{
	if (m_deflate) {
		ctx.logger().warn("deflate defined at file and dataset level (the dataset deflate setting will be used)");
	} else {
		m_deflate = level;
	}
}

void Dataset_op::fletcher(Context& ctx, Expression value)
{
	if (m_fletcher) {
		ctx.logger().warn("fletcher defined at file and dataset level (the dataset fletcher setting will be used)");
	} else {
		m_fletcher = value;
	}
}

void Dataset_op::execute(Context& ctx, hid_t h5_file, bool use_mpio, const unordered_map<string, Datatype_template_sptr>& dsets)
{
	Raii_hid xfer_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
	if (use_mpio) {
		if (0 > H5Pset_dxpl_mpio(xfer_lst, m_mpio)) {
			handle_hdf5_err();
		}
	}
	if (m_direction == READ) {
		do_read(ctx, h5_file, xfer_lst);
	} else {
		do_write(ctx, h5_file, xfer_lst, dsets);
}

void Dataset_op::do_read(Context& ctx, hid_t h5_file, hid_t read_lst)
{
	string dataset_name = m_dataset.to_string(ctx);
	ctx.logger().trace("Preparing for reading `{}' dataset", dataset_name);

	Ref_w ref = ctx[m_value].ref();
	if (!ref) {
		ctx.logger().warn("Cannot read `{}' dataset: `{}' data not available", dataset_name, m_value);
		return;
	}

	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	ctx.logger().trace("Applying `{}' memory selection", dataset_name);
	m_memory_selection.apply(ctx, h5_mem_space);

	ctx.logger().trace("Opening `{}' dataset", dataset_name);
	Raii_hid h5_set
		= make_raii_hid(H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose, ("Cannot open `" + dataset_name + "' dataset").c_str());

	ctx.logger().trace("Inquiring `{}' dataset dataspace", dataset_name);
	Raii_hid h5_file_space = make_raii_hid(H5Dget_space(h5_set), H5Sclose, ("Cannot inquire `" + dataset_name + "' dataset dataspace").c_str());

	ctx.logger().trace("Applying `{}' dataset selection", dataset_name);
	m_dataset_selection.apply(ctx, h5_file_space, h5_mem_space);

	ctx.logger().trace("Validating `{}' dataset dataspaces selection", dataset_name);
	validate_dataspaces(m_dataset_selection.selection_tree(), h5_mem_space, h5_file_space, dataset_name);

	ctx.logger().trace("Reading `{}' dataset", dataset_name);
	if (0 > H5Dread(h5_set, h5_mem_type, h5_mem_space, h5_file_space, read_lst, ref)) handle_hdf5_err();

	for (auto&& attr: m_attributes) {
		attr.execute(ctx, h5_file);
	}
	ctx.logger().trace("`{}' dataset read finished", dataset_name);
}

hid_t Dataset_op::dataset_creation_plist(Context& ctx, const Datatype* dataset_type, const string& dataset_name)
{
	hid_t dset_plist = H5Pcreate(H5P_DATASET_CREATE);
	// chunking
	Ref_r chunking_ref;
	try {
		chunking_ref = dataset_type->attribute("decl_hdf5.chunking").to_ref(ctx);
		ctx.logger().trace("Getting `{}' dataset chunking from type attribute", dataset_name);
	} catch (const Type_error& e) {
		// no chunking attribute, check dataset option
		if (m_chunking) {
			ctx.logger().trace("Getting `{}' dataset chunking from dataset operation", dataset_name);
			chunking_ref = m_chunking.to_ref(ctx);
		}
	}
	if (chunking_ref) {
		ctx.logger().trace("Setting `{}' dataset chunking:", dataset_name);
		vector<hsize_t> sizes;
		Datatype_sptr ref_type = chunking_ref.type();
		if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(ref_type)) {
			sizes.emplace_back(chunking_ref.scalar_value<size_t>());
		} else if (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(ref_type)) {
			for (size_t i = 0; i < array_type->size(); i++) {
				sizes.emplace_back(Ref_r{chunking_ref[i]}.scalar_value<hsize_t>());
			}
		} else if (auto&& tuple_type = dynamic_pointer_cast<const Tuple_datatype>(ref_type)) {
			for (size_t i = 0; i < tuple_type->size(); i++) {
				sizes.emplace_back(Ref_r{chunking_ref[i]}.scalar_value<hsize_t>());
			}
		} else {
			throw Type_error{"Chunking must be a scalar, an array or a tuple"};
		}

		if (0 > H5Pset_chunk(dset_plist, sizes.size(), sizes.data())) {
			handle_hdf5_err(fmt::format("Cannot set `{}' dataset chunking", dataset_name).c_str());
		}
	}

	// deflate
	unsigned int deflate_level = -1;
	try {
		deflate_level = dataset_type->attribute("decl_hdf5.deflate").to_long(ctx);
		ctx.logger().trace("Getting `{}' dataset deflate from type attribute", dataset_name);
	} catch (const Type_error& e) {
		// no deflate attribute, check dataset option
		if (m_deflate) {
			ctx.logger().trace("Getting `{}' dataset deflate from dataset operation", dataset_name);
			deflate_level = m_deflate.to_long(ctx);
		}
	}
	if (deflate_level != -1) {
		ctx.logger().trace("Setting `{}' dataset deflate to {}", dataset_name, deflate_level);
		if (0 > H5Pset_deflate(dset_plist, deflate_level)) {
			handle_hdf5_err(fmt::format("Cannot set `{}' dataset deflate", dataset_name).c_str());
		}
	}

	// fletcher
	long fletcher = -1;
	try {
		fletcher = dataset_type->attribute("decl_hdf5.fletcher").to_long(ctx);
		ctx.logger().trace("Getting `{}' dataset fletcher from type attribute", dataset_name);
	} catch (const Type_error& e) {
		// no fletcher attribute, check dataset option
		if (m_fletcher) {
			ctx.logger().trace("Getting `{}' dataset fletcher from dataset operation", dataset_name);
			fletcher = m_fletcher.to_long(ctx);
		}
	}
	if (fletcher != -1) {
		ctx.logger().trace("Setting `{}' dataset fletcher", dataset_name);
		if (0 > H5Pset_fletcher32(dset_plist)) {
			handle_hdf5_err(fmt::format("Cannot set `{}' dataset fletcher", dataset_name).c_str());
		}
	}

		return dset_plist;
	}
}

void Dataset_op::do_write(Context& ctx, hid_t h5_file, hid_t write_lst, const unordered_map<string, Datatype_template_sptr>& dsets)
{
	string dataset_name = m_dataset.to_string(ctx);
	ctx.logger().trace("Preparing for writing `{}' dataset", dataset_name);
	Ref_r ref = ctx[m_value].ref();
	if (!ref) {
		ctx.logger().warn("Cannot write `{}' dataset: `{}' data not available", dataset_name, m_value);
		return;
	}

	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	ctx.logger().trace("Applying `{}' memory selection", dataset_name);
	m_memory_selection.apply(ctx, h5_mem_space);

	auto&& dataset_type_iter = dsets.find(dataset_name);
	Datatype_sptr dataset_type;
	Raii_hid h5_file_type, h5_file_space;
	if (dataset_type_iter != dsets.end()) {
		dataset_type = dataset_type_iter->second->evaluate(ctx);
		tie(h5_file_space, h5_file_type) = space(dataset_type);
		ctx.logger().trace("Applying `{}' dataset selection", dataset_name);
		m_dataset_selection.apply(ctx, h5_file_space, h5_mem_space);
	} else {
		if (!m_dataset_selection.size().empty()) {
			throw Config_error{m_dataset_selection.selection_tree(), "Dataset selection is invalid in implicit dataset `{}'", dataset_name};
		}
		dataset_type = ref.type();
		tie(h5_file_space, h5_file_type) = space(dataset_type, true);
	}

	ctx.logger().trace("Validating `{}' dataset dataspaces selection", dataset_name);
	validate_dataspaces(m_dataset_selection.selection_tree(), h5_mem_space, h5_file_space, dataset_name);

	Raii_hid set_lst = make_raii_hid(H5Pcreate(H5P_LINK_CREATE), H5Pclose);
	if (0 > H5Pset_create_intermediate_group(set_lst, 1)) handle_hdf5_err();

	ctx.logger().trace("Opening `{}' dataset", dataset_name);
	hid_t h5_set_raw = H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT);
	Raii_hid dset_plist = make_raii_hid(dataset_creation_plist(ctx, dataset_type.get(), dataset_name), H5Pclose);
	if (0 > h5_set_raw) {
		ctx.logger().trace("Cannot open `{}' dataset, creating", dataset_name);
		h5_set_raw = H5Dcreate2(h5_file, dataset_name.c_str(), h5_file_type, h5_file_space, set_lst, dset_plist, H5P_DEFAULT);
	} else {
		// Dataset exists -> collision
		function<void(const char*, const std::string&)> notify = [&](const char* message, const std::string& filename) {
			ctx.logger().trace("Dataset collision `{}': {}", filename, message);
		};
		if (m_collision_policy & Collision_policy::WARNING) {
			notify = [&](const char* message, const std::string& filename) {
				ctx.logger().warn("Dataset collision `{}': {}", filename, message);
			};
		}

		if (m_collision_policy & Collision_policy::SKIP) {
			notify("Skipping", dataset_name);
			H5Dclose(h5_set_raw);
			return;
		} else if (m_collision_policy == Collision_policy::REPLACE) {
			notify("Deleting old dataset and creating a new one", dataset_name);
			H5Dclose(h5_set_raw);
			if (0 > H5Ldelete(h5_file, dataset_name.c_str(), H5P_DEFAULT)) handle_hdf5_err();
			;
			h5_set_raw = H5Dcreate2(h5_file, dataset_name.c_str(), h5_file_type, h5_file_space, set_lst, dset_plist, H5P_DEFAULT);
			if (h5_set_raw < 0) {
				throw System_error{"Dataset collision `{}': Cannot create a dataset after deleting old one", dataset_name};
			}
		} else if (m_collision_policy == Collision_policy::ERROR) {
			H5Dclose(h5_set_raw);
			throw System_error{"Dataset collision `{}': Dataset already exists", dataset_name};
		} else {
			// m_collision_policy & Collision_policy::WRITE_INTO == 1
			notify("Writing into exisiting dataset", dataset_name);
		}
	}
	Raii_hid h5_set = make_raii_hid(h5_set_raw, H5Dclose);

	ctx.logger().trace("Writing `{}' dataset", dataset_name);
	if (0 > H5Dwrite(h5_set, h5_mem_type, h5_mem_space, h5_file_space, write_lst, ref)) handle_hdf5_err();

	for (auto&& attr: m_attributes) {
		attr.execute(ctx, h5_file);
	}
	ctx.logger().trace("`{}' dataset write finished", dataset_name);
}

} // namespace decl_hdf5
