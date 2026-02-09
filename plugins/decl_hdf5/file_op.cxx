/*******************************************************************************
 * Copyright (C) 2015-2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <H5FDsubfiling.h>
#endif

#include <memory>
#include <unordered_map>
#include <utility>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "hdf5_wrapper.h"

#include "file_op.h"

using PDI::Config_error;
using PDI::Context;
using PDI::each;
using PDI::Error;
using PDI::Expression;
using PDI::opt_each;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::System_error;
using PDI::to_string;
using std::function;
using std::move;
using std::string;
using std::unique_ptr;
using std::unordered_map;
using std::vector;

namespace decl_hdf5 {

vector<File_op> File_op::parse(Context& ctx, PC_tree_t tree)
{
	// pass 0: mandatory parameters

	File_op template_op{to_string(PC_get(tree, ".file"))};


	// pass 1: file-level optional values
	Expression deflate;
	Expression fletcher;
	Expression default_when = 1L;
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		string key = to_string(key_tree);
		if (key == "file") {
			// already read in pass 0
		} else if (key == "collision_policy") {
			template_op.m_collision_policy = to_collision_policy(to_string(value));
		} else if (key == "on_event") {
			opt_each(value, [&](PC_tree_t event_tree) { template_op.m_event.emplace_back(to_string(event_tree)); });
		} else if (key == "when") {
			default_when = to_string(value);
		} else if (key == "communicator") {
#ifdef H5_HAVE_PARALLEL
			template_op.m_communicator = to_string(value);
#else
			throw Config_error {key_tree, "Used HDF5 is not parallel. Invalid communicator: `{}'", to_string(value)};
#endif
		} else if (key == "datasets") {
			each(value, [&](PC_tree_t dset_name, PC_tree_t dset_type) {
				std::string dset_name_value = to_string(dset_name);
				std::regex dset_regex(dset_name_value, std::regex::ECMAScript);
				if (dset_type.node && dset_name.node) {
					template_op.m_datasets.emplace_back(
						dset_name_value,
						dset_name.node->start_mark.line,
						dset_type.node->end_mark.line,
						dset_regex,
						ctx.datatype(dset_type)
					);
				} else {
					Config_error{key_tree, "Error in the definiion of dataset `{}' in datasets section.", dset_name_value};
				}
			});
		} else if (key == "deflate") {
			deflate = value;
		} else if (key == "fletcher") {
			fletcher = value;
		} else if (key == "subfiling") {
			template_op.m_subfiling = to_string(value);
		} else if (key == "write") {
			// will read in pass 2
		} else if (key == "read") {
			// will read in pass 2
		} else if (key == "logging") {
			// pass
		} else {
			throw Config_error{key_tree, "Unknown key in HDF5 file configuration: `{}'", key};
		}
	});


	// pass 2 read & writes

	vector<Dataset_op> dset_ops;
	vector<Attribute_op> attr_ops;
	unordered_map<string, Expression> dset_size_ops;

	PC_tree_t read_tree = PC_get(tree, ".read");
	if (!PC_status(PC_get(read_tree, "[0]"))) { // it's a list of names only
		each(read_tree, [&](PC_tree_t tree) {
			string dset_string = to_string(tree);
			if (dset_string.find("#") == string::npos) {
				dset_ops.emplace_back(Dataset_op::READ, to_string(tree), default_when);
			} else {
				attr_ops.emplace_back(Attribute_op::READ, tree, default_when);
			}
		});
	} else if (!PC_status(read_tree)) { // it's a name:{config...} mapping
		each(read_tree, [&](PC_tree_t name, PC_tree_t config) {
			opt_each(config, [&](PC_tree_t value) { // each config is an independant op
				if (!PC_status(PC_get(value, ".attribute"))) {
					attr_ops.emplace_back(Attribute_op::READ, to_string(name), default_when, value);
				} else if (!PC_status(PC_get(value, ".size_of"))) {
					dset_size_ops.emplace(to_string(name), to_string(PC_get(value, ".size_of")));
				} else {
					dset_ops.emplace_back(Dataset_op::READ, to_string(name), default_when, value);
				}
			});
		});
	}
	PC_tree_t write_tree = PC_get(tree, ".write");
	if (!PC_status(PC_get(write_tree, "[0]"))) { // it's a list of names only
		each(write_tree, [&](PC_tree_t tree) {
			string dset_string = to_string(tree);
			if (dset_string.find("#") == string::npos) {
				dset_ops.emplace_back(Dataset_op::WRITE, to_string(tree), default_when, template_op.m_collision_policy);
				if (deflate) {
					dset_ops.back().deflate(ctx, deflate.to_long(ctx));
				}
				if (fletcher) {
					dset_ops.back().fletcher(ctx, fletcher.to_long(ctx));
				}
			} else {
				attr_ops.emplace_back(Attribute_op::WRITE, tree, default_when);
			}
		});
	} else if (!PC_status(write_tree)) { // it's a name:{config...} mapping
		each(write_tree, [&](PC_tree_t name, PC_tree_t config) {
			if (!PC_status(PC_get(config, ".attribute"))) {
				opt_each(config, [&](PC_tree_t value) { // each config is an independant op
					attr_ops.emplace_back(Attribute_op::WRITE, to_string(name), default_when, value);
				});
			} else {
				opt_each(config, [&](PC_tree_t value) { // each config is an independant op
					dset_ops.emplace_back(Dataset_op::WRITE, to_string(name), default_when, value, template_op.m_collision_policy);
					if (deflate) {
						dset_ops.back().deflate(ctx, deflate.to_long(ctx));
					}
					if (fletcher) {
						dset_ops.back().fletcher(ctx, fletcher.to_long(ctx));
					}
				});
			}
		});
	}


	// final pass to build the result

	vector<File_op> result;

	if (template_op.m_event.empty()) { // we are in the data triggered case
		for (auto&& one_dset_op: dset_ops) {
			File_op one_op = template_op;
#ifdef H5_HAVE_PARALLEL
			if (one_dset_op.communicator()) {
				one_op.m_communicator = one_dset_op.communicator();
			}
#endif
			one_op.m_dset_ops.emplace_back(one_dset_op);
			result.emplace_back(move(one_op));
		}
		for (auto&& one_attr_op: attr_ops) {
			File_op one_op = template_op;
			one_op.m_attr_ops.emplace_back(one_attr_op);
			result.emplace_back(move(one_op));
		}
		for (auto&& one_dset_size_op: dset_size_ops) {
			File_op one_op = template_op;
			one_op.m_dset_size_ops.emplace(one_dset_size_op.first, one_dset_size_op.second);
			result.emplace_back(move(one_op));
		}
	} else {
#ifdef H5_HAVE_PARALLEL
		// check the dataset ops don't have specific communicators set
		for (auto&& one_dset_op: dset_ops) {
			if (one_dset_op.communicator()) {
				throw Config_error{tree, "Communicator can not be set at the dataset level for event triggered I/O"};
			}
		}
#endif
		template_op.m_dset_ops = move(dset_ops);
		template_op.m_attr_ops = move(attr_ops);
		template_op.m_dset_size_ops = move(dset_size_ops);
		result.emplace_back(move(template_op));
	}

	return result;
}

File_op::File_op(const File_op& other)
	: m_collision_policy{other.m_collision_policy}
	, m_file{other.m_file}
	, m_event{other.m_event}
	,
#ifdef H5_HAVE_PARALLEL
	m_communicator{other.m_communicator}
	, m_subfiling{other.m_subfiling}
	,
#endif
	m_dset_ops{other.m_dset_ops}
	, m_attr_ops{other.m_attr_ops}
	, m_dset_size_ops{other.m_dset_size_ops}
{
	for (auto&& dataset: other.m_datasets) {
		m_datasets.emplace_back(dataset);
	}
}

File_op::File_op(Expression&& file, Collision_policy collision_policy)
	: m_collision_policy{collision_policy}
	, m_file{move(file)}
{}

void File_op::execute(Context& ctx)
{
	// first gather the ops we actually want to do
	vector<Dataset_op> dset_reads;
	vector<Dataset_op> dset_writes;

	for (auto&& one_dset_op: m_dset_ops) {
		try {
			if (one_dset_op.when().to_long(ctx)) {
				if (one_dset_op.direction() == Dataset_op::READ) {
					dset_reads.push_back(one_dset_op);
				} else {
					dset_writes.push_back(one_dset_op);
				}
			}
		} catch (const Error& e) {
			ctx.logger().warn("Unable to evaluate when close while executing transfer for {}: `{}'", one_dset_op.value(), e.what());
		}
	}

	vector<Attribute_op> attr_reads;
	vector<Attribute_op> attr_writes;

	for (auto&& one_attr_op: m_attr_ops) {
		try {
			if (one_attr_op.when().to_long(ctx)) {
				if (one_attr_op.direction() == Attribute_op::READ) {
					attr_reads.push_back(one_attr_op);
				} else {
					attr_writes.push_back(one_attr_op);
				}
			}
		} catch (const Error& e) {
			ctx.logger().warn("Unable to evaluate when close while executing transfer for {}: `{}'", one_attr_op.name(), e.what());
		}
	}
	// nothing to do if no op is selected
	if (dset_reads.empty() && dset_writes.empty() && attr_reads.empty() && attr_writes.empty() && m_dset_size_ops.empty()) return;
	std::string filename = m_file.to_string(ctx);

	Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	bool use_mpio = false;
#ifdef H5_HAVE_PARALLEL
	MPI_Comm comm = MPI_COMM_SELF;
	if (communicator()) {
		comm = *(static_cast<const MPI_Comm*>(Ref_r{communicator().to_ref(ctx)}.get()));
	}
	if (comm != MPI_COMM_SELF) {
		if (0 > H5Pset_fapl_mpio(file_lst, comm, MPI_INFO_NULL)) handle_hdf5_err();
		use_mpio = true;
		ctx.logger().debug("Opening `{}' file in parallel mode", filename);

		if (subfiling().to_long(ctx)) {
			ctx.logger().warn("HDF5 subfiling enabled for file {}", filename);

			H5FD_subfiling_config_t subf_config;

			// TODO: make choices for ioc_selection, and for stripe_size
			H5Pget_fapl_subfiling(file_lst, &subf_config);
			subf_config.shared_cfg.ioc_selection = SELECT_IOC_EVERY_NTH_RANK;
			setenv("H5FD_SUBFILING_IOC_SELECTION_CRITERIA", "1", 1);
			subf_config.shared_cfg.stripe_size = 1024;

			H5Pset_fapl_subfiling(file_lst, &subf_config);
		}
	}
#endif

	hid_t h5_file_raw = -1;
	if ((!dset_writes.empty() || !attr_writes.empty()) && (!dset_reads.empty() || !attr_reads.empty())) {
		ctx.logger().trace("Opening `{}' file to read and write", filename);
		h5_file_raw = H5Fopen(m_file.to_string(ctx).c_str(), H5F_ACC_RDWR, file_lst);
	} else if (!dset_writes.empty() || !attr_writes.empty()) {
		ctx.logger().trace("Opening `{}' file to write", filename);
		h5_file_raw = H5Fopen(m_file.to_string(ctx).c_str(), H5F_ACC_RDWR, file_lst);
		if (0 > h5_file_raw) {
			ctx.logger().trace("Cannot open `{}' file, creating new file", filename);
			h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
		} else {
			// File exists -> collision
			function<void(const char*, const std::string&)> notify = [&](const char* message, const std::string& filename) {
				ctx.logger().trace("File `{}' already exists: {}", filename, message);
			};
			if (m_collision_policy & Collision_policy::WARNING) {
				notify = [&](const char* message, const std::string& filename) {
					ctx.logger().warn("File `{}' already exists: {}", filename, message);
				};
			}

			if (m_collision_policy & Collision_policy::SKIP) {
				notify("Skipping", filename);
				H5Fclose(h5_file_raw);
				return;
			} else if (m_collision_policy & Collision_policy::REPLACE) {
				notify("Deleting old file and creating a new one", filename);
				H5Fclose(h5_file_raw);
				h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, file_lst);
			} else if (m_collision_policy & Collision_policy::ERROR) {
				H5Fclose(h5_file_raw);
				throw System_error{"Filename collision `{}': File already exists", filename};
			} else {
				// m_collision_policy & Collision_policy::WRITE_INTO == 1
				notify("Writing into existing file", filename);
			}
		}
	} else {
		ctx.logger().trace("Opening `{}' file to read", filename);
		h5_file_raw = H5Fopen(filename.c_str(), H5F_ACC_RDONLY, file_lst);
	}
	Raii_hid h5_file = make_raii_hid(h5_file_raw, H5Fclose, ("Cannot open `" + filename + "' file").c_str());

	for (auto&& one_dset_op: dset_writes) {
		one_dset_op.execute(ctx, h5_file, use_mpio, m_datasets);
	}
	for (auto&& one_dset_op: dset_reads) {
		one_dset_op.execute(ctx, h5_file, use_mpio, m_datasets);
	}
	for (auto&& one_attr_op: attr_writes) {
		one_attr_op.execute(ctx, h5_file);
	}
	for (auto&& one_attr_op: attr_reads) {
		one_attr_op.execute(ctx, h5_file);
	}
	for (auto&& one_dset_size_op: m_dset_size_ops) {
		string dataset_name = one_dset_size_op.second.to_string(ctx);
		ctx.logger().trace("Getting size of `{}' dataset", dataset_name);
		Ref_w ref_w = ctx[one_dset_size_op.first].ref();
		if (!ref_w) {
			ctx.logger().warn("Data `{}' to read size of dataset not available", one_dset_size_op.first);
			return;
		}

		ctx.logger().trace("Opening `{}' dataset", dataset_name);
		Raii_hid dset_id = make_raii_hid(H5Dopen(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose);
		Raii_hid dset_space_id = make_raii_hid(H5Dget_space(dset_id), H5Sclose);
		if (H5Sis_simple(dset_space_id)) {
			int ndims = H5Sget_simple_extent_ndims(dset_space_id);
			unique_ptr<hsize_t[]> dims{new hsize_t[ndims]};
			H5Sget_simple_extent_dims(dset_space_id, dims.get(), NULL);
			for (int i = 0; i < ndims; i++) {
				*(static_cast<long*>(ref_w.get()) + i) = dims[i];
			}
		} else {
			// not an array
			*static_cast<long*>(ref_w.get()) = 0L;
		}

		ctx.logger().trace("Getting size of `{}' dataset finished", dataset_name);
	}
	ctx.logger().trace("All operations done in `{}'. Closing the file.", filename);
}

} // namespace decl_hdf5
