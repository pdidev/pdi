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

#include <utility>

#include <spdlog/spdlog.h>

#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/ref_any.h>

#include "hdf5_wrapper.h"

#include "file_op.h"

using PDI::Context;
using PDI::each;
using PDI::Error;
using PDI::Expression;
using PDI::opt_each;
using PDI::Ref_r;
using PDI::to_string;
using std::move;
using std::string;
using std::vector;

namespace decl_hdf5 {

vector<File_op> File_op::parse(Context& ctx, PC_tree_t tree)
{
	// pass 0: mandatory parameters
	
	File_op template_op{to_string(PC_get(tree, ".file"))};
	
	
	// pass 1: file-level optional values
	
	Expression default_when = 1L;
	each(tree, [&](PC_tree_t key_tree, PC_tree_t value) {
		string key = to_string(key_tree);
		if ( key == "file" ) {
			// already read in pass 0
		} else if ( key == "on_event" ) {
			opt_each(value, [&](PC_tree_t event_tree) {
				template_op.m_event.emplace_back(to_string(event_tree));
			});
		} else if ( key == "when" ) {
			default_when = to_string(value);
		} else if ( key == "communicator" ) {
#ifdef H5_HAVE_PARALLEL
			template_op.m_communicator = to_string(value);
#else
			throw Error {PDI_ERR_CONFIG, "Used HDF5 is not parallel. Invalid communicator: `{}'", to_string(value)};
#endif
		} else if ( key == "datasets" ) {
			each(value, [&](PC_tree_t dset_name, PC_tree_t dset_type) {
				template_op.m_datasets.emplace(to_string(dset_name), ctx.datatype(dset_type));
			});
		} else if ( key == "write" ) {
			// will read in pass 2
		} else if ( key == "read" ) {
			// will read in pass 2
		} else {
			throw Error{PDI_ERR_CONFIG, "Unknown key in HDF5 file configuration: `{}'", key};
		}
	});
	
	
	// pass 2 read & writes
	
	vector<Dataset_op> dset_ops;
	
	PC_tree_t read_tree = PC_get(tree, ".read");
	if ( !PC_status(PC_get(read_tree, "[0]")) ) { // it's a list of names only
		each(read_tree, [&](PC_tree_t tree) {
			dset_ops.emplace_back(Dataset_op::READ, to_string(tree), default_when);
		});
	} else if ( !PC_status(read_tree) ) { // it's a name:{config...} mapping
		each(read_tree, [&](PC_tree_t name, PC_tree_t config) {
			opt_each(config, [&](PC_tree_t value) { // each config is an independant op
				dset_ops.emplace_back(Dataset_op::READ, to_string(name), default_when, value);
			});
		});
	}
	PC_tree_t write_tree = PC_get(tree, ".write");
	if ( !PC_status(PC_get(write_tree, "[0]")) ) { // it's a list of names only
		each(write_tree, [&](PC_tree_t tree) {
			dset_ops.emplace_back(Dataset_op::WRITE, to_string(tree), default_when);
		});
	} else if ( !PC_status(write_tree) ) { // it's a name:{config...} mapping
		each(write_tree, [&](PC_tree_t name, PC_tree_t config) {
			opt_each(config, [&](PC_tree_t value) { // each config is an independant op
				dset_ops.emplace_back(Dataset_op::WRITE, to_string(name), default_when, value);
			});
		});
	}
	
	
	// final pass to build the result
	
	vector<File_op> result;
	
	if ( template_op.m_event.empty() ) { // we are in the data triggered case
		for (auto&& one_dset_op: dset_ops) {
			File_op one_op = template_op;
#ifdef H5_HAVE_PARALLEL
			if ( one_dset_op.communicator() ) {
				one_op.m_communicator = one_dset_op.communicator();
			}
#endif
			one_op.m_dset_ops.emplace_back(one_dset_op);
			result.emplace_back(move(one_op));
		}
	} else {
#ifdef H5_HAVE_PARALLEL
		// check the dataset ops don't have specific communicators set
		for (auto&& one_dset_op: dset_ops) {
			if ( one_dset_op.communicator() ) {
				throw Error{PDI_ERR_CONFIG, "Communicator can not be set at the dataset level for event triggered I/O"};
			}
		}
#endif
		template_op.m_dset_ops = move(dset_ops);
		result.emplace_back(move(template_op));
	}
	
	return result;
}

File_op::File_op(const File_op& other):
	m_file{other.m_file},
	m_event{other.m_event},
#ifdef H5_HAVE_PARALLEL
	m_communicator {other.m_communicator},
#endif
	m_dset_ops {other.m_dset_ops}
{
	for (auto&& dataset: other.m_datasets) {
		m_datasets.emplace(dataset.first, dataset.second->clone());
	}
}

File_op::File_op(Expression&& file):
	m_file{move(file)}
{}

void File_op::execute(Context& ctx)
{
	// first gather the ops we actually want to do
	std::vector<Dataset_op> dset_reads;
	std::vector<Dataset_op> dset_writes;
	
	for ( auto&& one_dset_op: m_dset_ops ) {
		try {
			if ( one_dset_op.when().to_long(ctx) ) {
				if ( one_dset_op.direction() == Dataset_op::READ ) {
					dset_reads.push_back(one_dset_op);
				} else {
					dset_writes.push_back(one_dset_op);
				}
			}
		} catch (const Error& e) {
			ctx.logger()->warn("Unable to evaluate when close while executing transfer for {}: `{}'", one_dset_op.value(), e.what());
		}
	}
	
	// nothing to do if no op is selected
	if ( dset_reads.empty() && dset_writes.empty() ) return;
	
	Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid xfer_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
	MPI_Comm comm = MPI_COMM_SELF;
	if (communicator()) {
		comm = *(static_cast<const MPI_Comm*>(Ref_r{communicator().to_ref(ctx)}.get()));
	}
	if ( comm != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, comm, MPI_INFO_NULL) ) handle_hdf5_err();
		if ( 0>H5Pset_dxpl_mpio(xfer_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	hid_t h5_file_raw = -1;
	if ( !dset_writes.empty() && !dset_reads.empty() ) {
		h5_file_raw = H5Fopen(m_file.to_string(ctx).c_str(), H5F_ACC_RDWR, file_lst);
	} else if ( !dset_writes.empty() ) {
		h5_file_raw = H5Fopen(m_file.to_string(ctx).c_str(), H5F_ACC_RDWR, file_lst);
		if (0>h5_file_raw) {
			h5_file_raw = H5Fcreate(m_file.to_string(ctx).c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
		}
	} else {
		h5_file_raw = H5Fopen(m_file.to_string(ctx).c_str(), H5F_ACC_RDONLY, file_lst);
	}
	Raii_hid h5_file = make_raii_hid(h5_file_raw, H5Fclose);
	
	for (auto&& one_dset_op: dset_writes ) {
		one_dset_op.execute(ctx, h5_file, xfer_lst, m_datasets);
	}
	for (auto&& one_dset_op: dset_reads ) {
		one_dset_op.execute(ctx, h5_file, xfer_lst, m_datasets);
	}
}

} // namespace decl_hdf5
