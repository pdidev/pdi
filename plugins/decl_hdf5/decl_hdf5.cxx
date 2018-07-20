/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <mpi.h>

#include <algorithm>
#include <iostream>
#include <sstream>
#include <string>
#include <tuple>
#include <utility>
#include <vector>

#include <hdf5.h>
#include <paraconf.h>
#include <spdlog.h>
#include <unistd.h>

#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/expression.h>

#include "decl_hdf5_cfg.h"
#include "file_cfg.h"
#include "raii_hid.h"
#include "selection_cfg.h"
#include "transfer_cfg.h"

namespace {

using PDI::Array_datatype;
using PDI::Context;
using PDI::Datatype;
using PDI::Datatype_uptr;
using PDI::Error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using std::make_tuple;
using std::move;
using std::string;
using std::stringstream;
using std::tie;
using std::transform;
using std::tuple;
using std::vector;

tuple<Raii_hid, Raii_hid> space(const Datatype& type)
{
	int rank = 0;
	vector<hsize_t> h5_size;
	vector<hsize_t> h5_subsize;
	vector<hsize_t> h5_start;
	const Datatype* subtype = &type;
	
	while (auto&& array_type = dynamic_cast<const Array_datatype*>(subtype)) {
		++rank;
		h5_size.emplace_back(array_type->size());
		h5_subsize.emplace_back(array_type->subsize());
		h5_start.emplace_back(array_type->start());
		subtype = &array_type->subtype();
	}
	reverse(h5_size.begin(), h5_size.end());
	reverse(h5_subsize.begin(), h5_subsize.end());
	reverse(h5_start.begin(), h5_start.end());
	auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(subtype);
	if (!scalar_type) throw Error {PDI_ERR_IMPL, "Unexpected type in HDF5"};
	
	hid_t h5_type;
	switch (scalar_type->kind()) {
	case Scalar_kind::UNSIGNED: {
		switch (scalar_type->datasize()) {
		case 1: h5_type = H5T_NATIVE_UINT8; break;
		case 2: h5_type = H5T_NATIVE_UINT16; break;
		case 4: h5_type = H5T_NATIVE_UINT32; break;
		case 8: h5_type = H5T_NATIVE_UINT64; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 signed: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::SIGNED: {
		switch (scalar_type->datasize()) {
		case 1: h5_type = H5T_NATIVE_INT8; break;
		case 2: h5_type = H5T_NATIVE_INT16; break;
		case 4: h5_type = H5T_NATIVE_INT32; break;
		case 8: h5_type = H5T_NATIVE_INT64; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 unsigned: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::FLOAT: {
		switch (scalar_type->datasize()) {
		case 4:  h5_type = H5T_NATIVE_FLOAT; break;
		case 8:  h5_type = H5T_NATIVE_DOUBLE; break;
		case 16: h5_type = H5T_NATIVE_LDOUBLE; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 float: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::ADDRESS: case Scalar_kind::UNKNOWN:
		throw Error {PDI_ERR_TYPE, "Invalid type for HDF5: #%d", scalar_type->kind()};
	}
	
	Raii_hid h5_space = make_raii_hid(H5Screate_simple(rank, &h5_size[0], NULL), H5Sclose);
	
	if (rank) {
		if ( 0>H5Sselect_hyperslab(h5_space, H5S_SELECT_SET, &h5_start[0], NULL, &h5_subsize[0], NULL) ) handle_hdf5_err();
	}
	return make_tuple(move(h5_space), Raii_hid{h5_type, NULL});
}

void select(Context& ctx, hid_t h5_space, const Selection_cfg& select, hid_t dflt_space = -1)
{
	int rank = H5Sget_simple_extent_ndims(h5_space);
	if ( 0>rank ) handle_hdf5_err();
	if ( 0==rank ) return;
	
	vector<hsize_t> h5_subsize(rank);
	vector<hsize_t> h5_start(rank);
	if ( 0>H5Sget_select_bounds(h5_space, &h5_start[0], &h5_subsize[0]) ) handle_hdf5_err();
	transform( h5_start.begin(), h5_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end) {
		return end-start+1;
	});
	
	if ( !select.size().empty() ) {
		if (select.size().size() != rank ) {
			throw Error{PDI_ERR_CONFIG, "Invalid selection: %dd selection in %dd array", select.size().size(), rank};
		}
		for ( int size_id=0; size_id<rank; ++size_id ) {
			h5_subsize[size_id] = select.size()[size_id].to_long(ctx);
		}
	} else if (dflt_space != -1 ) {
		int dflt_rank = H5Sget_simple_extent_ndims(dflt_space);
		if (dflt_rank != rank ) {
			throw Error{PDI_ERR_CONFIG, "Invalid default selection: %dd selection in %dd array", dflt_rank, rank};
		}
		
		vector<hsize_t> dflt_start(rank);
		if ( 0>H5Sget_select_bounds(dflt_space, &dflt_start[0], &h5_subsize[0] ) ) handle_hdf5_err();
		transform( dflt_start.begin(), dflt_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end) {
			return end-start+1;
		});
	}
	
	if ( !select.start().empty() ) {
		if ( select.start().size() != rank ) {
			throw Error{PDI_ERR_CONFIG, "Invalid selection: %dd start in %dd array", select.size().size(), rank};
		}
		for ( int size_id=0; size_id<rank; ++size_id ) {
			h5_start[size_id] += select.start()[size_id].to_long(ctx);
		}
	}
	
	if ( 0>H5Sselect_hyperslab(h5_space, H5S_SELECT_SET, &h5_start[0], NULL, &h5_subsize[0], NULL) ) handle_hdf5_err();
}

void do_read(Context& ctx, const Transfer_cfg& xfer, hid_t h5_file, hid_t read_lst, Ref ref)
{
	string dataset_name = xfer.dataset().to_string(ctx);
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	select(ctx, h5_mem_space, xfer.memory_selection());
	
	auto&& dataset_type_iter = xfer.parent().datasets().find(dataset_name);
	Datatype_uptr explicit_dataset_type;
	if ( dataset_type_iter != xfer.parent().datasets().end() ) {
		explicit_dataset_type = dataset_type_iter->second->evaluate(ctx);
	}
	
	const Datatype& file_datatype = ((explicit_dataset_type) ? (
	            *explicit_dataset_type
	        ) : (
	            ref.type()
	        ));
	        
	Raii_hid h5_file_space, h5_file_type;
	tie(h5_file_space, h5_file_type) = space(file_datatype);
	select(ctx, h5_file_space, xfer.dataset_selection(), h5_mem_space);
	
	Raii_hid h5_set = make_raii_hid(H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose);
	if ( 0>H5Dread(h5_set, h5_mem_type, h5_mem_space, h5_file_space, read_lst, Ref_w{ref}) ) handle_hdf5_err();
}

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

void do_write(Context& ctx, const Transfer_cfg& xfer, hid_t h5_file, hid_t write_lst, Ref ref)
{
	string dataset_name = xfer.dataset().to_string(ctx);
	
	Raii_hid h5_mem_space, h5_mem_type;
	tie(h5_mem_space, h5_mem_type) = space(ref.type());
	select(ctx, h5_mem_space, xfer.memory_selection());
	
	hssize_t n_data_pts = H5Sget_select_npoints(h5_mem_space);
	if ( 0>n_data_pts ) handle_hdf5_err();
	
	auto&& dataset_type_iter = xfer.parent().datasets().find(dataset_name);
	Datatype_uptr explicit_dataset_type;
	if ( dataset_type_iter != xfer.parent().datasets().end() ) {
		explicit_dataset_type = dataset_type_iter->second->evaluate(ctx);
	}
	
	const Datatype& file_datatype = ((explicit_dataset_type) ? (
	            *explicit_dataset_type
	        ) : (
	            ref.type()
	        ));
	        
	        
	Raii_hid h5_file_type, h5_file_space;
	tie(h5_file_space, h5_file_type) = space(file_datatype);
	Raii_hid h5_file_selection = make_raii_hid(H5Scopy(h5_file_space), H5Sclose);
	select(ctx, h5_file_selection, xfer.dataset_selection(), h5_mem_space);
	
	hssize_t n_file_pts = H5Sget_select_npoints(h5_file_selection);
	if ( 0>n_file_pts ) handle_hdf5_err();
	
	if ( n_data_pts != n_file_pts ) {
		vector<hsize_t> pr_size;
		vector<hsize_t> pr_subsize;
		vector<hsize_t> pr_start;
		
		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_mem_space);
		stringstream mem_desc;
		for ( int ii=0; ii< pr_size.size(); ++ii) mem_desc << " ("<< pr_start[ii]<<"-"<<(pr_start[ii]+pr_subsize[ii]-1)<<"/0-"<< (pr_size[ii]-1)<<")";
		
		tie(pr_size, pr_start, pr_subsize) = get_selection(h5_file_selection);
		stringstream file_desc;
		for ( int ii=0; ii< pr_size.size(); ++ii) file_desc << " ("<< pr_start[ii]<<"-"<<(pr_start[ii]+pr_subsize[ii]-1)<<"/0-"<< (pr_size[ii]-1)<<")";
		
		throw Error{PDI_ERR_CONFIG, "Incompatible selections while writing `%s': [%s ] -> [%s ]", dataset_name.c_str(), mem_desc.str().c_str(), file_desc.str().c_str()};
	}
	
	Raii_hid set_lst = make_raii_hid(H5Pcreate(H5P_LINK_CREATE), H5Pclose);
	if ( 0>H5Pset_create_intermediate_group(set_lst, 1) ) handle_hdf5_err();
	
	hid_t h5_set_raw = H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT);
	if ( 0 > h5_set_raw ) {
		h5_set_raw = H5Dcreate2(h5_file, dataset_name.c_str(), h5_file_type, h5_file_space, set_lst, H5P_DEFAULT, H5P_DEFAULT);
	}
	Raii_hid h5_set = make_raii_hid(h5_set_raw, H5Dclose);
	
	if ( 0>H5Dwrite(h5_set, h5_mem_type, h5_mem_space, h5_file_selection, write_lst, Ref_r{ref}) ) handle_hdf5_err();
}

void read(Context& ctx, const Transfer_cfg& xfer, Ref ref)
{
	try {
		if ( !xfer.when().to_long(ctx) ) return;
	} catch (Error&) {
		return; // an invalid expression is handled as false
	}
	
	Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid read_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
<<<<<<< 8310a4f70ca611ef65a4bf013296a21ae4b59ab5
<<<<<<< 4bd257caa2335bd5ca5634add863c6d6e5d68da7
	if ( xfer.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
=======
	if ( xfer.communicator() != "$MPI_COMM_SELF" ) {
=======
	if ( xfer.communicator() != "$MPI_COMM_SELF" && xfer.communicator(ctx) != MPI_COMM_SELF ) {
>>>>>>> Small fixes and added example to decl_hdf5
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(ctx), MPI_INFO_NULL) ) handle_hdf5_err();
>>>>>>> Replaced predef_desc by mpi plugin
		if ( 0>H5Pset_dxpl_mpio(read_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	Raii_hid h5_file = make_raii_hid(H5Fopen(xfer.parent().file().to_string(ctx).c_str(), H5F_ACC_RDONLY, file_lst), H5Fclose);
	do_read(ctx, xfer, h5_file, read_lst, ref);
}

void write(Context& ctx, const Transfer_cfg& xfer, Ref ref)
{
	try {
		if ( !xfer.when().to_long(ctx) ) return;
	} catch (Error&) {
		return; // an invalid expression is handled as false
	}
	
	Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid write_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
<<<<<<< 8310a4f70ca611ef65a4bf013296a21ae4b59ab5
<<<<<<< 4bd257caa2335bd5ca5634add863c6d6e5d68da7
	if ( xfer.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
=======
	if ( xfer.communicator() != "$MPI_COMM_SELF" ) {
=======
	if ( xfer.communicator() != "$MPI_COMM_SELF" && xfer.communicator(ctx) != MPI_COMM_SELF ) {
>>>>>>> Small fixes and added example to decl_hdf5
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(ctx), MPI_INFO_NULL) ) handle_hdf5_err();
>>>>>>> Replaced predef_desc by mpi plugin
		if ( 0>H5Pset_dxpl_mpio(write_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	string filename = xfer.parent().file().to_string(ctx);
	hid_t h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
	if (0>h5_file_raw) {
		h5_file_raw = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	}
	Raii_hid h5_file = make_raii_hid(h5_file_raw, H5Fclose);
	
	do_write(ctx, xfer, h5_file, write_lst, ref);
}

void execute(Context& ctx, const File_cfg& file_cfg)
{
	Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid xfer_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
<<<<<<< 8310a4f70ca611ef65a4bf013296a21ae4b59ab5
<<<<<<< 4bd257caa2335bd5ca5634add863c6d6e5d68da7
	if ( file_cfg.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, file_cfg.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
=======
	if ( file_cfg.communicator() != "$MPI_COMM_SELF" ) {
=======
	if ( file_cfg.communicator() != "$MPI_COMM_SELF" && file_cfg.communicator(ctx) != MPI_COMM_SELF ) {
>>>>>>> Small fixes and added example to decl_hdf5
		if ( 0>H5Pset_fapl_mpio(file_lst, file_cfg.communicator(ctx), MPI_INFO_NULL) ) handle_hdf5_err();
>>>>>>> Replaced predef_desc by mpi plugin
		if ( 0>H5Pset_dxpl_mpio(xfer_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	string filename = file_cfg.file().to_string(ctx);
	hid_t h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
	if (0>h5_file_raw) {
		h5_file_raw = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	}
	Raii_hid h5_file = make_raii_hid(h5_file_raw, H5Fclose);
	
	for (auto&& read: file_cfg.read() ) {
		const Transfer_cfg& xfer = read.second;
		try {
			if ( !xfer.when().to_long(ctx) ) continue;
		} catch (Error&) {
			continue; // an invalid expression is handled as false
		}
		do_read(ctx, xfer, h5_file, xfer_lst, ctx[read.first].ref());
		Ref ref = ctx[read.first].ref();
		if ( Ref_w{ref} ) {
			do_read(ctx, xfer, h5_file, xfer_lst, ref);
		} else {
			ctx.logger()->warn("(Decl'HDF5) Reference to read not available: `{}'", read.first);
		}
	}
	
	for (auto&& write: file_cfg.write() ) {
		const Transfer_cfg& xfer = write.second;
		try {
			if ( !xfer.when().to_long(ctx) ) continue;
		} catch (Error&) {
			continue; // an invalid expression is handled as false
		}
		Ref ref = ctx[write.first].ref();
		if ( Ref_r{ref} ) {
			do_write(ctx, xfer, h5_file, xfer_lst, ref);
		} else {
			ctx.logger()->warn("(Decl'HDF5) Reference to write not available: `{}'", write.first);
		}
	}
}

struct decl_hdf5_plugin: Plugin {

	const Decl_hdf5_cfg m_config;
	
	decl_hdf5_plugin(Context& ctx, PC_tree_t config, MPI_Comm*):
		Plugin {ctx},
<<<<<<< 4bd257caa2335bd5ca5634add863c6d6e5d68da7
		m_config{config, ctx.logger()}
=======
		m_config{config}
>>>>>>> Replaced predef_desc by mpi plugin
	{
		Hdf5_error_handler _;
		if ( 0>H5open() ) handle_hdf5_err("Cannot initialize HDF5 library");
		ctx.logger()->info("(Decl'HDF5) Plugin loaded successfully");
	}
	
	void data(const char* name, Ref ref) override
	{
		Hdf5_error_handler _;
		if (Ref_r{ref}) {
			auto range = m_config.write().equal_range(name);
			for (auto&& write_cfg = range.first; write_cfg != range.second; ++write_cfg) {
				write(context(), write_cfg->second, ref);
			}
		}
		if (Ref_w{ref}) {
			auto range = m_config.read().equal_range(name);
			for (auto&& read_cfg = range.first; read_cfg != range.second; ++read_cfg) {
				read(context(), read_cfg->second, ref);
			}
		}
	}
	
	void event(const char* event) override
	{
		Hdf5_error_handler _;
		auto range = m_config.events().equal_range(event);
		for (auto&& file = range.first; file != range.second; ++file) {
			execute(context(), file->second);
		}
	}
	
}; // struct decl_hdf5_plugin

} // namespace <anonymous>

PDI_PLUGIN(decl_hdf5)
