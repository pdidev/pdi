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
#include <functional>
#include <iostream>
#include <memory>
#include <string>
#include <tuple>
#include <unordered_map>
#include <vector>

#include <unistd.h>

#include <hdf5.h>

#include <paraconf.h>

#include <pdi.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <pdi/scalar_datatype.h>
#include <pdi/expression.h>

#include "ast.h"
#include "raii.h"

namespace {

using namespace decl_hdf5;
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
using std::cerr;
using std::endl;
using std::make_tuple;
using std::move;
using std::tie;
using std::transform;
using std::tuple;

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
	
	Raii_hid h5_space = raii_call(H5Screate_simple(rank, &h5_size[0], NULL), H5Sclose);
	    
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
	transform( h5_start.begin(), h5_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end){return end-start+1;});
	
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
		transform( dflt_start.begin(), dflt_start.end(), h5_subsize.begin(), h5_subsize.begin(), [](hsize_t start, hsize_t end){return end-start+1;});
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
	
	Raii_hid h5_set = raii_call(H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose);
	if ( 0>H5Dread(h5_set, h5_mem_type, h5_mem_space, h5_file_space, read_lst, Ref_w{ref}) ) handle_hdf5_err();
}

void do_write(Context& ctx, const Transfer_cfg& xfer, hid_t h5_file, hid_t write_lst, Ref ref)
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
	        
	        
	Raii_hid h5_file_type, h5_file_space;
	tie(h5_file_space, h5_file_type) = space(file_datatype);
	Raii_hid h5_file_selection = raii_call(H5Scopy(h5_file_space), H5Sclose);
	select(ctx, h5_file_selection, xfer.dataset_selection(), h5_mem_space);
	
	Raii_hid set_lst = raii_call(H5Pcreate(H5P_LINK_CREATE), H5Pclose);
	if ( 0>H5Pset_create_intermediate_group(set_lst, 1) ) handle_hdf5_err();
	
	//  {
	// //   int size; MPI_Comm_size(MPI_COMM_WORLD, &size);
	// //   int rank; MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	// //   for ( int ii=0; ii<size; ++ii ) {
	// //   if ( rank == ii ) {
	//
	//  cerr<<" ***    NAME: "<<dataset_name<<endl;
	//
	//  int pr_rank = H5Sget_simple_extent_ndims(h5_mem_space);
	//  vector<hsize_t> pr_size(pr_rank);
	//  H5Sget_simple_extent_dims(h5_mem_space, &pr_size[0], NULL);
	//  vector<hsize_t> pr_subsize(pr_rank);
	//  vector<hsize_t> pr_start(pr_rank);
	//  H5Sget_regular_hyperslab( h5_mem_space, &pr_start[0], NULL, &pr_subsize[0], NULL);
	//  cerr << " ***     MEM: "<< pr_rank <<":[";
	//  for ( int ii=0; ii< pr_rank; ++ii) cerr << " ("<< pr_subsize[ii]<<"/"<< pr_size[ii]<<"=>"<< pr_start[ii]<<")";
	//  cerr<<" ] @"<<Ref_r{ref}.get()<<endl;
	//
	//  int max_pr_idx = 0;
	//  for ( int ii=0; ii< pr_rank; ++ii ) {
	//          if (ii) max_pr_idx *= pr_size[ii-1];
	//          max_pr_idx += pr_size[ii];
	//  }
	//  cerr << " *** CONTENT:";
	//  for ( int pr_idx =0; pr_idx < max_pr_idx; ++pr_idx ) {
	//          if ( pr_idx && (pr_idx)% pr_size[0] == 0 ) cerr<<"             ";
	//          cerr << " "<<reinterpret_cast<const double*>(Ref_r{ref}.get())[pr_idx];
	//          if ( (pr_idx +1)% pr_size[0] == 0 ) cerr<<endl;
	//  }
	//
	//  pr_rank = H5Sget_simple_extent_ndims(h5_file_space);
	//  H5Sget_simple_extent_dims(h5_file_space, &pr_size[0], NULL);
	//  H5Sget_regular_hyperslab( h5_file_space, &pr_start[0], NULL, &pr_subsize[0], NULL);
	//  cerr << " ***    FILE: "<< pr_rank <<":[";
	//  for ( int ii=0; ii< pr_rank; ++ii) cerr << " ("<< pr_subsize[ii]<<"/"<< pr_size[ii]<<"=>"<< pr_start[ii]<<")";
	//  cerr<<" ]"<<endl;
	//
	//  pr_rank = H5Sget_simple_extent_ndims(h5_file_selection);
	//  H5Sget_simple_extent_dims(h5_file_selection, &pr_size[0], NULL);
	//  H5Sget_regular_hyperslab( h5_file_selection, &pr_start[0], NULL, &pr_subsize[0], NULL);
	//  cerr << " *** FSELECT: "<< pr_rank <<":[";
	//  for ( int ii=0; ii< pr_rank; ++ii) cerr << " ("<< pr_subsize[ii]<<"/"<< pr_size[ii]<<"=>"<< pr_start[ii]<<")";
	//  cerr<<" ]"<<endl;
	// //   }
	// //   MPI_Barrier(MPI_COMM_WORLD);
	// //   }
	//  }
	
	hid_t h5_set_raw = H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT);
	if ( 0 > h5_set_raw ) {
		h5_set_raw = H5Dcreate2(h5_file, dataset_name.c_str(), h5_file_type, h5_file_space, set_lst, H5P_DEFAULT, H5P_DEFAULT);
	}
	Raii_hid h5_set = raii_call(h5_set_raw, H5Dclose);
	    
	if ( 0>H5Dwrite(h5_set, h5_mem_type, h5_mem_space, h5_file_selection, write_lst, Ref_r{ref}) ) handle_hdf5_err();
}

void read(Context& ctx, const Transfer_cfg& xfer, Ref ref)
{
	try {
		if ( !xfer.when().to_long(ctx) ) return;
	} catch (...) {
		return;    // an invalid expression is handled as false
	}
	
	Raii_hid file_lst = raii_call(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid read_lst = raii_call(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
	if ( xfer.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
		if ( 0>H5Pset_dxpl_mpio(read_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	Raii_hid h5_file = raii_call(H5Fopen(xfer.parent().file().to_string(ctx).c_str(), H5F_ACC_RDONLY, file_lst), H5Fclose);
	do_read(ctx, xfer, h5_file, read_lst, ref);
}

void write(Context& ctx, const Transfer_cfg& xfer, Ref ref)
{
	try {
		if ( !xfer.when().to_long(ctx) ) return;
	} catch (...) {
		return;    // an invalid expression is handled as false
	}
	
	Raii_hid file_lst = raii_call(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid write_lst = raii_call(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
	if ( xfer.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, xfer.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
		if ( 0>H5Pset_dxpl_mpio(write_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	string filename = xfer.parent().file().to_string(ctx);
	hid_t h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
	if (0>h5_file_raw) {
		h5_file_raw = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	}
	Raii_hid h5_file = raii_call(h5_file_raw, H5Fclose);
	    
	do_write(ctx, xfer, h5_file, write_lst, ref);
}

void execute(Context& ctx, const File_cfg& file_cfg)
{
	Raii_hid file_lst = raii_call(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	Raii_hid xfer_lst = raii_call(H5Pcreate(H5P_DATASET_XFER), H5Pclose);
#ifdef H5_HAVE_PARALLEL
	if ( file_cfg.communicator() != MPI_COMM_SELF ) {
		if ( 0>H5Pset_fapl_mpio(file_lst, file_cfg.communicator(), MPI_INFO_NULL) ) handle_hdf5_err();
		if ( 0>H5Pset_dxpl_mpio(xfer_lst, H5FD_MPIO_COLLECTIVE) ) handle_hdf5_err();
	}
#endif
	
	string filename = file_cfg.file().to_string(ctx);
	hid_t h5_file_raw = H5Fcreate(filename.c_str(), H5F_ACC_EXCL, H5P_DEFAULT, file_lst);
	if (0>h5_file_raw) {
		h5_file_raw = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	}
	Raii_hid h5_file = raii_call(h5_file_raw, H5Fclose);
	    
	for (auto&& read: file_cfg.read() ) {
		const Transfer_cfg& xfer = read.second;
		try {
			if ( !xfer.when().to_long(ctx) ) continue;
		} catch (...) {
			continue;    // an invalid expression is handled as false
		}
		do_read(ctx, xfer, h5_file, xfer_lst, ctx[read.first].ref());
		Ref ref = ctx[read.first].ref();
		if ( Ref_w{ref} ) {
			do_read(ctx, xfer, h5_file, xfer_lst, ref);
		} else {
			cerr << " *** [PDI/Decl'HDF5] Warning: Reference to read not available: `"<<read.first<<"'"<<endl;
		}
	}
	
	for (auto&& write: file_cfg.write() ) {
		const Transfer_cfg& xfer = write.second;
		try {
			if ( !xfer.when().to_long(ctx) ) continue;
		} catch (...) {
			continue;    // an invalid expression is handled as false
		}
		Ref ref = ctx[write.first].ref();
		if ( Ref_r{ref} ) {
			do_write(ctx, xfer, h5_file, xfer_lst, ref);
		} else {
			cerr << " *** [PDI/Decl'HDF5] Warning: Reference to write not available: `"<<write.first<<"'"<<endl;
		}
	}
}

struct decl_hdf5_plugin: Plugin {

	const Decl_hdf5_cfg m_config;
	
	decl_hdf5_plugin(Context& ctx, PC_tree_t config, MPI_Comm*):
		Plugin {ctx},
		m_config{config}
	{
		Raii_set_hdf5_handler _;
		if ( 0>H5open() ) handle_hdf5_err("Cannot initialize HDF5 library");
	}
	
	void data(const char* name, Ref ref) override
	{
		Raii_set_hdf5_handler _;
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
		Raii_set_hdf5_handler _;
		auto range = m_config.events().equal_range(event);
		for (auto&& file = range.first; file != range.second; ++file) {
			execute(context(), file->second);
		}
	}
	
}; // struct decl_hdf5_plugin

} // namespace <anonymous>

PDI_PLUGIN(decl_hdf5)
