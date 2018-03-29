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
#include <memory>
#include <string>
#include <unordered_map>
#include <vector>

#include <unistd.h>

#include <hdf5.h>
#include <hdf5_hl.h>

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


namespace {

using PDI::Array_datatype;
using PDI::Context;
using PDI::Datatype;
using PDI::Data_descriptor;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Error;
using PDI::len;
using PDI::Plugin;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;
using PDI::Expression;
using std::cerr;
using std::endl;
using std::reverse;
using std::string;
using std::unordered_map;
using std::unique_ptr;
using std::vector;

struct Slab_dim {

	Expression start;
	
	Expression size;
	
};

struct Hdf5_variable {

	Expression h5var;
	
	Expression h5file;
	
	Expression select;
	
	/// hyperslab to write in the file
	vector<Slab_dim> file_slab;
	
};

/** Reads the plugin config for either inputs or outputs
    * \param conf the configuration node to read
    * \param dflt_file the default file for HDF5 inputs/outputs
    * \param dflt_select the default select for HDF5 inputs/outputs
    * \return the array where to store the resulting variables
    */
unordered_map<string, Hdf5_variable> read_config_file(Context& ctx, PC_tree_t conf, const unique_ptr<string> dflt_file, const unique_ptr<string> dflt_select)
{
	unordered_map<string, Hdf5_variable> result;
	
	int nb_hdf5data = len(conf, 0);
	for (int ii = 0; ii < nb_hdf5data; ++ii) {
	
		string data_name = to_string(PC_get(conf, "{%d}", ii));
		PC_tree_t data_conf = PC_get(conf, "<%d>", ii); // get the node
		
		// set the corresponding HDF5 dataset name
		// the data name is used as the default dataset name
		Expression var = to_string(PC_get(data_conf, ".var"), data_name);
		
		// set the HDF5 filename (i.e. where do we write the data)
		Expression file;
		if ( dflt_file ) {
			file = to_string(PC_get(data_conf, ".file"), *dflt_file);
		} else {
			file = to_string(PC_get(data_conf, ".file"));
		}
		
		// sampling or frequency (i.e. when do we expose)
		Expression select;
		if ( dflt_select ) {
			select = to_string(PC_get(data_conf, ".select"), *dflt_select);
		} else {
			select = to_string(PC_get(data_conf, ".select"));
		}
		
		PC_tree_t cfg = ctx.desc(data_name).config();
		vector<Slab_dim> global;
		PC_tree_t global_starts = PC_get(cfg, ".global_starts");
		PC_tree_t global_sizes = PC_get(cfg, ".global_sizes");
		
		if (PC_status(global_sizes) != PC_status(global_starts)) {
			throw Error {PDI_ERR_CONFIG, "global_sizes and global_starts must either both be specified or none should be"};
		}
		
		if (!PC_status(global_starts)) {
			int rank = len(global_starts);
			if (rank != len(global_sizes)) {
				throw Error {PDI_ERR_CONFIG, "Non matching rank of global_starts & global_sizes"};
			}
			
			int mult = 1; int offset = 0;
			string order = to_string(PC_get(cfg, ".order"), "C");
			if (order == "fortran" || order == "Fortran") {
				mult = -1; offset = rank - 1;
			}
			
			for (int rank_id = 0; rank_id < rank; ++rank_id) {
				global.emplace_back(Slab_dim {to_string(PC_get(global_starts, "[%d]", rank_id * mult + offset)), to_string(PC_get(global_sizes, "[%d]", rank_id * mult + offset))});
			}
		}
		
		result.emplace(data_name, Hdf5_variable {var, file, select, move(global)});
	}
	
	return result;
}

void init_sizes(Context& ctx, const Datatype& type, const vector<Slab_dim>& file_slab, hid_t& file_space, hid_t& mem_space, hid_t& h5type)
{
	int rank = 0;
	vector<hsize_t> h5msizes;
	vector<hsize_t> h5subsizes;
	vector<hsize_t> h5mstarts;
	const Datatype* subtype = &type;
	
	while (auto&& array_type = dynamic_cast<const Array_datatype*>(subtype)) {
		++rank;
		h5msizes.emplace_back(array_type->size());
		h5subsizes.emplace_back(array_type->subsize());
		h5mstarts.emplace_back(array_type->start());
		subtype = &array_type->subtype();
	}
	reverse(h5msizes.begin(), h5msizes.end());
	reverse(h5subsizes.begin(), h5subsizes.end());
	reverse(h5mstarts.begin(), h5mstarts.end());
	
	if (file_slab.size() && rank != static_cast<int>(file_slab.size())) throw Error {PDI_ERR_TYPE, "Invalid rank for data in HDF5"};
	
	auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(subtype);
	if (!scalar_type) throw Error {PDI_ERR_IMPL, "Unexpected type in HDF5"};
	
	switch (scalar_type->kind()) {
	case Scalar_kind::UNSIGNED: {
		switch (scalar_type->datasize()) {
		case 1: h5type = H5T_NATIVE_UINT8; break;
		case 2: h5type = H5T_NATIVE_UINT16; break;
		case 4: h5type = H5T_NATIVE_UINT32; break;
		case 8: h5type = H5T_NATIVE_UINT64; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 signed: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::SIGNED: {
		switch (scalar_type->datasize()) {
		case 1: h5type = H5T_NATIVE_INT8; break;
		case 2: h5type = H5T_NATIVE_INT16; break;
		case 4: h5type = H5T_NATIVE_INT32; break;
		case 8: h5type = H5T_NATIVE_INT64; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 unsigned: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::FLOAT: {
		switch (scalar_type->datasize()) {
		case 4:  h5type = H5T_NATIVE_FLOAT; break;
		case 8:  h5type = H5T_NATIVE_DOUBLE; break;
		case 16: h5type = H5T_NATIVE_LDOUBLE; break;
		default: throw Error {PDI_ERR_TYPE, "Invalid size for HDF5 float: #%ld", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::ADDRESS: case Scalar_kind::UNKNOWN:
		throw Error {PDI_ERR_TYPE, "Invalid type for HDF5: #%d", scalar_type->kind()};
	}
	
	// The size in memory (with ghost, data might be sparse)
	mem_space = H5Screate_simple(rank, &h5msizes[0], NULL);
	if (rank) {
		H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &h5mstarts[0], NULL, &h5subsizes[0], NULL);
	}
	
	// File space: part of the global file that we use to store our piece of array
	if (file_slab.size()) {
		vector<hsize_t> h5fsizes;
		vector<hsize_t> h5fstarts;
		for (auto&& sz : file_slab) {
			h5fsizes.emplace_back(sz.size.to_long(ctx));
			h5fstarts.emplace_back(sz.start.to_long(ctx));
		}
		file_space = H5Screate_simple(rank, &h5fsizes[0], NULL);
		H5Sselect_hyperslab(file_space, H5S_SELECT_SET, &h5fstarts[0], NULL, &h5subsizes[0], NULL);
	} else {
		file_space = H5Screate_simple(rank, &h5subsizes[0], NULL);
	}
}

int is_h5_file(const string& filename)
{
	H5E_auto2_t old_func;
	void* old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	int fexists = (H5Fis_hdf5(filename.c_str()) > 0);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
	return fexists;
}

void rm_if_exist(hid_t h5file, const string& dset_name)
{
	H5E_auto2_t old_func;
	void* old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	H5Ldelete(h5file, dset_name.c_str(), H5P_DEFAULT);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
}

void write_to_file(Context& ctx, Ref cref, const string& filename, const string& pathname, const vector<Slab_dim>& file_slab)
{
	Ref_r ref = cref;
	if (!ref) {
		throw Error {PDI_ERR_PLUGIN, "Could not get read access on variable to write it to disk"};
	}
	
	hid_t file_lst = H5Pcreate(H5P_FILE_ACCESS);
	if (file_lst < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to create HDF5 file plist"};
	}
#ifdef H5_HAVE_PARALLEL
	if (file_slab.size()) {
		H5Pset_fapl_mpio(file_lst, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)
	}
#endif
	
	hid_t file;
	if (is_h5_file(filename)) {
		file = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	} else {
		file = H5Fcreate(filename.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, file_lst);
	}
	
	hid_t fspace, mspace, type;
	init_sizes(ctx, cref.type(), file_slab, fspace, mspace, type);
	
	hid_t set_lst = H5Pcreate(H5P_LINK_CREATE);
	if (set_lst < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to create HDF5 set plist"};
	}
	if (H5Pset_create_intermediate_group(set_lst, 1) < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to set automatic creation of intermediate groups in HDF5"};
	}
	
	rm_if_exist(file, pathname);
	hid_t set = H5Dcreate(file, pathname.c_str(), type, fspace, set_lst, H5P_DEFAULT, H5P_DEFAULT);
	if (set < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to create HDF5 dataset"};
	}
	
	hid_t write_lst = H5Pcreate(H5P_DATASET_XFER);
	if (write_lst < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to create HDF5 dset plist"};
	}
#ifdef H5_HAVE_PARALLEL
	if (file_slab.size()) {
		if (H5Pset_dxpl_mpio(write_lst, H5FD_MPIO_COLLECTIVE) < 0) {
			throw Error {PDI_ERR_SYSTEM, "Unable to set collective IO in HDF5"};
		}
	}
#endif
	
	if (H5Dwrite(set, type, mspace, fspace, write_lst, ref) < 0) {
		throw Error {PDI_ERR_SYSTEM, "Unable to write HDF5 file"};
	}
	
	H5PTclose(write_lst);
	H5Dclose(set);
	H5PTclose(set_lst);
	H5Sclose(mspace);
	H5Sclose(fspace);
	H5Fclose(file);
	H5PTclose(file_lst);
}

void read_from_file(Context& ctx, Ref cref, const string& filename, const string& pathname, const vector<Slab_dim>& file_slab)
{
	Ref_w ref = cref;
	if (!ref) {
		throw Error {PDI_ERR_PLUGIN, "Could not get read access on variable to write it to disk"};
	}
	
	hid_t file_lst = H5Pcreate(H5P_FILE_ACCESS);
#ifdef H5_HAVE_PARALLEL
	if (file_slab.size()) {
		H5Pset_fapl_mpio(file_lst, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)
	}
#endif
	
	hid_t file = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	
	hid_t fspace, mspace, type;
	init_sizes(ctx, cref.type(), file_slab, fspace, mspace, type);
	
	hid_t set = H5Dopen(file, pathname.c_str(), H5P_DEFAULT);
	
	hid_t read_lst = H5Pcreate(H5P_DATASET_XFER);
#ifdef H5_HAVE_PARALLEL
	if (file_slab.size()) {
		H5Pset_dxpl_mpio(read_lst, H5FD_MPIO_COLLECTIVE);
	}
#endif
	
	H5Dread(set, type, mspace, fspace, read_lst, ref);
	
	H5PTclose(read_lst);
	H5Dclose(set);
	H5Sclose(mspace);
	H5Sclose(fspace);
	H5Fclose(file);
	H5PTclose(file_lst);
}

struct decl_hdf5_plugin: Plugin {

	unordered_map<string, Hdf5_variable> outputs;
	
	unordered_map<string, Hdf5_variable> inputs;
	
	
	decl_hdf5_plugin(Context& ctx, PC_tree_t conf, MPI_Comm*):
		Plugin {ctx}
	{
		// Failure initializing HDF5
		if ( H5open() < 0 ) {
			throw Error {PDI_ERR_PLUGIN, "Cannot initialize HDF5 library"};
		}
		
		{
			unique_ptr<string> dflt_file;
			try {
				dflt_file.reset(new string{to_string(PC_get(conf, ".defaults.outputs.file"))});
			} catch (const Error&) {}
			unique_ptr<string> dflt_select;
			try {
				dflt_select.reset(new string{to_string(PC_get(conf, ".defaults.outputs.select"))});
			} catch (const Error&) {}
			outputs = read_config_file(ctx, PC_get(conf, ".outputs"), move(dflt_file), move(dflt_select));
		}
		
		{
			unique_ptr<string> dflt_file;
			try {
				dflt_file.reset(new string{to_string(PC_get(conf, ".defaults.inputs.file"))});
			} catch (const Error&) {}
			unique_ptr<string> dflt_select;
			try {
				dflt_select.reset(new string{to_string(PC_get(conf, ".defaults.inputs.select"))});
			} catch (const Error&) {}
			inputs = read_config_file(ctx, PC_get(conf, ".inputs"), move(dflt_file), move(dflt_select));
		}
	}
	
	void data(const char* name, Ref ref) override
	{
		auto&& outev = outputs.find(name);
		if (outev != outputs.end()) {
			try {
				long select;
				try {
					select = outev->second.select.to_long(context());
				} catch (const Error&) {
					select = 0;
				}
				if (select) {
					write_to_file(context(), ref, outev->second.h5file.to_string(context()), outev->second.h5var.to_string(context()), outev->second.file_slab);
				}
			} catch (...) {
				cerr << "HDF5 error writing "<<name<<endl;
			}
		}
		
		auto&& inev = inputs.find(name);
		if (inev != inputs.end()) {
			try {
				long select;
				try {
					select = inev->second.select.to_long(context());
				} catch (const Error&) {
					select = 0;
				}
				if (select) {
					read_from_file(context(), ref, inev->second.h5file.to_string(context()), inev->second.h5var.to_string(context()), inev->second.file_slab);
				}
			} catch (...) {
				cerr << "HDF5 error reading "<<name<<endl;
			}
		}
	}
	
}; // struct decl_hdf5_plugin

} // namespace <anonymous>

PDI_PLUGIN(decl_hdf5)
