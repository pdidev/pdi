/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#include <string>
#include <unordered_map>
#include <vector>

#include <hdf5.h>
#include <hdf5_hl.h>

#include <paraconf.h>
#include <pdi.h>
#include <pdi/data_descriptor.h>
#include <pdi/data_reference.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/state.h>
#include <pdi/value.h>
#include <unistd.h>


namespace
{

using PDI::Array_datatype;
using PDI::Datatype;
using PDI::Data_descriptor;
using PDI::Data_ref;
using PDI::Data_r_ref;
using PDI::Data_w_ref;
using PDI::Error;
using PDI::len;
using PDI::PDI_K_ARRAY;
using PDI::PDI_K_SCALAR;
using PDI::PDI_K_STRUCT;
using PDI::Scalar_datatype;
using PDI::to_string;
using PDI::Value;
using std::string;
using std::unordered_map;
using std::vector;

struct Slab_dim {
	
	Value start;
	
	Value size;
};

struct Hdf5_variable {
	
	Value h5var;

	Value h5file;

	Value select;

	/// hyperslab to write in the file
	vector<Slab_dim> file_slab;
};


unordered_map<string, Hdf5_variable> outputs;

unordered_map<string, Hdf5_variable> inputs;


/** Reads the plugin config for either inputs or outputs
 * \param conf the configuration node to read
 * \param def_file the default file for HDF5 inputs/outputs
 * \param def_select the default select for HDF5 inputs/outputs
 * \return the array where to store the resulting variables
 */
unordered_map<string, Hdf5_variable> read_config_file(PC_tree_t conf, PC_tree_t dflt_file, PC_tree_t dflt_select)
{
	int nb_hdf5data;
	try {
		nb_hdf5data = len(conf);
	} catch(const Error &) {
		return {};
	}

	unordered_map<string, Hdf5_variable> result;
	for(int ii = 0; ii < nb_hdf5data; ++ii) {
		
		string data_name = to_string(PC_get(conf, "{%d}", ii));
		PC_tree_t data_conf = PC_get(conf, "<%d>", ii); // get the node

		// set the corresponding HDF5 dataset name
		Value var;
		try {
			var = to_string(PC_get(data_conf, ".var"));
		} catch(const Error &) {
			// the data name is used as the default dataset name
			var = data_name;
		}

		// set the HDF5 filename (i.e. where do we write the data)
		Value file;
		try {
			file = to_string(PC_get(data_conf, ".file"));
		} catch(const Error &) {
			file = to_string(dflt_file); // using default
		}

		// sampling or frequency (i.e. when do we expose)
		Value select;
		try {
			select = to_string(PC_get(data_conf, ".select"));
		} catch(const Error &) {
			select = to_string(dflt_select); // using default
		}

		const Data_descriptor &desc = PDI_state.desc(data_name);
		vector<Slab_dim> global;
		PC_tree_t global_starts = PC_get(desc.get_config(), ".global_starts");
		PC_tree_t global_sizes = PC_get(desc.get_config(), ".global_sizes");
		
		if ( PC_status(global_sizes) != PC_status(global_starts) ) {
			throw Error{PDI_ERR_CONFIG, "global_sizes and global_starts must either both be specified or none should be"};
		}
		
		if ( desc.get_type().kind == PDI::PDI_K_ARRAY && !PC_status(global_starts) ) {
			int rank = len(global_starts);
			if ( rank != len(global_sizes) ) {
				throw Error{PDI_ERR_CONFIG, "Non matching rank of global_starts & global_sizes"};
			}
			
			int mult = 1; int offset = 0;
			try {
				string order = to_string(PC_get(desc.get_config(), ".order"));
				if ( order == "fortran" || order == "Fortran" ) {
					mult = -1; offset = rank -1;
				}
			} catch (const Error&){}
			
			for(int ii = 0; ii < rank; ++ii) {
				global.emplace_back(Slab_dim {to_string(PC_get(global_starts, "[%d]", ii*mult+offset)), to_string(PC_get(global_sizes, "[%d]", ii*mult+offset))});
			}
		}

		result.emplace(data_name, Hdf5_variable {var, file, select, move(global)});
	}

	return result;
}

PDI_status_t PDI_declh5_init(PC_tree_t conf, MPI_Comm *)
{
	if(PC_status(conf)) {
		throw Error {PDI_ERR_PLUGIN, "Invalid configuration."};
	}
	
	if(H5open() < 0) { // Failure initializing HDF5
		throw Error {PDI_ERR_PLUGIN, "Cannot initialize HDF5 library"};
	}

	outputs = read_config_file(PC_get(conf, ".outputs"), PC_get(conf, ".defaults.outputs.file"), PC_get(conf, ".defaults.outputs.select"));
	
	inputs = read_config_file(PC_get(conf, ".inputs"), PC_get(conf, ".defaults.inputs.file"), PC_get(conf, ".defaults.inputs.select"));
	
	return PDI_OK;
}

PDI_status_t PDI_declh5_finalize()
{
	inputs.clear();
	outputs.clear();
	return PDI_OK;
}

PDI_status_t PDI_declh5_event(const char *)
{
	return PDI_OK;
}

void init_sizes(const Datatype &type, const vector<Slab_dim>& file_slab, hid_t& file_space, hid_t& mem_space, hid_t& h5type)
{
	size_t rank = 0;
	vector<hsize_t> h5msizes;
	vector<hsize_t> h5subsizes;
	vector<hsize_t> h5mstarts;
	const Datatype *subtype = &type;

	while (subtype->kind == PDI_K_ARRAY) {
		const Array_datatype &array_type = *subtype->c.array;
		rank += array_type.m_dimensions.size();

		for(auto && dim : array_type.m_dimensions) {
			h5msizes.emplace_back(dim.m_size);
			h5subsizes.emplace_back(dim.m_subsize);
			h5mstarts.emplace_back(dim.m_start);
		}
		
		subtype = &array_type.type;
	}

	if ( file_slab.size() && rank != file_slab.size() ) throw Error{PDI_ERR_TYPE, "Invalid rank for data in HDF5"};
	
	if(subtype->kind != PDI_K_SCALAR) throw Error{PDI_ERR_IMPL, "Unexpected type in HDF5"};
	switch(subtype->c.scalar) {
	using namespace PDI;
	case PDI_T_INT8:        h5type = H5T_NATIVE_INT8; break;
	case PDI_T_INT16:       h5type = H5T_NATIVE_INT16; break;
	case PDI_T_INT32:       h5type = H5T_NATIVE_INT32; break;
	case PDI_T_INT64:       h5type = H5T_NATIVE_INT64; break;
	case PDI_T_FLOAT:       h5type = H5T_NATIVE_FLOAT; break;
	case PDI_T_DOUBLE:      h5type = H5T_NATIVE_DOUBLE; break;
	case PDI_T_LONG_DOUBLE: h5type = H5T_NATIVE_LDOUBLE; break;
	default: throw Error {PDI_ERR_TYPE, "Invalid type for HDF5: #%d", subtype->c.scalar};
	}
	
	// The size in memory (with ghost, data might be sparse)
	mem_space = H5Screate_simple(rank, &h5msizes[0], NULL);
	if ( rank ) H5Sselect_hyperslab(mem_space, H5S_SELECT_SET, &h5mstarts[0], NULL, &h5subsizes[0], NULL);
	
	// File space: part of the global file that we use to store our piece of array
	if ( file_slab.size() ) {
		vector<hsize_t> h5fsizes;
		vector<hsize_t> h5fstarts;
		for(auto && sz : file_slab) {
			h5fsizes.emplace_back(sz.size);
			h5fstarts.emplace_back(sz.start);
		}
		file_space = H5Screate_simple(rank, &h5fsizes[0], NULL);
		H5Sselect_hyperslab(file_space, H5S_SELECT_SET, &h5fstarts[0], NULL, &h5subsizes[0], NULL);
	} else {
		file_space = H5Screate_simple(rank, &h5subsizes[0], NULL);
	}
}

int is_h5_file(const string &filename)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	int fexists = (H5Fis_hdf5(filename.c_str()) > 0);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
	return fexists;
}

void rm_if_exist(hid_t h5file, const string &dset_name)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	H5Ldelete(h5file, dset_name.c_str(), H5P_DEFAULT);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
}

void write_to_file(Data_ref cref, const string &filename, const string &pathname, const vector<Slab_dim>& file_slab)
{
	Data_r_ref ref = cref;
	if ( !ref ) {
		throw Error {PDI_ERR_PLUGIN, "Could not get read access on variable to write it to disk"};
	}
	
	hid_t file_lst = H5Pcreate(H5P_FILE_ACCESS);
	if ( file_lst<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to create HDF5 file plist"};
	}
#ifdef H5_HAVE_PARALLEL
	if ( file_slab.size() ) {
		H5Pset_fapl_mpio(file_lst, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)
	}
#endif
	
	hid_t file;
	if(is_h5_file(filename)) {
		file = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);
	} else {
		file = H5Fcreate(filename.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, file_lst);
	}
	
	hid_t fspace, mspace, type;
	init_sizes(cref.type(), file_slab, fspace, mspace, type);
	
	hid_t set_lst = H5Pcreate(H5P_LINK_CREATE);
	if ( set_lst<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to create HDF5 set plist"};
	}
	if ( H5Pset_create_intermediate_group(set_lst, 1)<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to set automatic creation of intermediate groups in HDF5"};
	}
	
	rm_if_exist(file, pathname);
	hid_t set = H5Dcreate(file, pathname.c_str(), type, fspace, set_lst, H5P_DEFAULT, H5P_DEFAULT);
	if ( set<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to create HDF5 dataset"};
	}

	hid_t write_lst = H5Pcreate(H5P_DATASET_XFER);
	if ( write_lst<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to create HDF5 dset plist"};
	}
#ifdef H5_HAVE_PARALLEL
	if ( file_slab.size() ) {
		if ( H5Pset_dxpl_mpio(write_lst, H5FD_MPIO_COLLECTIVE)<0 ) {
			throw Error{PDI_ERR_SYSTEM, "Unable to set collective IO in HDF5"};
		}
	}
#endif
	
	if ( H5Dwrite(set, type, mspace, fspace, write_lst, ref)<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to write HDF5 file"};
	}

	H5PTclose(write_lst);
	H5Dclose(set);
	H5PTclose(set_lst);
	H5Sclose(mspace);
	H5Sclose(fspace);
	H5Fclose(file);
	H5PTclose(file_lst);
}

void read_from_file(Data_ref cref, const string &filename, const string &pathname, const vector<Slab_dim>& file_slab)
{
	Data_w_ref ref = cref;
	if ( !ref ) {
		throw Error {PDI_ERR_PLUGIN, "Could not get read access on variable to write it to disk"};
	}
	
	hid_t file_lst = H5Pcreate(H5P_FILE_ACCESS);
#ifdef H5_HAVE_PARALLEL
	if ( file_slab.size() ) {
		H5Pset_fapl_mpio(file_lst, MPI_COMM_WORLD, MPI_INFO_NULL);  // todo: check communicator (set another than WORLD)
	}
#endif
	
	hid_t file = H5Fopen(filename.c_str(), H5F_ACC_RDWR, file_lst);

	hid_t fspace, mspace, type;
	init_sizes(cref.type(), file_slab, fspace, mspace, type);
	
	hid_t set = H5Dopen(file, pathname.c_str(), H5P_DEFAULT);

	hid_t read_lst = H5Pcreate(H5P_DATASET_XFER);
#ifdef H5_HAVE_PARALLEL
	if ( file_slab.size() ) {
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

PDI_status_t PDI_declh5_data(const string &name, Data_ref ref)
{
	auto&& outev = outputs.find(name);
	if(outev != outputs.end()) {
		int select;
		try {
			select = outev->second.select;
		} catch ( const Error& ) {
			select = 0;
		}
		if ( select ) {
			write_to_file(ref, outev->second.h5file, outev->second.h5var, outev->second.file_slab);
		}
	}

	auto&& inev = inputs.find(name);
	if(inev != inputs.end()) {
		int select;
		try {
			select = outev->second.select;
		} catch ( const Error& ) {
			select = 0;
		}
		if(select) {
			read_from_file(ref, inev->second.h5file, inev->second.h5var, inev->second.file_slab);
		}
	}

	return PDI_OK;
}

} // namespace <anonymous>

PDI_PLUGIN(declh5)
