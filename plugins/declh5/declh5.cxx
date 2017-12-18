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

#include "config.h"

#include <mpi.h>

#include <iostream>
#include <string>
#include <unordered_map>
#include <vector>

#include <hdf5.h>
#include <hdf5_hl.h>

#include <pdi.h>
#include <pdi/data_reference.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/state.h>

namespace {

using namespace PDI;
using std::cerr;
using std::cout;
using std::endl;
using std::string;
using std::unordered_map;
using std::vector;

struct Hdf5Variable
{
	Value h5var;
	
	Value h5file;
	
	Value select;
	
};


unordered_map<string, Hdf5Variable> outputs;

unordered_map<string, Hdf5Variable> inputs;


/** Reads the plugin config for either inputs or outputs
 * \param conf the configuration node to read
 * \param def_file the default file for HDF5 inputs/outputs
 * \param def_select the default select for HDF5 inputs/outputs
 * \return the array where to store the resulting variables
 */
unordered_map<string, Hdf5Variable> read_config_file( PC_tree_t conf, const Value* dflt_file, const Value* dflt_select)
{
	PDI::Try_pc _;
	unordered_map<string, Hdf5Variable> result;
	
	int nb_hdf5data;
	try {
		nb_hdf5data = len(conf);
	} catch ( const Error& ) {
		return result;
	}
	
	for ( int ii=0; ii<nb_hdf5data; ++ii ) {
		string data_name = to_string(PC_get(conf, "{%d}", ii));
		
		PC_tree_t data_conf = PC_get(conf, "<%d>", ii); // get the node
		
		// set the corresponding HDF5 dataset name
		Value var;
		try {
			var = to_string(PC_get(data_conf, ".var"));
		} catch (const Error&) {
			// the data name is used as the default dataset name
			var = data_name;
		}
		
		// set the HDF5 filename (i.e. where do we write the data)
		Value file;
		try {
			file = to_string(PC_get(data_conf, ".file"));
		} catch (const Error&) {
			if ( dflt_file ) {
				// using default
				file = *dflt_file;
			} else {
				throw;
			}
		}

		// sampling or frequency (i.e. when do we expose)
		Value select;
		try {
			select = to_string(PC_get(data_conf, ".select"));
		} catch (const Error&) {
			if ( dflt_file ) {
				// using default
				select = *dflt_select;
			} else {
				throw;
			}
		}
		
		result.emplace(data_name, Hdf5Variable{var, file, select});
	}
	
	return result;
}


PDI_status_t PDI_declh5_init(PC_tree_t conf, MPI_Comm *)
{
	if ( H5open() < 0 ) { // Failure initializing HDF5
		throw Error{PDI_ERR_PLUGIN, "[PDI/HDF5] Cannot load HDF5 library"};
	}
	
	if ( PC_status(conf) ) {
		throw Error{PDI_ERR_PLUGIN, "[PDI/HDF5] Invalid configuration."};
	}
	
	// default output file if none specified
	Value out_file;
	Value *dflt_out_file;
	try {
		out_file = to_string(PC_get(conf, ".defaults.outputs.file"));
		dflt_out_file = &out_file;
	} catch (...) {
		dflt_out_file = nullptr;
	}
	
	// default output select if none specified
	Value out_select;
	Value *dflt_out_select;
	try {
		out_select = to_string(PC_get(conf, ".defaults.outputs.select"));
		dflt_out_select = &out_select;
	} catch (...) {
		dflt_out_select = nullptr;
	}
	
	outputs = read_config_file(PC_get(conf, ".outputs"), dflt_out_file, dflt_out_select);
	
	// default input file if none specified
	Value in_file;
	Value *dflt_in_file;
	try {
		in_file = to_string(PC_get(conf, ".defaults.inputs.file"));
		dflt_in_file = &in_file;
	} catch (...) {
		dflt_in_file = nullptr;
	}
	
	// default input select if none specified
	Value in_select;
	Value *dflt_in_select;
	try {
		in_select = to_string(PC_get(conf, ".defaults.inputs.select"));
		dflt_in_select = &in_select;
	} catch (...) {
		dflt_in_select = nullptr;
	}
	
	inputs = read_config_file(PC_get(conf, ".inputs"), dflt_in_file, dflt_in_select);
	
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

hid_t h5type(Scalar_datatype ptype) {
	switch (ptype) {
		case PDI_T_INT8: return  H5T_NATIVE_INT8;
		case PDI_T_INT16: return  H5T_NATIVE_INT16;
		case PDI_T_INT32: return  H5T_NATIVE_INT32;
		case PDI_T_INT64: return  H5T_NATIVE_INT64;
		case PDI_T_FLOAT: return  H5T_NATIVE_FLOAT;
		case PDI_T_DOUBLE: return  H5T_NATIVE_DOUBLE;
		case PDI_T_LONG_DOUBLE: return  H5T_NATIVE_LDOUBLE;
		case PDI_T_UNDEF: return H5T_NO_CLASS;
	}
	return H5T_NO_CLASS; //TODO: better handle the error
}

int is_h5_file(const string& filename)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	int fexists = (H5Fis_hdf5(filename.c_str())>0);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
	return fexists;
}

void rm_if_exist(hid_t h5file, const string& dset_name)
{
	H5E_auto2_t old_func;
	void *old_data;
	H5Eget_auto2(H5E_DEFAULT, &old_func, &old_data);
	H5Eset_auto(H5E_DEFAULT, NULL, NULL);
	H5Ldelete(h5file, dset_name.c_str(), H5P_DEFAULT);
	H5Eset_auto(H5E_DEFAULT, old_func, old_data);
}

void write_to_file(Data_ref cref, const string& filename, const string& pathname)
{
	int rank = 0;
	vector<hsize_t> h5sizes;
	vector<hsize_t> h5subsizes;
	vector<hsize_t> h5starts;
	
	Scalar_datatype scalart;
	
	if ( cref.type().kind == PDI_K_ARRAY ) {
		if ( cref.type().c.array->type.kind != PDI_K_SCALAR ){
			throw Error(PDI_ERR_IMPL, "DeclH5 does not suppport recursive array");
		}
		rank = cref.type().c.array->m_dimensions.size();
		for ( auto&& dim: cref.type().c.array->m_dimensions ) {
			h5sizes.emplace_back(dim.m_size);
			h5subsizes.emplace_back(dim.m_subsize);
			h5starts.emplace_back(dim.m_start);
		}
		scalart = cref.type().c.array->type.c.scalar;
	} else {
		scalart = cref.type().c.scalar;
	}
	
	hid_t h5file;
	if ( is_h5_file(filename) ) {
		h5file = H5Fopen(filename.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
	} else {
		h5file = H5Fcreate(filename.c_str(), H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	}
	
	rm_if_exist(h5file, pathname);
	
	hid_t h5fspace = H5Screate_simple(rank, &h5subsizes[0], NULL);
	hid_t h5mspace = H5Screate_simple(rank, &h5sizes[0], NULL);
	if ( rank ) {
		H5Sselect_hyperslab(h5mspace, H5S_SELECT_SET, &h5starts[0], NULL, &h5subsizes[0], NULL );
	}
	hid_t h5lcp = H5Pcreate(H5P_LINK_CREATE);
	H5Pset_create_intermediate_group( h5lcp, 1 );
	hid_t h5set = H5Dcreate( h5file, pathname.c_str(), h5type(scalart),
	                         h5fspace, h5lcp, H5P_DEFAULT, H5P_DEFAULT);
	
	if ( Data_r_ref ref = cref ) {
		H5Dwrite(h5set, h5type(scalart), h5mspace, H5S_ALL, H5P_DEFAULT, ref);
	} else {
		throw Error{PDI_ERR_PLUGIN, "Could not get read access on variable to write it to disk"};
	}
	
	H5Dclose(h5set);
	H5PTclose(h5lcp);
	H5Sclose(h5mspace);
	H5Sclose(h5fspace);
	H5Fclose(h5file);
}

PDI_status_t read_from_file(Data_ref cref, const string& filename, const string& pathname)
{
	int rank = 0;
	vector<hsize_t> h5sizes;
	vector<hsize_t> h5subsizes;
	vector<hsize_t> h5starts;
	
	Scalar_datatype scalart;
	
	if ( cref.type().kind == PDI_K_ARRAY ) {
		if ( cref.type().c.array->type.kind != PDI_K_SCALAR ){
			throw Error(PDI_ERR_IMPL, "DeclH5 does not suppport recursive array");
		}
		rank = cref.type().c.array->m_dimensions.size();
		for ( auto&& dim: cref.type().c.array->m_dimensions ) {
			h5sizes.emplace_back(dim.m_size);
			h5subsizes.emplace_back(dim.m_subsize);
			h5starts.emplace_back(dim.m_start);
		}
		scalart = cref.type().c.array->type.c.scalar;
	} else {
		scalart = cref.type().c.scalar;
	}
	
	/// Open file for read/write
	hid_t file_id = H5Fopen(filename.c_str(), H5F_ACC_RDWR, H5P_DEFAULT);
	if ( file_id<0 ) {
		throw Error{PDI_ERR_SYSTEM, "Unable to open file `%s' for writing", filename.c_str()};
	}
	
	/// Open dataset "pathname" and gets its memspace
	hid_t dataset_id = H5Dopen(file_id, pathname.c_str(), H5P_DEFAULT);
	if( dataset_id < 0 ) {
		H5Fclose(file_id);
		throw Error{PDI_ERR_SYSTEM, "Unable to open dataset `%s' in file `%s' for writing", pathname.c_str(), filename.c_str()};
	}
	
	/// The dataspace from the file is left unmodified (ie get all data from file).
	hid_t dataspace_id = H5Dget_space(dataset_id);
	
	/// Create data representation 
	hid_t memspace = H5Screate_simple(rank, &h5sizes[0], NULL);
	
	/// Extract subspace of data 
	if ( rank ) {
		H5Sselect_hyperslab(memspace, H5S_SELECT_SET, &h5starts[0], NULL, 
		                    &h5subsizes[0], NULL);
	}
	
	/// Read content 
	if ( Data_w_ref ref = cref ) {
		if( H5Dread(dataset_id, h5type(scalart), memspace, dataspace_id, H5P_DEFAULT, ref) ) {
			throw Error{PDI_ERR_SYSTEM, "Could not read from disk"};
		}
	} else {
		throw Error{PDI_ERR_PLUGIN, "Could not get write access on variable to read it from disk"};
	}
	
	H5Sclose(memspace);
	H5Sclose(dataspace_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	return PDI_OK;
}

PDI_status_t PDI_declh5_data(const string& name, Data_ref ref)
{
	auto&& outev = outputs.find(name);
	if ( outev != outputs.end() ) {
		if ( 0 != outev->second.select ) {
			write_to_file(ref, outev->second.h5file, outev->second.h5var);
		}
	}
	
	auto&& inev = inputs.find(name);
	if ( inev != inputs.end() ) {
		if ( 0 != inev->second.select ) {
			read_from_file(ref, inev->second.h5file, inev->second.h5var);
		}
	}
	
	return PDI_OK;
}

} // namespace <anonymous>

PDI_PLUGIN(declh5)
