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

#include <string>
#include <vector>

#include <pdi/array_datatype.h>
#include <pdi/datatype.h>
#include <pdi/error.h>
#include <pdi/scalar_datatype.h>

#include "hdf5_wrapper.h"

using PDI::Array_datatype;
using PDI::Datatype;
using PDI::Error;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using std::make_tuple;
using std::move;
using std::string;
using std::tuple;
using std::vector;

namespace {

herr_t raii_walker(unsigned n, const H5E_error2_t* err_desc, void* client_data)
{
	string& result = *(static_cast<string*>(client_data));
	if ( n >= 1 ) result += "\n * ";
	result += err_desc->desc;
	return 0;
}

} // namespace <anonymous>

namespace decl_hdf5 {

void handle_hdf5_err(const char* message)
{
	string h5_errmsg;
	H5Ewalk2(H5E_DEFAULT, H5E_WALK_UPWARD, raii_walker, &h5_errmsg);
	if ( h5_errmsg.empty() ) h5_errmsg = "HDF5 error";
	
	if ( !message ) message = "";
	throw Error{PDI_ERR_SYSTEM, "%s%s", message, h5_errmsg.c_str()};
}

tuple<Raii_hid, Raii_hid> space(const Datatype& type, bool dense)
{
	int rank = 0;
	vector<hsize_t> h5_size;
	vector<hsize_t> h5_subsize;
	vector<hsize_t> h5_start;
	const Datatype* subtype = &type;
	
	while (auto&& array_type = dynamic_cast<const Array_datatype*>(subtype)) {
		++rank;
		if ( dense ) {
			h5_size.emplace_back(array_type->subsize());
			h5_subsize.emplace_back(array_type->subsize());
			h5_start.emplace_back(0);
		} else {
			h5_size.emplace_back(array_type->size());
			h5_subsize.emplace_back(array_type->subsize());
			h5_start.emplace_back(array_type->start());
		}
		subtype = &array_type->subtype();
	}
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

} // namespace decl_hdf5
