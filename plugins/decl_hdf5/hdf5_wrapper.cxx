/*******************************************************************************
 * Copyright (C) 2015-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <pdi/record_datatype.h>

#include "hdf5_wrapper.h"

using PDI::Array_datatype;
using PDI::Datatype_sptr;
using PDI::Error;
using PDI::Impl_error;
using PDI::System_error;
using PDI::Type_error;
using PDI::Scalar_datatype;
using PDI::Record_datatype;
using PDI::Scalar_kind;
using std::dynamic_pointer_cast;
using std::make_tuple;
using std::move;
using std::string;
using std::tie;
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

hid_t get_h5_type(Datatype_sptr type)
{
	if (auto&& record_type = dynamic_pointer_cast<const Record_datatype>(type)) {
		hid_t h5_type = H5Tcreate (H5T_COMPOUND, record_type->buffersize());
		for (const auto& member : record_type->members()) {
			H5Tinsert(h5_type, member.name().c_str(), member.displacement(), get_h5_type(member.type()));
		}
		return h5_type;
	} else if (dynamic_pointer_cast<const Array_datatype>(type)) {
		std::vector<hsize_t> dims;
		auto&& subtype = type;
		while (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(subtype)) {
			dims.emplace_back(array_type->size());
			subtype = array_type->subtype();
		}
		return H5Tarray_create2(get_h5_type(subtype), dims.size(), &dims[0]);
	} else if (auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(type)) {
		switch (scalar_type->kind()) {
		case Scalar_kind::UNSIGNED: {
			switch (scalar_type->datasize()) {
			case 1: return H5T_NATIVE_UINT8;
			case 2: return H5T_NATIVE_UINT16;
			case 4: return H5T_NATIVE_UINT32;
			case 8: return H5T_NATIVE_UINT64;
			default: throw Type_error{"Invalid size for HDF5 signed: #{}", scalar_type->datasize()};
			}
		}
		case Scalar_kind::SIGNED: {
			switch (scalar_type->datasize()) {
			case 1: return H5T_NATIVE_INT8;
			case 2: return H5T_NATIVE_INT16;
			case 4: return H5T_NATIVE_INT32;
			case 8: return H5T_NATIVE_INT64;
			default: throw Type_error{"Invalid size for HDF5 unsigned: #{}", scalar_type->datasize()};
			}
		}
		case Scalar_kind::FLOAT: {
			switch (scalar_type->datasize()) {
			case 4:  return H5T_NATIVE_FLOAT;
			case 8:  return H5T_NATIVE_DOUBLE;
			case 16: return H5T_NATIVE_LDOUBLE;
			default: throw Type_error{"Invalid size for HDF5 float: #{}", scalar_type->datasize()};
			}
		}
		default: throw Type_error{"Invalid type for HDF5: #{}", static_cast<uint8_t>(scalar_type->kind())};
		}
	} else {
		throw Impl_error{"Unexpected type in HDF5"};
	}
}

} // namespace <anonymous>

namespace decl_hdf5 {

void handle_hdf5_err(const char* message)
{
	string h5_errmsg;
	H5Ewalk2(H5E_DEFAULT, H5E_WALK_UPWARD, raii_walker, &h5_errmsg);
	if ( h5_errmsg.empty() ) h5_errmsg = "HDF5 error";
	
	if ( !message ) message = "";
	throw System_error{"{} {}", message, h5_errmsg};
}

tuple<Raii_hid, Raii_hid> space(Datatype_sptr type, bool dense)
{
	//check if outer type is an array
	if (dynamic_pointer_cast<const Array_datatype>(type)) {
		int rank = 0;
		vector<hsize_t> h5_size;
		vector<hsize_t> h5_subsize;
		vector<hsize_t> h5_start;
		Datatype_sptr subtype = type;
		
		while (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(subtype)) {
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
			subtype = array_type->subtype();
		}
		if (!subtype->dense()) {
			throw Type_error{"The top array datatype is the only one that can be sparse in dataset"};
		}
		
		Raii_hid h5_space = make_raii_hid(H5Screate_simple(rank, &h5_size[0], NULL), H5Sclose);
		if ( 0>H5Sselect_hyperslab(h5_space, H5S_SELECT_SET, &h5_start[0], NULL, &h5_subsize[0], NULL) ) handle_hdf5_err();
		
		return make_tuple(move(h5_space), Raii_hid{get_h5_type(subtype), H5Tclose});
	} else {
		if (!type->dense()) {
			throw Type_error{"The top array datatype is the only one that can be sparse in dataset"};
		}
		return make_tuple(make_raii_hid(H5Screate(H5S_SCALAR), H5Sclose), make_raii_hid(get_h5_type(type), H5Tclose));
	}
}

} // namespace decl_hdf5
