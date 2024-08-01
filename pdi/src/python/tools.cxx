/*******************************************************************************
* Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
* Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <cstdint>
#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/data_descriptor.h"
#include "pdi/datatype.h"
#include "pdi/error.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/pointer_datatype.h"
#include "pdi/record_datatype.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "pdi/python/python_ref_wrapper.h"
#include "pdi/python/tools.h"

namespace PDI {

using namespace pybind11::literals;
using std::dynamic_pointer_cast;
using std::vector;

namespace {

/** Tells if type has record inside
 *  \param type type to check
 *  \return true if has record inside, false otherwise
 */
bool has_record_inside(Datatype_sptr type)
{
	if (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(type)) {
		return has_record_inside(array_type->subtype());
	} else if (auto&& pointer_type = dynamic_pointer_cast<const Pointer_datatype>(type)) {
		return has_record_inside(pointer_type->subtype());
	} else if (dynamic_pointer_cast<const Record_datatype>(type)) {
		return true;
	} else {
		return false;
	}
}

} // namespace

pybind11::object to_python(Ref r, bool force_const)
{
	if (has_record_inside(r.type())) {
		return pybind11::cast(Python_ref_wrapper{r});
	}
	
	ssize_t ndim = 0;
	vector<ssize_t> starts;
	vector<ssize_t> shape;
	vector<ssize_t> strides;
	
	auto&& subtype = r.type();
	while (auto&& array_type = dynamic_pointer_cast<const Array_datatype>(subtype)) {
		shape.emplace_back(array_type->subsize());
		if (ndim) strides.emplace_back(array_type->size());
		starts.emplace_back(array_type->start());
		++ndim;
		subtype = array_type->subtype();
	}
	auto&& scalar_type = dynamic_pointer_cast<const Scalar_datatype>(subtype);
	if (ndim) strides.emplace_back(scalar_type->buffersize());
	
	pybind11::dtype pytype;
	switch (scalar_type->kind()) {
	case Scalar_kind::FLOAT: {
		switch (scalar_type->datasize()) {
		case sizeof(float):
			pytype = pybind11::dtype::of<float>();
			break;
		case sizeof(double):
			pytype = pybind11::dtype::of<double>();
			break;
		default:
			throw Type_error{"Unable to pass {} bytes floating point value to python", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::SIGNED: {
		switch (scalar_type->datasize()) {
		case sizeof(int8_t):
			pytype = pybind11::dtype::of<int8_t>();
			break;
		case sizeof(int16_t):
			pytype = pybind11::dtype::of<int16_t>();
			break;
		case sizeof(int32_t):
			pytype = pybind11::dtype::of<int32_t>();
			break;
		case sizeof(int64_t):
			pytype = pybind11::dtype::of<int64_t>();
			break;
		default:
			throw Type_error{"Unable to pass {} bytes integer value to python", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::UNSIGNED: {
		switch (scalar_type->datasize()) {
		case sizeof(uint8_t):
			pytype = pybind11::dtype::of<uint8_t>();
			break;
		case sizeof(uint16_t):
			pytype = pybind11::dtype::of<uint16_t>();
			break;
		case sizeof(uint32_t):
			pytype = pybind11::dtype::of<uint32_t>();
			break;
		case sizeof(uint64_t):
			pytype = pybind11::dtype::of<uint64_t>();
			break;
		default:
			throw Type_error{"Unable to pass {} bytes unsigned integer value to python", scalar_type->datasize()};
		}
	} break;
	default:
		throw Type_error{"Unable to pass value of unexpected type to python"};
	}
	
	ssize_t cumulated_stride = 1;
	for (auto&& stride = strides.rbegin(); stride != strides.rend(); ++stride) {
		*stride *= cumulated_stride;
		cumulated_stride = *stride;
	}
	
	ssize_t offset = 0;
	for (int ii = 0; ii < ndim; ++ii) {
		offset += starts[ii] * strides[ii];
	}
	
	if ( !force_const ) {
		if ( Ref_w r_w{r} ) {
			return pybind11::array{pytype, std::move(shape), std::move(strides), static_cast<uint8_t*>(r_w.get())+offset, pybind11::capsule{new Ref{r}, [](void* pr)
			{
				delete static_cast<Ref*>(pr);
			}}};
		}
	}
	if (Ref_r r_r{r}) {
		return pybind11::array{pytype, std::move(shape), std::move(strides), static_cast<const uint8_t*>(r_r.get())+offset, pybind11::capsule{new Ref{r}, [](void* pr)
		{
			delete static_cast<Ref*>(pr);
		}}};
	}
	return pybind11::none();
}

Datatype_sptr python_type(const pybind11::array& a)
{
	//TODO: handle non C-order arrays
	vector<size_t> sizes(a.ndim());
	if (a.ndim()) sizes[0] = a.shape(0);
	for (int ii = 1; ii < a.ndim(); ++ii) {
		sizes[ii] = a.strides(ii - 1) / a.strides(ii);
	}
	Scalar_kind k;
	switch (a.dtype().kind()) {
	case 'i':
		k = Scalar_kind::SIGNED;
		break;
	case 'u':
	case 'b':
		k = Scalar_kind::UNSIGNED;
		break;
	case 'f':
		k = Scalar_kind::FLOAT;
		break;
	default:
		throw Impl_error{"Unexpected python type descriptor: {}", a.dtype().kind()};
	}
	
	Datatype_sptr result = Scalar_datatype::make(k, static_cast<size_t>(a.dtype().itemsize()));
	for (int ii = a.ndim() - 1; ii >= 0; --ii) {
		result = Array_datatype::make(move(result), sizes[ii], 0, static_cast<size_t>(a.shape(ii)));
	}
	return result;
}

} // namespace PDI
