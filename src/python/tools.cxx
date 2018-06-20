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

#include "config.h"

#include <vector>

#include "pdi/array_datatype.h"
#include "pdi/context.h"
#include "pdi/datatype.h"
#include "pdi/data_descriptor.h"
#include "pdi/error.h"
#include "pdi/global_context.h"
#include "pdi/logger.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/ref_any.h"
#include "pdi/scalar_datatype.h"

#include "pdi/python/tools.h"


namespace PDI {

using namespace pybind11::literals;
using std::move;
using std::vector;


pybind11::array to_python(Ref r)
{
	ssize_t ndim = 0;
	vector<ssize_t> starts;
	vector<ssize_t> shape;
	vector<ssize_t> strides;
	
	const Datatype* subtype = &r.type();
	while (auto&& array_type = dynamic_cast<const Array_datatype*>(subtype)) {
		shape.emplace_back(array_type->subsize());
		if ( ndim ) strides.emplace_back(array_type->size());
		starts.emplace_back(array_type->start());
		++ndim;
		subtype = &array_type->subtype();
	}
	auto&& scalar_type = dynamic_cast<const Scalar_datatype*>(subtype);
	if ( ndim ) strides.emplace_back(scalar_type->buffersize());
	
	pybind11::dtype pytype;
	switch (scalar_type->kind()) {
	case Scalar_kind::FLOAT: {
		switch (scalar_type->datasize()) {
		case sizeof(float): pytype = pybind11::dtype::of<float>(); break;
		case sizeof(double): pytype = pybind11::dtype::of<double>(); break;
		default: throw Error{PDI_ERR_TYPE, "Unable to pass %d bytes floating point value to python", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::SIGNED: {
		switch (scalar_type->datasize()) {
		case sizeof(int8_t): pytype = pybind11::dtype::of<int8_t>(); break;
		case sizeof(int16_t): pytype = pybind11::dtype::of<int16_t>(); break;
		case sizeof(int32_t): pytype = pybind11::dtype::of<int32_t>(); break;
		case sizeof(int64_t): pytype = pybind11::dtype::of<int64_t>(); break;
		default: throw Error{PDI_ERR_TYPE, "Unable to pass %d bytes integer value to python", scalar_type->datasize()};
		}
	} break;
	case Scalar_kind::UNSIGNED: {
		switch (scalar_type->datasize()) {
		case sizeof(uint8_t): pytype = pybind11::dtype::of<uint8_t>(); break;
		case sizeof(uint16_t): pytype = pybind11::dtype::of<uint16_t>(); break;
		case sizeof(uint32_t): pytype = pybind11::dtype::of<uint32_t>(); break;
		case sizeof(uint64_t): pytype = pybind11::dtype::of<uint64_t>(); break;
		default: throw Error{PDI_ERR_TYPE, "Unable to pass %d bytes unsigned integer value to python", scalar_type->datasize()};
		}
	} break;
	default: throw Error{PDI_ERR_TYPE, "Unable to pass value of unexpected type to python"};
	}
	
	ssize_t cumulated_stride = 1;
	for (auto&& stride = strides.rbegin(); stride != strides.rend(); ++stride) {
		*stride *= cumulated_stride;
		cumulated_stride = *stride;
	}
	
	int64_t* ptr;
	if (Ref_w r_w{r}) {
		ptr = static_cast<int64_t*>(r_w.get());
	} else if (Ref_r r_r{r}) {
		ptr = static_cast<int64_t*>(const_cast<void*>(r_r.get()));
		//      pybind11::detail::array_descriptor_proxy(result.ptr())->flags &= ~pybind11::detail::npy_api::NPY_ARRAY_WRITEABLE_;
	}
	for ( int ii=0; ii<ndim; ++ii ) {
		ptr += starts[ii]*strides[ii];
	}
	
	Ref* pr = new Ref{r};
	pybind11::array result = pybind11::array{pytype, move(shape), move(strides), ptr, pybind11::capsule{pr, [](void* pr)
	{
		delete static_cast<Ref*>(pr);
	}}};
	return result;
}

Datatype_uptr python_type(pybind11::array& a)
{
	//TODO: handle non C-order arrays
	vector<size_t> sizes(a.ndim());
	if (a.ndim()) sizes[0] = a.shape(0);
	for ( int ii=1; ii<a.ndim(); ++ii ) {
		sizes[ii] = a.strides(ii-1)/a.strides(ii);
	}
	Scalar_kind k;
	switch (a.dtype().kind()) {
	case 'c': case 'b': case '?': case 'h': case 'i': case 'l': case 'q': case 'n':
		k = Scalar_kind::SIGNED;
		break;
	case 'B': case 'H': case 'I': case 'L': case 'Q': case 'N':
		k = Scalar_kind::UNSIGNED;
		break;
	case 'e': case 'f': case 'd': case 'g':
		k = Scalar_kind::FLOAT;
		break;
	case 'P':
		k = Scalar_kind::ADDRESS;
		break;
	default:
		throw Error{PDI_ERR_IMPL, "Unexpected python type descriptor: %c", a.dtype().kind()};
	}
	
	Datatype_uptr result{new Scalar_datatype{k, static_cast<size_t>(a.dtype().itemsize())}};
	for ( int ii=a.ndim()-1; ii>=0; --ii ) {
		result.reset(new Array_datatype{move(result), sizes[ii], 0, static_cast<size_t>(a.shape(ii))});
	}
	return result;
}

} // namespace PDI
