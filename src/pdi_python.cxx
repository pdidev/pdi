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

/** \file pdi.c
* Implementation of the PDI public API functions.
**/

#include "config.h"

#include <cstddef>
#include <exception>
#include <iostream>
#include <string>
#include <sstream>
#include <type_traits>
#include <unordered_set>

#include <pybind11/pybind11.h>
#include <pybind11/numpy.h>

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


namespace {

using namespace PDI;
template <typename T> using pycls = pybind11::class_<T>;
template <typename T> using pyenu = pybind11::enum_<T>;
using pyarr = pybind11::array;
using pylst = pybind11::list;
using pymod = pybind11::module;
using pyobj = pybind11::object;
using pystr = pybind11::str;
using pytup = pybind11::tuple;
using namespace pybind11::literals;
using std::move;
using std::vector;


pytup pyversion(pytup pyexpected)
{
	Paraconf_wrapper fw;
	
	unsigned long expected_major = pyexpected[0].cast<unsigned long>();
	unsigned long expected_minor = pyexpected[1].cast<unsigned long>();
	unsigned long expected_patch = pyexpected[2].cast<unsigned long>();
	
	if (
	    ( expected_major || expected_minor || expected_patch )
	    && (
	        expected_major != PDI_VERSION_MAJOR
	        || expected_minor > PDI_VERSION_MINOR
	    )
	) {
		throw Error{
			PDI_ERR_PLUGIN,
			"Invalid PDI API version: %lu.%lu.%lu, PDI provided version is %lu.%lu.%lu",
			expected_major,
			expected_minor,
			expected_patch,
			PDI_VERSION_MAJOR,
			PDI_VERSION_MINOR,
			PDI_VERSION_PATCH
		};
	}
	
	return pybind11::make_tuple(PDI_VERSION_MAJOR, PDI_VERSION_MINOR, PDI_VERSION_PATCH);
}

pybind11::array pycast(Ref r, void* data)
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
	
	int64_t* ptr = static_cast<int64_t*>(data);
	for ( int ii=0; ii<ndim; ++ii ) {
		ptr += starts[ii]*strides[ii];
	}
	
	Ref* pr = new Ref{r};
	return pybind11::array{pytype, move(shape), move(strides), ptr, pybind11::capsule{pr, [](void* pr)
	{
		delete static_cast<Ref*>(pr);
	}}};
}

Datatype_uptr type(pybind11::array& a)
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

} // namespace <anonymous>

PYBIND11_MODULE(pdi, m)
{
	m.doc() = "PDI python public application API";
	
	static pybind11::exception<Error> exc(m, "Error");
	pybind11::register_exception_translator([](std::exception_ptr p) {
		try {
			if (p) std::rethrow_exception(p);
		} catch (const Error& e) {
			exc(e.what());
		}
	});
	
	pyenu<PDI_inout_t>(m, "Inout")
	.value("IN", PDI_IN)
	.value("OUT", PDI_OUT)
	.value("INOUT", PDI_INOUT)
	.value("NONE", PDI_NONE)
	.export_values();
	
	m.def("version", pyversion, "Checks PDI API version", "version"_a = pybind11::make_tuple(0,0,0));
	
	m.def("init", [](pyobj pycomm, char* conf) {
		Paraconf_wrapper fw;
		pymod pyMPI = pymod::import("mpi4py.MPI");
		MPI_Comm comm = MPI_Comm_f2c(pycomm.attr("py2f")().cast<MPI_Fint>());
//		MPI_Comm comm = *reinterpret_cast<MPI_Comm*>(pyMPI.attr("_addressof")(pycomm).cast<uintptr_t>());
		Global_context::init(PC_parse_string(conf), &comm);
		return pyMPI.attr("Comm").attr("f2py")(MPI_Comm_c2f(comm));
	}, "Initialize PDI");
	
	m.def("event", [](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context().event(name);
	}, "Triggers a PDI \"event\"");
	
	m.def("share", [](const char* name, pybind11::array pybuf, PDI_inout_t access) {
		Paraconf_wrapper fw;
		Ref r{
			pybuf.mutable_data(),
			[pybuf](void*){/* we just keep a pybuf copy to prevent dealloc */},
			type(pybuf),
			static_cast<bool>(access & PDI_OUT),
			static_cast<bool>(access & PDI_IN)
		};
		try {
			Global_context::context()[name].share(r, false, false);
		} catch (...) {
			// on error, do not free the data as would be done automatically otherwise
			r.release();
			throw;
		}
	}, "Shares some data with PDI. The user code should not modify it before a call to either release or reclaim");
	
	m.def("access", [](const char* name, PDI_inout_t inout) {
		Paraconf_wrapper fw;
		Data_descriptor& desc = Global_context::context()[name];
		pyarr result = pycast(desc.ref(), desc.share(desc.ref(), inout & PDI_IN, inout & PDI_OUT));
		if (!(inout & PDI_OUT)) pybind11::detail::array_descriptor_proxy(result.ptr())->flags &= ~pybind11::detail::npy_api::NPY_ARRAY_WRITEABLE_;
		return result;
	}, "Requests for PDI to access a data buffer");
	
	m.def("release", [](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context()[name].release();
	}, "Releases ownership of a data shared with PDI. PDI is then responsible to free the associated memory whenever necessary.");
	
	m.def("reclaim", [](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context()[name].reclaim();
	}, "Reclaims ownership of a data buffer shared with PDI. PDI does not manage the buffer memory anymore.");
}
