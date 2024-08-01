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

#include <cstddef>
#include <exception>
#include <iostream>
#include <sstream>
#include <string>
#include <type_traits>
#include <unordered_set>

#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

#include "pdi/context.h"
#include "pdi/data_descriptor.h"
#include "pdi/error.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/plugin.h"
#include "pdi/python/python_ref_wrapper.h"
#include "pdi/python/tools.h"
#include "pdi/ref_any.h"

#include "global_context.h"

namespace {

using namespace PDI;
// template <typename T> using pycls = pybind11::class_<T>;
template <typename T>
using pyenu = pybind11::enum_<T>;
using pyarr = pybind11::array;
// using pylst = pybind11::list;
using pymod = pybind11::module;
using pyobj = pybind11::object;
// using pystr = pybind11::str;
using pytup = pybind11::tuple;
using namespace pybind11::literals;
using std::move;
using std::vector;

/** Check API version of PDI and python
 *
 * \param pyexpected tuple with expected PDI version
 * \return python tulpe with PDI version
 */
pytup pyversion(pytup pyexpected)
{
	Paraconf_wrapper fw;
	
	unsigned long expected_major = pyexpected[0].cast<unsigned long>();
	unsigned long expected_minor = pyexpected[1].cast<unsigned long>();
	unsigned long expected_patch = pyexpected[2].cast<unsigned long>();
	
	if ((expected_major || expected_minor || expected_patch) && (expected_major != PDI_VERSION_MAJOR || expected_minor > PDI_VERSION_MINOR)) {
		throw Plugin_error{
			"Invalid PDI API version: %lu.%lu.%lu, PDI provided version is %lu.%lu.%lu",
			expected_major,
			expected_minor,
			expected_patch,
			PDI_VERSION_MAJOR,
			PDI_VERSION_MINOR,
			PDI_VERSION_PATCH};
	}
	
	return pybind11::make_tuple(PDI_VERSION_MAJOR, PDI_VERSION_MINOR, PDI_VERSION_PATCH);
}

} // namespace

/** Macro that creates entry point in python interpreter
 * \param _pdi name of the module
 * \param m macro argument that defines a variable of type py::module
 */
PYBIND11_MODULE(_pdi, m)
{
	m.attr("__name__") = "pdi._pdi"; // make this a private submodule of pdi
	m.doc() = "PDI python public application API";
	
	pybind11::register_exception<Error>(m, "Error");
	
	pyenu<PDI_inout_t>(m, "Inout").value("IN", PDI_IN).value("OUT", PDI_OUT).value("INOUT", PDI_INOUT).value("NONE", PDI_NONE).export_values();
	
	m.def("version", pyversion, "Checks PDI API version", "version"_a = pybind11::make_tuple(0, 0, 0));
	
	m.def(
	    "init",
	[](char* conf) {
		Paraconf_wrapper fw;
		Global_context::init(PC_parse_string(conf));
	},
	"Initialize PDI"
	    );
	    
	m.def(
	    "finalize",
	[]() {
		Paraconf_wrapper fw;
		Global_context::finalize();
	},
	"Finalize PDI"
	    );
	    
	m.def(
	    "event",
	[](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context().event(name);
	},
	"Triggers a PDI \"event\""
	    );
	    
	m.def(
	    "share",
	[](const char* name, pybind11::array pybuf, PDI_inout_t access) {
		Paraconf_wrapper fw;
		Ref r{
			pybuf.mutable_data(),
			[pybuf](void*) { /* keep pybuf copy to prevent deallocation of the underlying memory */ },
			python_type(pybuf),
			static_cast<bool>(access& PDI_OUT),
			static_cast<bool>(access& PDI_IN)};
		try {
			Global_context::context()[name].share(r, false, false);
		} catch (...) {
			// on error, do not free the data as would be done automatically otherwise
			r.release();
			throw;
		}
	},
	"Shares some data with PDI. The user code should not modify it before a call to either release or reclaim"
	    );
	    
	m.def(
	    "access",
	[](const char* name, PDI_inout_t inout) {
		Paraconf_wrapper fw;
		Data_descriptor& desc = Global_context::context()[name];
		desc.share(desc.ref(), false, false);
		return to_python(desc.ref(), !(inout& PDI_OUT));
	},
	"Requests for PDI to access a data buffer"
	    );
	    
	m.def(
	    "release",
	[](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context()[name].release();
	},
	"Releases ownership of a data shared with PDI. PDI is then responsible to free the associated memory whenever necessary."
	    );
	    
	m.def(
	    "reclaim",
	[](const char* name) {
		Paraconf_wrapper fw;
		Global_context::context()[name].reclaim();
	},
	"Reclaims ownership of a data buffer shared with PDI. PDI does not manage the buffer memory anymore."
	    );
	    
	pybind11::class_<Python_ref_wrapper>(m, "ref")
	.def("__getattribute__", &Python_ref_wrapper::getattribute) // get member
	.def("__setattr__", &Python_ref_wrapper::setattribute) // set member
	.def("__getitem__", &Python_ref_wrapper::getitem); // [] operator
}
