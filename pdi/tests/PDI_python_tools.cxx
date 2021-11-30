/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <memory>

#include <pybind11/embed.h>

#include <gtest/gtest.h>

#include <pdi/pdi_fwd.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/python/tools.h>
#include "global_context.h"

using std::unique_ptr;

using PDI::Context;
using PDI::Global_context;
using PDI::Paraconf_wrapper;
using PDI::Datatype_uptr;
using PDI::Ref;



TEST(Python, ref_to_python)
{
	pybind11::initialize_interpreter();

	Paraconf_wrapper _;
	unique_ptr<Context> ctx {new Global_context{PC_parse_string("logging: off")}};

	Datatype_uptr type = ctx->datatype(PC_parse_string("{type: array, subtype: int, size: [10, 6, 3], subsize: [3, 2, 1], start: [3, 2, 1]}"))->evaluate(*ctx);

	int data[10][6][3];

	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 6; j++) {
			for (int k = 0; k < 3; k++) {
				data[i][j][k] = i * 18 + j * 3 + k;
			}
		}
	}

	Ref test_ref {data, [](void*){}, move(type), true, true};
	{
		pybind11::dict pyscope = pybind11::module::import("__main__").attr("__dict__");
		pyscope["py_data"] = to_python(test_ref);

		pybind11::exec("assert py_data[0][0][0] == 61 and  py_data[0][1][0] == 64", pyscope);
		pybind11::exec("assert py_data[1][0][0] == 79 and  py_data[1][1][0] == 82", pyscope);
		pybind11::exec("assert py_data[2][0][0] == 97 and  py_data[2][1][0] == 100", pyscope);
	}
	pybind11::finalize_interpreter();
}
