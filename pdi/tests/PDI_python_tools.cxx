// SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

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
using PDI::Datatype_sptr;
using PDI::Global_context;
using PDI::Paraconf_wrapper;
using PDI::Ref;

TEST(Python, ref_to_python)
{
	pybind11::initialize_interpreter();

	Paraconf_wrapper _;
	unique_ptr<Context> ctx{new Global_context{PC_parse_string("logging: off")}};

	Datatype_sptr type
		= ctx->datatype(PC_parse_string("{type: array, subtype: int, size: [10, 6, 3], subsize: [3, 2, 1], start: [3, 2, 1]}"))->evaluate(*ctx);

	int data[10][6][3];

	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 6; j++) {
			for (int k = 0; k < 3; k++) {
				data[i][j][k] = i * 18 + j * 3 + k;
			}
		}
	}

	Ref test_ref{data, [](void*) {}, move(type), true, true};
	{
		pybind11::dict pyscope = pybind11::module::import("__main__").attr("__dict__");
		pyscope["py_data"] = to_python(test_ref);

		pybind11::exec("assert py_data[0][0][0] == 61 and  py_data[0][1][0] == 64", pyscope);
		pybind11::exec("assert py_data[1][0][0] == 79 and  py_data[1][1][0] == 82", pyscope);
		pybind11::exec("assert py_data[2][0][0] == 97 and  py_data[2][1][0] == 100", pyscope);
	}
	pybind11::finalize_interpreter();
}
