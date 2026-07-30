/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2024-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <filesystem>
#include <iostream>
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

class DeclNetcdfCheckType: public ::PDI::PdiTest
{};


/*
 * Name:                DeclNetcdfCheckType.IntReadMismatch
 *
 * Description:         Tests write and read of int with type mismatch
 */
TEST_F(DeclNetcdfCheckType, IntReadMismatch)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  int_in: int32
  int_out: int64
plugins:
  decl_netcdf:
    - file: 'test_int_read.nc'
      on_event: write_data
      write:
        int_in:
          variable: scalar_int32
    - file: 'test_int_read.nc'
      on_event: read_data
      read:
        int_out:
          variable:
            scalar_int32
)=="));

	// write data
	int32_t int_in = 42;
	PDI_multi_expose("write_data", "int_in", &int_in, PDI_OUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("test_int_read.nc"));

	// read data
	int64_t int_out = -1;

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::StrEq("Error while triggering event `read_data': "
				"Type_error: Decl_netcdf plugin: Datatype mismatch (with size): "
				"read 'scalar_int32' of size 4 for a buffer of size 8")
		)
	);

	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "int_out", &int_out, PDI_IN, NULL));
}

/*
 * Name:                DeclNetcdfCheckType.FloatReadMismatch
 *
 * Description:         Tests write and read of float/double with type mismatch
 */
TEST_F(DeclNetcdfCheckType, FloatReadMismatch)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  var_in: float
  var_out: double
plugins:
  decl_netcdf:
    - file: 'test_float_read.nc'
      on_event: write_data
      write:
        var_in:
          variable: scalar_float
    - file: 'test_float_read.nc'
      on_event: read_data
      read:
        var_out:
          variable: scalar_float
)=="));

	// write data
	float var_in = 12.34;
	PDI_multi_expose("write_data", "var_in", &var_in, PDI_OUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("test_float_read.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::StrEq("Error while triggering event `read_data': "
				"Type_error: Decl_netcdf plugin: Datatype mismatch (with size): "
				"read 'scalar_float' of size 4 for a buffer of size 8")
		)
	);

	// read data
	double var_out = -1.0;
	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "var_out", &var_out, PDI_IN, NULL));
}

/*
 * Name:                DeclNetcdfCheckType.ReadDataNotDefinedInYaml
 *
 * Description:         Tests write and read of float/double with type mismatch
 */
TEST_F(DeclNetcdfCheckType, ReadDataNotDefinedInYaml)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  var_in: float
plugins:
  decl_netcdf:
    - file: 'test_float_read_data_not_defined.nc'
      on_event: write_data
      write:
        var_in:
          variable: scalar_float
    - file: 'test_float_read_data_not_defined.nc'
      on_event: read_data
      read:
        var_out:
          variable: scalar_float
)=="));

	// write data
	float var_in = 15.34;
	PDI_multi_expose("write_data", "var_in", &var_in, PDI_OUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("test_float_read_data_not_defined.nc"));

	EXPECT_CALL(
		*this,
		PdiError(
			testing::Eq(PDI_ERR_TYPE),
			testing::AllOf(
				testing::StartsWith("Error while triggering event `read_data': "
					"Type_error: Can not read `scalar_float' : "),
				testing::HasSubstr("The exposed data to PDI has an undefined type."),
				testing::HasSubstr("Possible reason: The exposed data, used to read the variable `scalar_float', "
					"is not defined in yaml (meta)data section.")
			)
		)
	);

	// read data
	float var_out = -1.0;
	EXPECT_EQ(PDI_ERR_TYPE, PDI_multi_expose("read_data", "var_out", &var_out, PDI_IN, NULL));
}

