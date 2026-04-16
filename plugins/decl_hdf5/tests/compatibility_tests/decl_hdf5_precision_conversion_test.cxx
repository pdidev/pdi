/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <numeric>
#include <ranges>

#include <hdf5.h>

#include <pdi/testing.h>

class DeclHdf5: public ::PDI::PdiTest
{};

/* Precision conversion with decl_hdf5 
 * data in double precision
 * file dataset in double, float, and int
 */
TEST_F(DeclHdf5, PrecisionConversion)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  N: int
data:
  array: {type: array, size: [$N, $N], subtype: double}
plugins:
  decl_hdf5:   
    - file: d2d_test.h5
      datasets:
        double_ds: {type: array, size: [$N, $N], subtype: double}
      write:
        array:
          dataset: double_ds
    - file: d2f_test.h5
      datasets:
        float_ds: {type: array, size: [$N, $N], subtype: float}
      write:
        array:
          dataset: float_ds
    - file: d2i_test.h5
      datasets:
        int_ds: {type: array, size: [$N, $N], subtype: int}
      write:
        array:
          dataset: int_ds
)=="));

	EXPECT_FALSE(std::filesystem::exists("d2d_test.h5"));
	EXPECT_FALSE(std::filesystem::exists("d2f_test.h5"));
	EXPECT_FALSE(std::filesystem::exists("d2i_test.h5"));

	int const N = 100;
	PDI_expose("N", &N, PDI_OUT);

	auto const test_array = make_a<std::array<double, N * N>>();
	PDI_expose("array", test_array.data(), PDI_OUT);

	EXPECT_TRUE(std::filesystem::exists("d2d_test.h5"));
	EXPECT_TRUE(std::filesystem::exists("d2f_test.h5"));
	EXPECT_TRUE(std::filesystem::exists("d2i_test.h5"));

	hid_t file_id = H5Fopen("d2d_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	hid_t dataset_id = H5Dopen2(file_id, "/double_ds", H5P_DEFAULT);
	hid_t type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_IEEE_F64LE));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	file_id = H5Fopen("d2f_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/float_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_IEEE_F32LE));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	file_id = H5Fopen("d2i_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/int_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_STD_I32LE));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}
