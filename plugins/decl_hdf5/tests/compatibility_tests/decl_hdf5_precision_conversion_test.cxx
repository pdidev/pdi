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

	static constexpr size_t const N = 100;
	PDI_expose("N", &N, PDI_OUT);

	auto const test_array = make_a<std::array<std::array<double, N>, N>>();
	PDI_expose("array", test_array.data(), PDI_OUT);

	EXPECT_TRUE(std::filesystem::exists("d2d_test.h5"));
	EXPECT_TRUE(std::filesystem::exists("d2f_test.h5"));
	EXPECT_TRUE(std::filesystem::exists("d2i_test.h5"));

	// read double precision dataset and compare
	hid_t file_id = H5Fopen("d2d_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	hid_t dataset_id = H5Dopen2(file_id, "/double_ds", H5P_DEFAULT);
	hid_t type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_IEEE_F64LE));
	std::array<std::array<double, N>, N> read_double_array;

	herr_t status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_double_array.data());
	ASSERT_GE(status, 0);

	EXPECT_EQ(test_array, read_double_array);

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	// read simple precision dataset and compare
	file_id = H5Fopen("d2f_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/float_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_IEEE_F32LE));
	std::array< std::array<float, N>, N > read_float_array;

	status = H5Dread(dataset_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_float_array.data());
	ASSERT_GE(status, 0);

	for (auto i = 0; i < N; i++) {
		EXPECT_THAT(read_float_array[i], ::testing::Pointwise(::testing::FloatEq(), test_array[i]));
	}

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	// read ingeger dataset and compare
	file_id = H5Fopen("d2i_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/int_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_STD_I32LE));
	std::array< std::array<int, N>, N > read_int_array;

	status = H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_int_array.data());
	ASSERT_GE(status, 0);

	// checking ...
	// working on it

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}
