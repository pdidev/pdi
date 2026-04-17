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

#include <concepts>
#include <filesystem>

#include <hdf5.h>

#include <pdi/testing.h>

class DeclHdf5: public ::PDI::PdiTest
{};

template <std::floating_point T, size_t N> // Only for floating point values
bool compare_2dfields(const std::array<std::array<double, N>, N>& d_data, const std::array<std::array<T, N>, N>& read_data)
{
	for (size_t i = 0; i < N; ++i) {
		for (size_t j = 0; j < N; ++j) {
			const double diff = std::abs(d_data[i][j] - static_cast<double>(read_data[i][j]));
			if (diff > std::numeric_limits<T>::epsilon()) {
				return false;
			}
		}
	}
	return true;
}

template <typename T, size_t N>
requires std::is_integral_v<T> // Only for integer values
bool compare_2dfields(const std::array<std::array<double, N>, N>& d_data, const std::array<std::array<T, N>, N>& read_data)
{
	for (size_t i = 0; i < N; ++i) {
		for (size_t j = 0; j < N; ++j) {
			if (static_cast<T>(std::trunc(d_data[i][j])) != read_data[i][j]) {
				return false;
			}
		}
	}
	return true;
}

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
	auto read_double_array = make_a<std::array<std::array<double, N>, N>>();

	herr_t status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_double_array.data());

	EXPECT_TRUE(compare_2dfields<double>(test_array, read_double_array));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	// read simple precision dataset and compare
	file_id = H5Fopen("d2f_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/float_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_IEEE_F32LE));
	auto read_float_array = make_a<std::array<std::array<float, N>, N>>();

	status = H5Dread(dataset_id, H5T_NATIVE_FLOAT, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_float_array.data());

	EXPECT_TRUE(compare_2dfields<float>(test_array, read_float_array));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);

	// read ingeger dataset and compare
	file_id = H5Fopen("d2i_test.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "/int_ds", H5P_DEFAULT);
	type_id = H5Dget_type(dataset_id);

	EXPECT_TRUE(H5Tequal(type_id, H5T_STD_I32LE));
	auto read_int_array = make_a<std::array<std::array<int, N>, N>>();

	status = H5Dread(dataset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, read_int_array.data());

	EXPECT_TRUE(compare_2dfields<int>(test_array, read_int_array));

	H5Tclose(type_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}
