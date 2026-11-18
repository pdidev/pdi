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

#include <cstdlib>
#include <unistd.h>

#include <filesystem>
#include <iostream>
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

class GdamarisMultiDim: public ::PDI::PdiTest
{};

TEST_F(GdamarisMultiDim, 2DwithGhost)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { damaris_nn: {size: 2, type: array, subtype: int64 } }
data:
  damaris_values: {size: ['$damaris_nn[0]','$damaris_nn[1]'], type: array, subtype: int}
  pdi_values: {size: ['$damaris_nn[0]','$damaris_nn[1]'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './HDF5_files/damaris_2D_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
    - file: 'data_2D_without_ghost.h5'
      on_event: read_pdi
      read:
        pdi_values:
          dataset: int_values
)=="));

	int const dim = 2;

	std::string exec_name = "damaris_write_2D_data";
	std::string yaml_file = "test_write_2D_data.yml";
	std::string run_command = "mpirun -np 2 ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	// dataset: int_values
	std::string filename = "./HDF5_files/damaris_2D_It0.h5";
	ASSERT_TRUE(std::filesystem::exists(filename)) << filename + " doesn't exist.";

	std::string dataset_name = "int_values";
	std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename;
	+" > /dev/null 2>&1";

	int run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	///
	std::array<int, dim> local_size_with_ghost = {10, 7};
	std::array<int, dim> ghost_left = {2, 1};
	std::array<int, dim> ghost_right = {0, 3};
	std::array<int64_t, dim> global_size_expected = {};

	for (int ii = 0; ii < dim; ++ii) {
		global_size_expected[ii] = local_size_with_ghost[ii] - ghost_left[ii] - ghost_right[ii];
	}

	std::array<int64_t, dim> global_size = {-1, -1};
	PDI_expose("damaris_nn", global_size.data(), PDI_INOUT); // get global size
	ASSERT_EQ(global_size_expected, global_size);

	int total_size = global_size_expected[0] * global_size_expected[1];
	std::vector<int> pdi_values(total_size);
	std::vector<int> damaris_values(total_size);

	PDI_multi_expose("read_pdi", "pdi_values", pdi_values.data(), PDI_IN, "damaris_values", damaris_values.data(), PDI_IN, NULL);

	ASSERT_EQ(pdi_values, damaris_values) << "Error in the reading value";
}

TEST_F(GdamarisMultiDim, 3DwithGhost)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { damaris_nn: {size: 3, type: array, subtype: int64 } }
data:
  damaris_values: {size: ['$damaris_nn[0]','$damaris_nn[1]','$damaris_nn[2]'], type: array, subtype: int}
  pdi_values: {size: ['$damaris_nn[0]','$damaris_nn[1]','$damaris_nn[2]'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './HDF5_files/damaris_3D_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
    - file: 'data_3D_without_ghost.h5'
      on_event: read_pdi
      read:
        pdi_values:
          dataset: int_values
)=="));

	int const dim = 3;

	std::string exec_name = "damaris_write_3D_data";
	std::string yaml_file = "test_write_3D_data.yml";
	std::string run_command = "mpirun -np 2 ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	// dataset: int_values
	std::string filename = "./HDF5_files/damaris_3D_It0.h5";
	ASSERT_TRUE(std::filesystem::exists(filename)) << filename + " doesn't exist.";

	std::string dataset_name = "int_values";
	std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename;
	+" > /dev/null 2>&1";

	int run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	///
	std::array<int, dim> local_size_with_ghost = {10, 7, 15};
	std::array<int, dim> ghost_left = {2, 1, 4};
	std::array<int, dim> ghost_right = {0, 3, 2};

	std::array<int64_t, dim> global_size_expected = {};

	for (int ii = 0; ii < dim; ++ii) {
		global_size_expected[ii] = local_size_with_ghost[ii] - ghost_left[ii] - ghost_right[ii];
	}

	std::array<int64_t, dim> global_size = {-1, -1, -1};
	PDI_expose("damaris_nn", global_size.data(), PDI_INOUT); // get global size
	ASSERT_EQ(global_size_expected, global_size);

	int total_size = global_size_expected[0] * global_size_expected[1] * global_size_expected[2];
	std::vector<int> pdi_values(total_size);
	std::vector<int> damaris_values(total_size);

	PDI_multi_expose("read_pdi", "pdi_values", pdi_values.data(), PDI_IN, "damaris_values", damaris_values.data(), PDI_IN, NULL);

	ASSERT_EQ(pdi_values, damaris_values) << "Error in the reading value";
}
