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
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

class Gdamaris: public ::PDI::PdiTest
{};

TEST_F(Gdamaris, File2Data)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { damaris22_nn: int, damaris_nn: int }
data:
  damaris22_values: {size: ['$damaris_nn'], type: array, subtype: int}
  damaris_values: {size: ['$damaris_nn'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './HDF5_files/damaris_scalar_type_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
        damaris22_values:
          dataset: int22_values
        damaris22_nn:
          size_of: int22_values
)=="));

	std::string exec_name = "damaris_write_multiple_data";
	std::string yaml_file = "test_write_1_file_2_data.yml";
	std::string run_command = "mpirun -np 2 ../" + exec_name + " ../" + yaml_file;

	constexpr int const array_size_0 = 10;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step for input file " + yaml_file;

	// dataset: int_values
	std::string filename = "./HDF5_files/damaris_scalar_type_It0.h5";
	ASSERT_TRUE(std::filesystem::exists(filename));

	std::string dataset_name = "int_values";
	std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename + " > /dev/null 2>&1";

	int run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	int global_size = 0;
	PDI_expose("damaris_nn", &global_size, PDI_INOUT); // get global size
	ASSERT_EQ(global_size, array_size_0);

	// dataset: int22_values
	dataset_name = "int22_values";
	run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename + " > /dev/null 2>&1";

	run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	global_size = 0;
	PDI_expose("damaris22_nn", &global_size, PDI_INOUT); // get global size
	ASSERT_EQ(global_size, array_size_0);
}

TEST_F(Gdamaris, TwoFile)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { damaris22_nn: int, damaris_nn: int }
data:
  damaris22_values: {size: ['$damaris_nn'], type: array, subtype: int}
  damaris_values: {size: ['$damaris_nn'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './HDF5_files/damaris_scalar_type_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
    - file: './HDF5_22_files/damaris_scalar_type_It0.h5'
      read:
        damaris22_values:
          dataset: int22_values
        damaris22_nn:
          size_of: int22_values
)=="));

	std::string exec_name = "damaris_write_multiple_data";
	std::string yaml_file = "test_write_2_file_1_data.yml";
	std::string run_command = "mpirun -np 2 ../" + exec_name + " ../" + yaml_file;

	constexpr int const array_size_0 = 10;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step for input file " + yaml_file;

	// dataset: int_values
	std::string filename = "./HDF5_files/damaris_scalar_type_It0.h5";
	ASSERT_TRUE(std::filesystem::exists(filename));

	std::string dataset_name = "int_values";
	std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename + " > /dev/null 2>&1";

	int run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	int global_size = 0;
	PDI_expose("damaris_nn", &global_size, PDI_INOUT); // get global size
	ASSERT_EQ(global_size, array_size_0);

	// dataset: int22_values
	filename = "./HDF5_22_files/damaris_scalar_type_It0.h5";
	ASSERT_TRUE(std::filesystem::exists(filename));

	dataset_name = "int22_values";
	run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename;
	+" > /dev/null 2>&1";

	run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

	global_size = 0;
	PDI_expose("damaris22_nn", &global_size, PDI_INOUT); // get global size
	ASSERT_EQ(global_size, array_size_0);
}
