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

class Gdamaris: public ::PDI::PdiTest
{};

TEST_F(Gdamaris, Simu2Server1Collective)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { pdi_nn: int, damaris_nn: int }
data:
  pdi_values: {size: ['$pdi_nn'], type: array, subtype: int}
  damaris_values: {size: ['$damaris_nn'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './data_iter0.h5'
      read:
        pdi_values:
          dataset: int_values
        pdi_nn:
          size_of: int_values
    - file: './HDF5_files/damaris_scalar_type_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
)=="));

	const int nb_total_proc = 3;
	const int nb_simu_proc = 2;

	std::string exec_name = "test_write_simple_array_int";
	std::string yaml_file = "test_write_multi_process_collective.yml";
	std::string run_command = "mpirun -np " + std::to_string(nb_total_proc) + " ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	constexpr int const array_size_0 = 10 * nb_simu_proc;
	int global_size_pdi = 0;
	int global_size_damaris = 0;

	ASSERT_TRUE(std::filesystem::exists("data_iter0.h5"));
	ASSERT_TRUE(std::filesystem::exists("./HDF5_files/damaris_scalar_type_It0.h5"));

	PDI_expose("pdi_nn", &global_size_pdi, PDI_INOUT); // get global size
	ASSERT_EQ(global_size_pdi, array_size_0);
	PDI_expose("damaris_nn", &global_size_damaris, PDI_INOUT); // get global size
	ASSERT_EQ(global_size_damaris, array_size_0);

	std::array<int, array_size_0> pdi_values;
	std::array<int, array_size_0> damaris_values;

	PDI_multi_expose("read_pdi", "pdi_values", pdi_values.data(), PDI_INOUT, NULL);
	PDI_multi_expose("read_damaris", "damaris_values", damaris_values.data(), PDI_INOUT, NULL);

	EXPECT_EQ(pdi_values, damaris_values);
}

TEST_F(Gdamaris, Simu4Server2Collective)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata: { pdi_nn: int, damaris_nn: int }
data:
  pdi_values: {size: ['$pdi_nn'], type: array, subtype: int}
  damaris_values: {size: ['$damaris_nn'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './data_iter0.h5'
      read:
        pdi_values:
          dataset: int_values
        pdi_nn:
          size_of: int_values
    - file: './HDF5_files/damaris_scalar_type_It0.h5'
      read:
        damaris_values:
          dataset: int_values
        damaris_nn:
          size_of: int_values
)=="));

	const int nb_total_proc = 6;
	const int nb_simu_proc = 4;

	std::string exec_name = "test_write_simple_array_int";
	std::string yaml_file = "test_write_multi_process_collective_2_servers.yml";
	std::string run_command = "mpirun -np " + std::to_string(nb_total_proc) + " ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	constexpr int const array_size_0 = 10 * nb_simu_proc;
	int global_size_pdi = 0;
	int global_size_damaris = 0;

	ASSERT_TRUE(std::filesystem::exists("data_iter0.h5"));
	ASSERT_TRUE(std::filesystem::exists("./HDF5_files/damaris_scalar_type_It0.h5"));

	PDI_expose("pdi_nn", &global_size_pdi, PDI_INOUT); // get global size
	ASSERT_EQ(global_size_pdi, array_size_0);
	PDI_expose("damaris_nn", &global_size_damaris, PDI_INOUT); // get global size
	ASSERT_EQ(global_size_damaris, array_size_0);

	std::array<int, array_size_0> pdi_values;
	std::array<int, array_size_0> damaris_values;

	PDI_multi_expose("read_pdi", "pdi_values", pdi_values.data(), PDI_INOUT, NULL);
	PDI_multi_expose("read_damaris", "damaris_values", damaris_values.data(), PDI_INOUT, NULL);

	EXPECT_EQ(pdi_values, damaris_values);
}

TEST_F(Gdamaris, Simu4Server2FilePerCore)
{
	const int nb_total_proc = 6;
	const int nb_simu_proc = 4;

	std::string exec_name = "test_write_simple_array_int";
	std::string yaml_file = "test_write_multi_process_file_per_core.yml";
	std::string run_command = "mpirun -np " + std::to_string(nb_total_proc) + " ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	ASSERT_TRUE(std::filesystem::exists("./HDF5_files/damaris_scalar_type_It0_Pr0.h5"));
	ASSERT_TRUE(std::filesystem::exists("./HDF5_files/damaris_scalar_type_It0_Pr1.h5"));

	// loop over the simulation process
	for (int irank = 0; irank < nb_simu_proc; ++irank) {
		int server_expected = irank / 2;
		int value_expected = irank * 100;

		std::string dataset_name = "/int_values/P" + std::to_string(irank);
		std::string filename = "./HDF5_files/damaris_scalar_type_It0_Pr" + std::to_string(server_expected) + ".h5";

		// check the dataset exist in file
		std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename + " > /dev/null 2>&1";

		int run_check_dataset = std::system(run_command1.c_str());
		EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;

		// If the dataset exists, we check the value
		if (run_check_dataset == 0) {
			std::string run_command2 = "h5dump -d  \'" + dataset_name + "\' -s '0' -c '1' " + filename
			                         + " | grep \'(0): " + std::to_string(value_expected) + "\' > /dev/null 2>&1";
			int run_check_value = std::system(run_command2.c_str());

			// show the value on the standart output to compare
			if (run_check_value != 0) {
				run_command2 = "h5dump -d \'/int_values/P" + std::to_string(irank) + "\' " + filename;
				std::system(run_command2.c_str());
			}
			EXPECT_EQ(run_check_value, 0) << "Error: The value of " + dataset_name + " is not equal to " + std::to_string(value_expected) + " in "
												 + filename + "(see value in the file below).";
		}
	}
}

TEST_F(Gdamaris, WriteMetadata)
{
	const int nb_total_proc = 2;
	const int nb_simu_proc = 1;

	std::string exec_name = "test_write_simple_array_int";
	std::string yaml_file = "test_write_metadata.yml";
	std::string run_command = "mpirun -np " + std::to_string(nb_total_proc) + " ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step with yaml file " + yaml_file;

	// check the dataset exist in file
	std::string dataset_name = "/int_values";
	std::string filename = "./HDF5_files/damaris_metadata_array_It0.h5";

	ASSERT_TRUE(std::filesystem::exists(filename));

	// check the dataset exist in file
	std::string value_expected = "(0): 0, 1, 2, 3, 4, 5, 6, 7, 8, 9";
	std::string run_command1 = "h5dump -d \'" + dataset_name + "\' " + filename + " | grep \'" + value_expected + "\' > /dev/null 2>&1";

	int run_check_dataset = std::system(run_command1.c_str());
	EXPECT_EQ(run_check_dataset, 0) << "Error: The dataset " + dataset_name + " doesn't exist in " + filename;
}
