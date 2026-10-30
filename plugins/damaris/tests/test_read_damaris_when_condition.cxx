/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Institut National de Recherche en Sciences et Technologies du Numérique (INRIA)
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

#include <pdi/testing.h>

class Gdamaris: public ::PDI::PdiTest
{};

TEST_F(Gdamaris, WhenCondition)
{
	std::string exec_name = "damaris_write_when_condition";
	std::string yaml_file = "test_damaris_when_condition.yml";
	std::string run_command = "mpirun -np 2 ../" + exec_name + " ../" + yaml_file;

	int result = std::system(run_command.c_str());
	ASSERT_EQ(result, 0) << "Error in the writing step for yaml file " + yaml_file;

	int max_iter = 35;
	int frequency = 10; // frequency of writting the data

	std::string pre_filename = "./HDF5_files/when_condition_It";
	int damaris_iteration = 0;
	for (int ii = 0; ii < max_iter; ++ii) {
		if (ii % frequency == 0) {
			damaris_iteration = ii / frequency;
			std::string filename = pre_filename + std::to_string(damaris_iteration) + ".h5";
			EXPECT_TRUE(std::filesystem::exists(filename));

			int value_expected = ii;
			std::string run_command2 = "h5dump -d  \'iteration_ds\' -s '0' -c '1' " + filename + " | grep \'(0): " + std::to_string(value_expected)
			                         + "\' > /dev/null 2>&1";
			int run_check_value = std::system(run_command2.c_str());
			EXPECT_EQ(run_check_value, 0);
		}
	}

	std::string filename = pre_filename + std::to_string(damaris_iteration + 1) + ".h5";
	EXPECT_FALSE(std::filesystem::exists(filename));
}
