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

#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <sstream>
#include <string>

#include <pdi/testing.h>

// Must match test_write_pyscript.c
constexpr int LOCAL_SIZE = 8;
constexpr int NB_ITER = 5;
constexpr int NB_CLIENTS = 2;

class PyScript: public ::PDI::PdiTest
{
protected:
	/// Runs test_write_pyscript with 2 clients and 1 Damaris server core, returns the exit status.
	/// The output goes to `writer.log'.
	static int run_writer(const std::string& yaml_file)
	{
		std::filesystem::create_directory("log");
		std::string run_command = "mpirun -np 3 ../test_write_pyscript ../" + yaml_file + " > writer.log 2>&1";
		return std::system(run_command.c_str());
	}

	/// Sum of the `values` dataset of all clients at iteration `it`, see test_write_pyscript.c
	static long expected_sum(int it)
	{
		long sum = 0;
		for (int rank = 0; rank < NB_CLIENTS; ++rank) {
			for (int i = 0; i < LOCAL_SIZE; ++i) {
				sum += 1000 * rank + 10 * it + i;
			}
		}
		return sum;
	}

	static std::string read_file(const std::string& filename)
	{
		std::ifstream file(filename);
		std::stringstream content;
		content << file.rdbuf();
		return content.str();
	}
};

/// The pyscript is run by the Damaris server on the iterations selected by `frequency`, with the
/// data written by every client
TEST_F(PyScript, RunsWithClientsData)
{
	ASSERT_EQ(run_writer("test_damaris_pyscript.yml"), 0) << "Error in the writing step";

	for (int it = 0; it < NB_ITER; ++it) {
		std::string filename = "pyscript_It" + std::to_string(it) + ".txt";
		if (it % 2 == 0) { // frequency: 2
			ASSERT_TRUE(std::filesystem::exists(filename)) << "The pyscript did not run at iteration " << it;
			std::istringstream result(read_file(filename));
			int nb_blocks = -1;
			long sum = -1;
			result >> nb_blocks >> sum;
			EXPECT_EQ(nb_blocks, NB_CLIENTS) << "at iteration " << it;
			EXPECT_EQ(sum, expected_sum(it)) << "at iteration " << it;
		} else {
			EXPECT_FALSE(std::filesystem::exists(filename)) << "The pyscript should not run at iteration " << it;
		}
	}
}

/// The `pyscript` keys and the dataset `script` key end up in the generated Damaris XML
TEST_F(PyScript, GeneratedXml)
{
	ASSERT_EQ(run_writer("test_damaris_pyscript.yml"), 0) << "Error in the writing step";

	// The plugin prints the generated XML at debug level (`logging: debug' in the YAML)
	std::string xml = read_file("writer.log");
	ASSERT_NE(xml.find("Generated Damaris XML configuration"), std::string::npos) << xml;

	EXPECT_NE(xml.find("script=\"TestPyScript\""), std::string::npos) << xml;
	for (const char* attribute:
	     {"<pyscript name=\"TestPyScript\"",
	      "file=\"../test_pyscript.py\"",
	      "execution=\"remote\"",
	      "language=\"python\"",
	      "scope=\"core\"",
	      "external=\"false\"",
	      "frequency=\"2\"",
	      "nthreads=\"1\"",
	      "timeout=\"4\"",
	      "keep-workers=\"no\"",
	      "comment=\"pyscript test\""})
	{
		EXPECT_NE(xml.find(attribute), std::string::npos) << "missing `" << attribute << "' in:\n" << xml;
	}
}

/// An unknown key in `pyscript` is a configuration error
TEST_F(PyScript, UnknownKeyFails)
{
	EXPECT_NE(run_writer("test_damaris_pyscript_unknown_key.yml"), 0);
	std::string output = read_file("writer.log");
	EXPECT_NE(output.find("The key `not_a_pyscript_key' doesn't exist for a pyscript."), std::string::npos) << output;
	EXPECT_FALSE(std::filesystem::exists("pyscript_It0.txt"));
}

/// A `pyscript` without `file` is a configuration error
TEST_F(PyScript, MissingFileFails)
{
	EXPECT_NE(run_writer("test_damaris_pyscript_missing_file.yml"), 0);
	std::string output = read_file("writer.log");
	EXPECT_NE(output.find("A pyscript requires both a `name' and a `file'."), std::string::npos) << output;
	EXPECT_FALSE(std::filesystem::exists("pyscript_It0.txt"));
}
