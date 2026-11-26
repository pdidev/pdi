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

#include <chrono>
#include <cstdlib>
#include <filesystem>
#include <fstream>
#include <string>
#include <thread>

#include <pdi/testing.h>

// Set by CMake:
//  - DASK_SCHEDULER_EXECUTABLE: the `dask-scheduler' to start (DAMARIS_DASK_SCHEDULER_EXECUTABLE)
//  - DASK_BIN_DIR: its directory, prepended to PATH as Damaris starts the workers with `dask worker'
//  - DASK_PYTHONPATH: appended to PYTHONPATH for the Python run by the Damaris servers
//    (DAMARIS_DASK_PYTHONPATH, e.g. where damaris4py and dask are installed)

// Must match test_write_pyscript.c
constexpr int LOCAL_SIZE = 8;
constexpr int NB_ITER = 5;
constexpr int NB_CLIENTS = 2;
constexpr int NB_SERVERS = 2;

class PyScriptDask: public ::PDI::PdiTest
{
protected:
	void SetUp() override
	{
		const char* path = std::getenv("PATH");
		setenv("PATH", (std::string(DASK_BIN_DIR) + (path ? std::string(":") + path : "")).c_str(), 1);
		if (std::string(DASK_PYTHONPATH) != "") {
			const char* pythonpath = std::getenv("PYTHONPATH");
			setenv("PYTHONPATH", ((pythonpath ? std::string(pythonpath) + ":" : "") + DASK_PYTHONPATH).c_str(), 1);
		}

		std::filesystem::create_directory("log");
		// Random free ports, so that the test doesn't depend on (or disturb) another scheduler
		std::string start_scheduler = std::string(DASK_SCHEDULER_EXECUTABLE)
		                            + " --scheduler-file dask_file.json --host 127.0.0.1 --port 0 --dashboard-address :0"
		                              " > scheduler.log 2>&1 & echo $! > scheduler.pid";
		ASSERT_EQ(std::system(start_scheduler.c_str()), 0);
		for (int i = 0; i < 300 && !std::filesystem::exists("dask_file.json"); ++i) {
			std::this_thread::sleep_for(std::chrono::milliseconds(100));
		}
		ASSERT_TRUE(std::filesystem::exists("dask_file.json")) << "The Dask scheduler did not start";
	}

	void TearDown() override { std::system("kill $(cat scheduler.pid) > /dev/null 2>&1"); }
};

/// The pyscript builds a dask.array from the blocks of every Damaris server core
TEST_F(PyScriptDask, SumOverAllServers)
{
	std::string run_command = "mpirun -np " + std::to_string(NB_CLIENTS + NB_SERVERS)
	                        + " ../test_write_pyscript ../test_damaris_pyscript_dask.yml";
	ASSERT_EQ(std::system(run_command.c_str()), 0) << "Error in the writing step";

	for (int it = 0; it < NB_ITER; ++it) {
		std::string filename = "pyscript_dask_It" + std::to_string(it) + ".txt";
		ASSERT_TRUE(std::filesystem::exists(filename)) << "The pyscript did not run at iteration " << it;

		long expected_sum = 0;
		for (int rank = 0; rank < NB_CLIENTS; ++rank) {
			for (int i = 0; i < LOCAL_SIZE; ++i) {
				expected_sum += 1000 * rank + 10 * it + i;
			}
		}
		std::ifstream result(filename);
		int nb_servers = -1;
		long sum = -1;
		result >> nb_servers >> sum;
		EXPECT_EQ(nb_servers, NB_SERVERS) << "at iteration " << it;
		EXPECT_EQ(sum, expected_sum) << "at iteration " << it;
	}
}
