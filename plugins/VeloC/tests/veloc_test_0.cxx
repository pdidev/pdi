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

#include <mpi.h>
#include <iostream>
#include <pdi.h>

static void error_handler(PDI_status_t status, const char* message, void* ctx)
{
	if (status) {
		std::cerr << "[PDI error] " << message << "\n";
		*static_cast<int*>(ctx) = 1;
	}
}

const char CONF_VALID[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_0\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "      veloc_file : veloc_file_buf \n"
	  "      custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n"
	  "      custom_recover:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_rec\n"
	  "           route_file_on: route_rec\n"
	  "           end_on: end_rec\n";

const char CONF_CONFIG_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    checkpoint_label: test_0\n"
	  "    iteration: ii\n";

const char CONF_LABEL_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    iteration: ii\n";

const char CONF_ITERATION_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_0\n";

const char CONF_ITER_NOT_PROTECTED[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_0\n"
	  "    iteration: ii\n"
	  "    managed_checkpointing:\n"
	  "      protect_data: [var]\n"
	  "      checkpoint_on: ckp\n";

const char CONF_BAD_FAILURE[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 99\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_0\n"
	  "    iteration: ii\n"
	  "    managed_checkpointing:\n"
	  "      protect_data: [ii,var]\n"
	  "      checkpoint_on: ckp\n";

const char CONF_PROTECT_DATA_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    managed_checkpointing:\n"
	  "      checkpoint_on: ckp\n";

const char CONF_CP_OFILE_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n";

const char CONF_CP_START_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       veloc_file : veloc_file_buf\n"
	  "       custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n";

const char CONF_CP_ROUTE_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       veloc_file : veloc_file_buf\n"
	  "       custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           end_on: end_ckp\n";

const char CONF_CP_END_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       veloc_file : veloc_file_buf\n"
	  "       custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n";

const char CONF_REC_OFILE_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 1\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       veloc_file : veloc_file_buf\n"
	  "       custom_recover:\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n";

const char CONF_REC_START_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 1\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "       veloc_file : veloc_file_buf\n"
	  "       custom_recover:\n"
	  "           original_file: my_file.dat\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n";

const char CONF_REC_ROUTE_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 1\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    protect_data: [ii, var]\n"
	  "    recover_on: rec\n"
	  "    custom_recover:\n"
	  "      original_file: my_file.dat\n"
	  "      veloc_file: veloc_file_buf\n"
	  "      start_on: start_rec\n"
	  "      end_on: end_rec\n";

const char CONF_REC_END_MISSING[]
	= "metadata:\n"
	  "  ii: int\n"
	  "  veloc_file_buf: {type: array, subtype: char, size: 256}\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 1\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_01\n"
	  "    iteration: ii\n"
	  "    protect_data: [ii, var]\n"
	  "    recover_on: rec\n"
	  "    custom_recover:\n"
	  "      original_file: my_file.dat\n"
	  "      veloc_file: veloc_file_buf\n"
	  "      start_on: start_rec\n"
	  "      route_file_on: route_rec\n";

const char CONF_DUPLICATE_EVENTS[]
	= "metadata:\n"
	  "  ii: int\n"
	  "data:\n"
	  "  var: int\n"
	  "plugins:\n"
	  "  veloc:\n"
	  "    failure: 0\n"
	  "    config_file: veloc_config.cfg\n"
	  "    checkpoint_label: test_0\n"
	  "    iteration: ii\n"
	  "    custom_checkpointing:\n"
	  "      custom_checkpoint:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n"
	  "      custom_recover:\n"
	  "           original_file: my_file.dat\n"
	  "           start_on: start_ckp\n"
	  "           route_file_on: route_ckp\n"
	  "           end_on: end_ckp\n";

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);

	struct Test {
		const char* name;
		const char* yaml;
		int expect_error;
	};

	const Test tests[] = {
		{"valid custom configuration", CONF_VALID, 0},

		{"missing config_file", CONF_CONFIG_MISSING, 1},
		{"missing checkpoint_label", CONF_LABEL_MISSING, 1},
		{"missing iteration", CONF_ITERATION_MISSING, 1},
		{"iteration not in protect_data", CONF_ITER_NOT_PROTECTED, 1},
		{"invalid failure value", CONF_BAD_FAILURE, 1},
		{"missing protect_data", CONF_PROTECT_DATA_MISSING, 1},

		{"custom_checkpoint: missing veloc_file", CONF_CP_OFILE_MISSING, 1},
		{"custom_checkpoint: missing start_on", CONF_CP_START_MISSING, 1},
		{"custom_checkpoint: missing route_file_on", CONF_CP_ROUTE_MISSING, 1},
		{"custom_checkpoint: missing end_on", CONF_CP_END_MISSING, 1},

		{"custom_recover: missing original_file", CONF_REC_OFILE_MISSING, 1},
		{"custom_recover: missing start_on", CONF_REC_START_MISSING, 1},
		{"custom_recover: missing route_file_on", CONF_REC_ROUTE_MISSING, 1},
		{"custom_recover: missing end_on", CONF_REC_END_MISSING, 1},

		{"missing config_file", CONF_DUPLICATE_EVENTS, 1},
	};

	int passed = 0, failed = 0;

	for (auto& t: tests) {
		int has_errored = 0;

		PDI_errhandler_t custom_handler;
		custom_handler.func = error_handler;
		custom_handler.context = &has_errored;

		PC_tree_t conf = PC_parse_string(t.yaml);

		PDI_errhandler_t prev_handler = PDI_errhandler(custom_handler);

		PDI_init(conf);

		PDI_errhandler(prev_handler);

		if (!has_errored) {
			PDI_finalize();
			PC_tree_destroy(&conf);
		}

		bool ok = (has_errored == t.expect_error);
		std::cout << (ok ? "[PASS] " : "[FAIL] ") << t.name << "\n";
		ok ? ++passed : ++failed;
	}

	std::cout << "\n" << passed << " passed, " << failed << " failed.\n";

	if (failed == 0) {
		std::cout << "TEST 0_0 PASSED " << std::endl;
	}

	MPI_Finalize();
	return (failed == 0) ? 0 : 1;
}