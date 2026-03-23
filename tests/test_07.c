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

#include <assert.h>
#include <pdi.h>

#include "test.h"

#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define MAX_LINE_LENGTH 1024
#define PLACEHOLDER "/<full>/<path>/<to>/<test_07_data.yml>"
#define TEMP_FILE_PATH "/tmp_dir_test/temp_test_07.yml"

struct Record_data {
	int a;
	int* b;
} typedef Record_data;

int replace_placeholder_in_file(const char* file_path, const char* replace_str)
{
	FILE* file = fopen(file_path, "r");
	if (!file) {
		perror("Failed to open file");
		return 1;
	}
	FILE* temp_file = fopen(TEMP_FILE_PATH, "w");
	if (!temp_file) {
		perror("Failed to open temp file");
		fclose(file);
		return 1;
	}
	char line[MAX_LINE_LENGTH];
	while (fgets(line, sizeof(line), file)) {
		char* pos = strstr(line, PLACEHOLDER);
		if (pos) {
			fwrite(line, 1, pos - line, temp_file);
			fwrite(replace_str, 1, strlen(replace_str), temp_file);
			fwrite(pos + strlen(PLACEHOLDER), 1, strlen(pos) - strlen(PLACEHOLDER), temp_file);
		} else {
			fputs(line, temp_file);
		}
	}
	fclose(file);
	fclose(temp_file);
	printf("Modified YAML written to: %s\n", TEMP_FILE_PATH);
	return 0;
}

int main(int argc, char* argv[])
{
	// Detect if running in GitHub Actions
	const char* github_actions = getenv("GITHUB_ACTIONS");
	bool is_github_actions = (github_actions != NULL);
	const char* ci = getenv("CI");
	printf("CI=%s\n", ci ? ci : "null");

// Detect macOS
#ifdef __APPLE__
	bool is_macOS = true;
#else
	bool is_macOS = false;
#endif

	PC_tree_t conf;
	bool conf_created = false;

	if (is_macOS) {
		printf("is_macOS=%s\n", is_macOS ? "true" : "false");
		// macOS (local CI): Use PC_parse_string to emulate the test, workaround for online CI on macOS
		static const char* CONFIG_YAML
			= "pdi:\n"
			  "  logging: trace\n"
			  "  metadata:\n"
			  "    input: int\n"
			  "  data:\n"
			  "    scalar_data: int\n"
			  "    array_data:\n"
			  "      type: array\n"
			  "      subtype: int\n"
			  "      size: 8\n"
			  "      subsize: 4\n"
			  "      start: 2\n"
			  "    record_data:\n"
			  "      type: record\n"
			  "      buffersize: 16\n"
			  "      members:\n"
			  "        a:\n"
			  "          disp: 0\n"
			  "          type: int\n"
			  "        b:\n"
			  "          disp: 8\n"
			  "          type: pointer\n"
			  "          subtype: int\n"
			  "  plugins:\n"
			  "    serialize:\n"
			  "      scalar_data: scalar_data_serialized\n"
			  "      array_data: array_data_serialized\n"
			  "      record_data: record_data_serialized\n"
			  "    decl_hdf5:\n"
			  "      - file: serialize_test_06.h5\n"
			  "        when: '$input=0'\n"
			  "        write: [scalar_data_serialized, array_data_serialized, record_data_serialized]\n"
			  "      - file: serialize_test_06.h5\n"
			  "        when: '$input=1'\n"
			  "        read: [scalar_data_serialized, array_data_serialized, record_data_serialized]";

		conf = PC_parse_string(CONFIG_YAML);
		conf_created = true;
		PDI_init(conf);
	} else if (is_github_actions) {
		printf("GITHUB_ACTIONS=%s\n", github_actions);
		// GitHub Actions (online CI): Use file-based logic with placeholder replacement, workaround for online CI on Linux
		if (argc < 2) {
			fprintf(stderr, "Missing path to test_07.yml\n");
			return 1;
		}
		const char* yaml_path = argv[1];
		char data_path[1024];
		snprintf(data_path, sizeof(data_path), "%.*s_data.yml", (int)(strlen(yaml_path) - 4), yaml_path);
		if (replace_placeholder_in_file(yaml_path, data_path) != 0) {
			fprintf(stderr, "Failed to modify the root YAML file\n");
			return 1;
		}
		conf = PC_parse_path("/tmp_dir_test/temp_test_07.yml");
		conf_created = true;
		PDI_init(PC_get(conf, ".pdi"));
	} else {
		printf("GITHUB_ACTIONS is NOT set\n");
		// Local Linux: Use classic file-based logic without placeholder replacement, classic use for local CI
		if (argc < 2) {
			fprintf(stderr, "Missing path to test_07.yml\n");
			return 1;
		}
		conf = PC_parse_path(argv[1]);
		conf_created = true;
		PDI_init(PC_get(conf, ".pdi"));
	}

	int input = 0;
	PDI_expose("input", &input, PDI_OUT);

	int scalar_data = 42;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);
	int array_data[8];
	for (int i = 0; i < 8; i++) {
		array_data[i] = 42 + i;
	}
	PDI_expose("array_data", array_data, PDI_OUT);

	Record_data record_data;
	record_data.a = 50;
	int b = 51;
	record_data.b = &b;
	PDI_expose("record_data", &record_data, PDI_OUT);

	input = 1;
	PDI_expose("input", &input, PDI_OUT);

	int scalar_data_read;
	PDI_expose("scalar_data", &scalar_data_read, PDI_IN);
	printf("%d ?== %d\n", scalar_data, scalar_data_read);
	if (scalar_data != scalar_data_read) {
		fprintf(stderr, "Assertion failed: %d != %d\n", scalar_data, scalar_data_read);
		exit(EXIT_FAILURE);
	}

	int array_data_read[8];
	PDI_expose("array_data", array_data_read, PDI_IN);
	for (int i = 2; i < 6; i++) {
		printf("[%d] %d ?== %d\n", i, array_data[i], array_data_read[i]);
		assert(array_data[i] == array_data_read[i]);
	}

	int b_read;
	Record_data record_data_read;
	record_data_read.b = &b_read;
	PDI_expose("record_data", &record_data_read, PDI_IN);
	printf("%d ?== %d\n", record_data.a, record_data_read.a);
	assert(record_data.a == record_data_read.a);
	printf("%d ?== %d\n", b, b_read);
	assert(b == b_read);

	PDI_finalize();
	if (conf_created) {
		PC_tree_destroy(&conf);
	}
	return 0;
}
