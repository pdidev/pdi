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
#define PLACEHOLDER_ARRAY_DATA_YAML "test_07_array_data.yml"
#define PLACEHOLDER_RECORD_DATA_YAML "test_07_record_data.yml"
#define PLACEHOLDER_SUBDATA_YAML "test_07_subdata.yml"
#define TEMP_FILE_PATH "/tmp_dir_test/temp_test_07.yml"

struct Record_data {
	int a;
	int* b;
} typedef Record_data;

int replace_all_placeholders(const char* input_path)
{
	FILE* in = fopen(input_path, "r");
	if (!in)
	{
		perror("open input");
		return 1;
	}

	FILE* out = fopen(TEMP_FILE_PATH, "w");
	if (!out)
	{
		perror("open output");
		fclose(in);
		return 1;
	}

	char line[MAX_LINE_LENGTH];

	while (fgets(line, sizeof(line), in))
	{
		char buffer[MAX_LINE_LENGTH * 2];
		strcpy(buffer, line);

		// Replace each placeholder sequentially
		struct {
			const char* placeholder;
			const char* replacement;
		} replacements[] = {
			{PLACEHOLDER_ARRAY_DATA_YAML, "/tmp_dir_test/test_07_array_data.yml"},
			{PLACEHOLDER_RECORD_DATA_YAML, "/tmp_dir_test/test_07_record_data.yml"},
			{PLACEHOLDER_SUBDATA_YAML, "/tmp_dir_test/test_07_subdata.yml"},
		};

		for (size_t i = 0; i < sizeof(replacements) / sizeof(replacements[0]); ++i)
		{
			char* pos = strstr(buffer, replacements[i].placeholder);
			if (pos) {
				char tmp[MAX_LINE_LENGTH * 2];

				size_t prefix_len = pos - buffer;

				memcpy(tmp, buffer, prefix_len);
				strcpy(tmp + prefix_len, replacements[i].replacement);
				strcpy(tmp + prefix_len + strlen(replacements[i].replacement), pos + strlen(replacements[i].placeholder));
				strcpy(buffer, tmp);
			}
		}

		fputs(buffer, out);
	}

	fclose(in);
	fclose(out);

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

	PC_tree_t conf;
	bool conf_created = false;

	if (is_github_actions) {
		printf("GITHUB_ACTIONS=%s\n", github_actions);
		// GitHub Actions (online CI): Use file-based logic with placeholder replacement, workaround for online CI on Linux
		if (argc < 2) {
			fprintf(stderr, "Missing path to test_07.yml\n");
			return 1;
		}
		const char* yaml_path = argv[1];
		if (replace_all_placeholders(yaml_path) != 0) {
			fprintf(stderr, "Failed to modify the root YAML file\n");
			return 1;
		}
		conf = PC_parse_path(TEMP_FILE_PATH);
		conf_created = true;
		PDI_init(conf);
	} else {
		printf("GITHUB_ACTIONS is NOT set\n");
		// Local Linux: Use classic file-based logic without placeholder replacement, classic use for local CI (or macOS with Paraconf 1.1+)
		if (argc < 2) {
			fprintf(stderr, "Missing path to test_07.yml\n");
			return 1;
		}
		conf = PC_parse_path(argv[1]);
		conf_created = true;
		PDI_init(conf);
	}

	int input = 0;
	PDI_expose("input", &input, PDI_OUT);

	int scalar_data = 42;
	PDI_expose("scalar_data", &scalar_data, PDI_OUT);

	int scalar_subdata = 43;
	PDI_expose("scalar_subdata", &scalar_subdata, PDI_OUT);

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

	int scalar_subdata_read;
	PDI_expose("scalar_subdata", &scalar_subdata_read, PDI_IN);
	printf("%d ?== %d\n", scalar_subdata, scalar_subdata_read);
	if (scalar_subdata != scalar_subdata_read) {
		fprintf(stderr, "Assertion failed: %d != %d\n", scalar_subdata, scalar_subdata_read);
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
