/*
 * SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#include <assert.h>
#include <pdi.h>

#include "test.h"

void write_vector()
{
	Vector data_write;
	alloc_vector(&data_write);
	init_vector(&data_write);

	int array[10][10];
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			array[i][j] = 10 * i + j;
		}
	}

	PDI_multi_expose("write", "int_array", array, PDI_OUT, "vector_data", &data_write, PDI_OUT, NULL);

	free_vector(&data_write);
}

void check_read_vector(const Vector* data_read)
{
	Vector data;
	alloc_vector(&data);
	init_vector(&data);

	assert_eq_vector(data_read, &data);
	free_vector(&data);
}

void read_vector()
{
	Vector data_read;
	alloc_vector(&data_read);

	int array[10][10];
	for (int i = 0; i < 10; i++) {
		for (int j = 0; j < 10; j++) {
			array[i][j] = 0;
		}
	}

	PDI_multi_expose("read", "int_array", array, PDI_IN, "vector_data", &data_read, PDI_IN, NULL);

	for (int i = 1; i < 9; i++) {
		for (int j = 1; j < 9; j++) {
			if (array[i][j] != 10 * i + j) {
				printf("Array[%d][%d]: %d != %d\n", i, j, array[i][j], 10 * i + j);
				exit(1);
			}
		}
	}

	for (int i = 0; i < 10; i++) {
		if (array[0][i] != 0) {
			printf("Array[0][%d]: %d != 0\n", i, array[0][i]);
			exit(1);
		}
	}

	for (int i = 0; i < 10; i++) {
		if (array[9][i] != 0) {
			printf("Array[9][%d]: %d != 0\n", i, array[9][i]);
			exit(1);
		}
	}

	for (int i = 0; i < 10; i++) {
		if (array[i][0] != 0) {
			printf("Array[%d][0]: %d != 0\n", i, array[i][0]);
			exit(1);
		}
	}

	for (int i = 0; i < 10; i++) {
		if (array[i][9] != 0) {
			printf("Array[%d][9]: %d != 0\n", i, array[i][9]);
			exit(1);
		}
	}

	check_read_vector(&data_read);
	free_vector(&data_read);
}

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_path(argv[1]));

	write_vector();
	read_vector();

	PDI_finalize();
	return 0;
}
