/*******************************************************************************
 * Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <benchmark/benchmark.h>
#include <hdf5.h>
#include <iostream>
#include <memory>
#include <string>

#include <pdi.h>
#include <paraconf.h>

class Decl_hdf5_matrix : public benchmark::Fixture
{
public:

	void SetUp(const ::benchmark::State& state)
	{
	}
	
	void TearDown(const ::benchmark::State& state)
	{
	}
};

static void PDI_write(benchmark::State& state)
{
	const char* CONFIG_YAML =
		"logging: off                                                  \n"
		"metadata:                                                     \n"
		"  matrix_size: { size: 2, type: array, subtype: int64 }       \n"
		"data:                                                         \n"
		"  matrix_data:                                                \n"
		"    type: array                                               \n"
		"    subtype: double                                           \n"
		"    size: ['${matrix_size[0]}', '${matrix_size[1]}']          \n"
		"plugins:                                                      \n"
		"  decl_hdf5:                                                  \n"
		"    file: matrix_data.h5                                      \n"
		"    collision_policy: replace                                 \n"
		"    write: [matrix_data]                                      \n"
		;

	int64_t matrix_size[2] = {state.range(0), state.range(0)};
	std::unique_ptr<double[]> matrix {static_cast<double*>(operator new[](matrix_size[0] * matrix_size[1] * sizeof(double)))};
	for (int i = 0; i < matrix_size[0] * matrix_size[1]; i++) {
		matrix[i] = i * 1.2345;
	}
	PDI_init(PC_parse_string(CONFIG_YAML));
	PDI_expose("matrix_size", matrix_size, PDI_OUT);
	for (auto _ : state) {
		PDI_expose("matrix_data", matrix.get(), PDI_OUT);
	}
	PDI_finalize();
}
BENCHMARK(PDI_write)->Name("Decl_hdf5_matrix/PDI_write")->RangeMultiplier(4)->Range(1024, 1024<<4);

static void HDF5_write(benchmark::State& state)
{
	int64_t matrix_size[2] = {state.range(0), state.range(0)};
	std::unique_ptr<double[]> matrix {static_cast<double*>(operator new[](matrix_size[0] * matrix_size[1] * sizeof(double)))};
	for (int i = 0; i < matrix_size[0] * matrix_size[1]; i++) {
		matrix[i] = i * 1.2345;
	}

	for (auto _ : state) {
		hid_t file_id = H5Fcreate("matrix_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
		if (file_id < 0) exit(1);
		hsize_t coords[2] = {static_cast<hsize_t>(matrix_size[0]), static_cast<hsize_t>(matrix_size[1])};
		hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
		if (dataspace_id < 0) exit(1);
		hid_t dataset_id = H5Dcreate2(file_id, "matrix_data", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
		if (dataset_id < 0) exit(1);
		herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix.get());
		if (status < 0) exit(1);
		status = H5Sclose(dataspace_id);
		if (status < 0) exit(1);
		status = H5Dclose(dataset_id);
		if (status < 0) exit(1);
		status = H5Fclose(file_id);
		if (status < 0) exit(1);
	}
}
BENCHMARK(HDF5_write)->Name("Decl_hdf5_matrix/HDF5_write")->RangeMultiplier(4)->Range(1024, 1024<<4);

static void PDI_read(benchmark::State& state)
{
	int64_t matrix_size[2] = {state.range(0), state.range(0)};
	std::unique_ptr<double[]> matrix {static_cast<double*>(operator new[](matrix_size[0] * matrix_size[1] * sizeof(double)))};
	for (int i = 0; i < matrix_size[0] * matrix_size[1]; i++) {
		matrix[i] = i * 1.2345;
	}

	// create file
	hid_t file_id = H5Fcreate("matrix_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) exit(1);
	hsize_t coords[2] = {static_cast<hsize_t>(matrix_size[0]), static_cast<hsize_t>(matrix_size[1])};
	hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
	if (dataspace_id < 0) exit(1);
	hid_t dataset_id = H5Dcreate2(file_id, "matrix_data", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	if (dataset_id < 0) exit(1);
	herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix.get());
	if (status < 0) exit(1);
	status = H5Sclose(dataspace_id);
	if (status < 0) exit(1);
	status = H5Dclose(dataset_id);
	if (status < 0) exit(1);
	status = H5Fclose(file_id);
	if (status < 0) exit(1);

	// read file
	const char* CONFIG_YAML =
		"logging: off                                                  \n"
		"metadata:                                                     \n"
		"  matrix_size: { size: 2, type: array, subtype: int64 }       \n"
		"data:                                                         \n"
		"  matrix_data:                                                \n"
		"    type: array                                               \n"
		"    subtype: double                                           \n"
		"    size: ['${matrix_size[0]}', '${matrix_size[1]}']          \n"
		"plugins:                                                      \n"
		"  decl_hdf5:                                                  \n"
		"    file: matrix_data.h5                                      \n"
		"    collision_policy: replace                                 \n"
		"    read: [matrix_data]                                       \n"
		;

	
	
	for (int i = 0; i < matrix_size[0] * matrix_size[1]; i++) {
		matrix[i] = 0.0;
	}
	PDI_init(PC_parse_string(CONFIG_YAML));
	PDI_expose("matrix_size", matrix_size, PDI_OUT);
	for (auto _ : state) {
		PDI_expose("matrix_data", matrix.get(), PDI_IN);
	}
	PDI_finalize();
}
BENCHMARK(PDI_read)->Name("Decl_hdf5_matrix/PDI_read")->RangeMultiplier(4)->Range(1024, 1024<<4);

static void HDF5_read(benchmark::State& state)
{
	int64_t matrix_size[2] = {state.range(0), state.range(0)};
	std::unique_ptr<double[]> matrix {static_cast<double*>(operator new[](matrix_size[0] * matrix_size[1] * sizeof(double)))};
	
	// create file
	hid_t file_id = H5Fcreate("matrix_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) exit(1);
	hsize_t coords[2] = {static_cast<hsize_t>(matrix_size[0]), static_cast<hsize_t>(matrix_size[1])};
	hid_t dataspace_id = H5Screate_simple(2, coords, NULL);
	if (dataspace_id < 0) exit(1);
	hid_t dataset_id = H5Dcreate2(file_id, "matrix_data", H5T_NATIVE_INT, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);	
	if (dataset_id < 0) exit(1);
	herr_t status = H5Dwrite(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix.get());
	if (status < 0) exit(1);
	status = H5Sclose(dataspace_id);
	if (status < 0) exit(1);
	status = H5Dclose(dataset_id);
	if (status < 0) exit(1);
	status = H5Fclose(file_id);
	if (status < 0) exit(1);

	for (int i = 0; i < matrix_size[0] * matrix_size[1]; i++) {
		matrix[i] = 0.0;
	}

	for (auto _ : state) {
		hid_t file_id = H5Fopen("matrix_data.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
		if (file_id < 0) exit(1);
		hid_t dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);	
		if (dataset_id < 0) exit(1);
		herr_t status = H5Dread(dataset_id, H5T_NATIVE_DOUBLE, H5S_ALL, H5S_ALL, H5P_DEFAULT, matrix.get());
		if (status < 0) exit(1);
		status = H5Dclose(dataset_id);
		if (status < 0) exit(1);
		status = H5Fclose(file_id);
		if (status < 0) exit(1);
	}
}
BENCHMARK(HDF5_read)->Name("Decl_hdf5_matrix/HDF5_read")->RangeMultiplier(4)->Range(1024, 1024<<4);
