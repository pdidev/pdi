// SPDX-FileCopyrightText: 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <iostream>
#include <memory>
#include <string>
#include <benchmark/benchmark.h>
#include <hdf5.h>

#include <paraconf.h>
#include <pdi.h>

class Decl_hdf5_struct: public benchmark::Fixture
{
public:
	static const size_t data_size = 256;

	struct Record_type {
		int size;
		double data[data_size];
	};

	void SetUp(const ::benchmark::State& state) {}

	void TearDown(const ::benchmark::State& state) {}
};

static void PDI_write(benchmark::State& state)
{
	const char* CONFIG_YAML
		= "logging: off                                                  \n"
		  "metadata:                                                     \n"
		  "  size: int                                                   \n"
		  "data:                                                         \n"
		  "  record_data:                                                \n"
		  "    type: struct                                              \n"
		  "    members:                                                  \n"
		  "      - size: int                                             \n"
		  "      - data:                                                 \n"
		  "          type: array                                         \n"
		  "          subtype: double                                     \n"
		  "          size: $size                                         \n"
		  "plugins:                                                      \n"
		  "  decl_hdf5:                                                  \n"
		  "    file: record_data.h5                                      \n"
		  "    collision_policy: replace                                 \n"
		  "    write: [record_data]                                      \n";

	Decl_hdf5_struct::Record_type record_data;
	record_data.size = Decl_hdf5_struct::data_size;
	for (int i = 0; i < Decl_hdf5_struct::data_size; i++) {
		record_data.data[i] = i * 1.23;
	}
	PDI_init(PC_parse_string(CONFIG_YAML));
	int size = Decl_hdf5_struct::data_size;
	PDI_expose("size", &size, PDI_OUT);
	for (auto _: state) {
		PDI_expose("record_data", &record_data, PDI_OUT);
	}
	PDI_finalize();
}

BENCHMARK(PDI_write)->Name("Decl_hdf5_struct/PDI_write");

static void HDF5_write(benchmark::State& state)
{
	Decl_hdf5_struct::Record_type record_data;
	record_data.size = Decl_hdf5_struct::data_size;
	for (int i = 0; i < Decl_hdf5_struct::data_size; i++) {
		record_data.data[i] = i * 1.23;
	}

	for (auto _: state) {
		hid_t file_id = H5Fcreate("record_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
		if (file_id < 0) exit(1);
		hid_t dataspace_id = H5Screate(H5S_SCALAR);
		hsize_t size = Decl_hdf5_struct::data_size;
		hid_t array_type_id = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &size);
		if (array_type_id < 0) exit(1);
		hid_t record_id = H5Tcreate(H5T_COMPOUND, sizeof(Decl_hdf5_struct::Record_type));
		if (record_id < 0) exit(1);
		herr_t status = H5Tinsert(record_id, "size", HOFFSET(Decl_hdf5_struct::Record_type, size), H5T_NATIVE_INT);
		if (status < 0) exit(1);
		status = H5Tinsert(record_id, "data", HOFFSET(Decl_hdf5_struct::Record_type, data), array_type_id);
		if (status < 0) exit(1);
		hid_t dataset_id = H5Dcreate(file_id, "record_data", record_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
		if (dataset_id < 0) exit(1);
		status = H5Dwrite(dataset_id, record_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
		if (status < 0) exit(1);
		status = H5Tclose(array_type_id);
		if (status < 0) exit(1);
		status = H5Tclose(record_id);
		if (status < 0) exit(1);
		status = H5Sclose(dataspace_id);
		if (status < 0) exit(1);
		status = H5Dclose(dataset_id);
		if (status < 0) exit(1);
		status = H5Fclose(file_id);
		if (status < 0) exit(1);
	}
}

BENCHMARK(HDF5_write)->Name("Decl_hdf5_struct/HDF5_write");

static void PDI_read(benchmark::State& state)
{
	Decl_hdf5_struct::Record_type record_data;
	record_data.size = Decl_hdf5_struct::data_size;
	for (int i = 0; i < Decl_hdf5_struct::data_size; i++) {
		record_data.data[i] = i * 1.23;
	}

	// create file
	hid_t file_id = H5Fcreate("record_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) exit(1);
	hid_t dataspace_id = H5Screate(H5S_SCALAR);
	if (dataspace_id < 0) exit(1);
	hsize_t size = Decl_hdf5_struct::data_size;
	hid_t array_type_id = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &size);
	if (array_type_id < 0) exit(1);

	hid_t record_id = H5Tcreate(H5T_COMPOUND, sizeof(Decl_hdf5_struct::Record_type));
	if (record_id < 0) exit(1);
	herr_t status = H5Tinsert(record_id, "size", HOFFSET(Decl_hdf5_struct::Record_type, size), H5T_NATIVE_INT);
	if (status < 0) exit(1);
	status = H5Tinsert(record_id, "data", HOFFSET(Decl_hdf5_struct::Record_type, data), array_type_id);
	if (status < 0) exit(1);
	hid_t dataset_id = H5Dcreate(file_id, "record_data", record_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) exit(1);
	status = H5Dwrite(dataset_id, record_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
	if (status < 0) exit(1);
	status = H5Tclose(array_type_id);
	if (status < 0) exit(1);
	status = H5Tclose(record_id);
	if (status < 0) exit(1);
	status = H5Sclose(dataspace_id);
	if (status < 0) exit(1);
	status = H5Dclose(dataset_id);
	if (status < 0) exit(1);
	status = H5Fclose(file_id);
	if (status < 0) exit(1);

	const char* CONFIG_YAML
		= "logging: off                                                  \n"
		  "metadata:                                                     \n"
		  "  size: int                                                   \n"
		  "data:                                                         \n"
		  "  record_data:                                                \n"
		  "    type: struct                                              \n"
		  "    members:                                                  \n"
		  "      - size: int                                             \n"
		  "      - data:                                                 \n"
		  "          type: array                                         \n"
		  "          subtype: double                                     \n"
		  "          size: $size                                         \n"
		  "plugins:                                                      \n"
		  "  decl_hdf5:                                                  \n"
		  "    file: record_data.h5                                      \n"
		  "    collision_policy: replace                                 \n"
		  "    read: [record_data]                                      \n";


	PDI_init(PC_parse_string(CONFIG_YAML));
	int data_size = Decl_hdf5_struct::data_size;
	PDI_expose("size", &data_size, PDI_OUT);
	for (auto _: state) {
		PDI_expose("record_data", &record_data, PDI_IN);
	}
	PDI_finalize();
}

BENCHMARK(PDI_read)->Name("Decl_hdf5_struct/PDI_read");

static void HDF5_read(benchmark::State& state)
{
	Decl_hdf5_struct::Record_type record_data;
	record_data.size = Decl_hdf5_struct::data_size;
	for (int i = 0; i < Decl_hdf5_struct::data_size; i++) {
		record_data.data[i] = i * 1.23;
	}

	//create file
	hid_t file_id = H5Fcreate("record_data.h5", H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
	if (file_id < 0) exit(1);
	hid_t dataspace_id = H5Screate(H5S_SCALAR);
	if (dataspace_id < 0) exit(1);
	hsize_t size = Decl_hdf5_struct::data_size;
	hid_t array_type_id = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &size);
	if (array_type_id < 0) exit(1);

	hid_t record_id = H5Tcreate(H5T_COMPOUND, sizeof(Decl_hdf5_struct::Record_type));
	if (record_id < 0) exit(1);
	herr_t status = H5Tinsert(record_id, "size", HOFFSET(Decl_hdf5_struct::Record_type, size), H5T_NATIVE_INT);
	if (status < 0) exit(1);
	status = H5Tinsert(record_id, "data", HOFFSET(Decl_hdf5_struct::Record_type, data), array_type_id);
	if (status < 0) exit(1);
	hid_t dataset_id = H5Dcreate(file_id, "record_data", record_id, dataspace_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
	if (dataset_id < 0) exit(1);
	status = H5Dwrite(dataset_id, record_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
	if (status < 0) exit(1);
	status = H5Tclose(array_type_id);
	if (status < 0) exit(1);
	status = H5Tclose(record_id);
	if (status < 0) exit(1);
	status = H5Sclose(dataspace_id);
	if (status < 0) exit(1);
	status = H5Dclose(dataset_id);
	if (status < 0) exit(1);
	status = H5Fclose(file_id);
	if (status < 0) exit(1);

	for (auto _: state) {
		hid_t file_id = H5Fopen("record_data.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
		if (file_id < 0) exit(1);
		hid_t dataset_id = H5Dopen2(file_id, "record_data", H5P_DEFAULT);
		if (dataset_id < 0) exit(1);
		hsize_t size = Decl_hdf5_struct::data_size;
		hid_t array_type_id = H5Tarray_create(H5T_NATIVE_DOUBLE, 1, &size);
		if (array_type_id < 0) exit(1);

		hid_t record_id = H5Tcreate(H5T_COMPOUND, sizeof(Decl_hdf5_struct::Record_type));
		if (record_id < 0) exit(1);
		herr_t status = H5Tinsert(record_id, "size", HOFFSET(Decl_hdf5_struct::Record_type, size), H5T_NATIVE_INT);
		if (status < 0) exit(1);
		status = H5Tinsert(record_id, "data", HOFFSET(Decl_hdf5_struct::Record_type, data), array_type_id);
		if (status < 0) exit(1);
		status = H5Dread(dataset_id, record_id, H5S_ALL, H5S_ALL, H5P_DEFAULT, &record_data);
		if (status < 0) exit(1);
		status = H5Tclose(array_type_id);
		if (status < 0) exit(1);
		status = H5Tclose(record_id);
		if (status < 0) exit(1);
		status = H5Dclose(dataset_id);
		if (status < 0) exit(1);
		status = H5Fclose(file_id);
		if (status < 0) exit(1);
	}
}

BENCHMARK(HDF5_read)->Name("Decl_hdf5_struct/HDF5_read");
