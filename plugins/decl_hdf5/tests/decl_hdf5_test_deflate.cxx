// SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <gtest/gtest.h>
#include <hdf5.h>
#include <paraconf.h>
#include <time.h>
#include <unistd.h>
#include <pdi.h>

/** check the deflate level set in a HDF5 plist
 * 
 * \param plist_id the plist identifier
 * \param[out] level set to the deflate level in the plist
 * \return whether deflate is set in the plist
 */
bool get_deflate_level(hid_t plist_id, unsigned int& level)
{
	int nfilters = H5Pget_nfilters(plist_id);
	for (int i = 0; i < nfilters; i++) {
		size_t nelmts = 1;
		unsigned int cd_value = 0;

		H5Z_filter_t filter = H5Pget_filter2(plist_id, i, nullptr, &nelmts, &cd_value, 0, nullptr, nullptr);

		if (filter == H5Z_FILTER_DEFLATE) {
			if (nelmts == 1) {
				level = cd_value;
				return true;
			}
		}
	}
	return false;
}

TEST(decl_hdf5_deflate, no_deflate)
{
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  pb_size: int                                                          \n"
		  "data:                                                                   \n"
		  "  matrix_data:                                                          \n"
		  "    size: ['$pb_size','$pb_size']                                       \n"
		  "    type: array                                                         \n"
		  "    subtype: double                                                     \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    - file: decl_hdf5_test_comp_lev0.h5                                 \n"
		  "      write:                                                            \n"
		  "        matrix_data:                                                    \n";


	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	size_t N = 1000;
	PDI_expose("pb_size", &N, PDI_OUT);

	std::vector<double> matrix_data(N * N);
	for (size_t i = 0; i < N; i++) {
		for (size_t j = 0; j < N; j++) {
			matrix_data[i * N + j] = N * i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data.data(), PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	hid_t file_id, dataset_id, plist_id;
	herr_t status;
	unsigned int compression_level;
	H5Z_filter_t filter_type;
	size_t cd_nelmts = 1;

	file_id = H5Fopen("decl_hdf5_test_comp_lev0.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	plist_id = H5Dget_create_plist(dataset_id);

	unsigned int level = 0;
	bool has_deflate = get_deflate_level(plist_id, level);

	ASSERT_FALSE(has_deflate);

	H5Pclose(plist_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}

TEST(decl_hdf5_deflate, deflate_level1)
{
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  pb_size: int                                                          \n"
		  "data:                                                                   \n"
		  "  matrix_data:                                                          \n"
		  "    size: ['$pb_size','$pb_size']                                       \n"
		  "    type: array                                                         \n"
		  "    subtype: double                                                     \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    - file: decl_hdf5_test_comp_lev1.h5                                 \n"
		  "      write:                                                            \n"
		  "        matrix_data:                                                    \n"
		  "          chunking: [16, 16]                                            \n"
		  "          deflate: 1                                                    \n";


	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	size_t N = 1000;
	PDI_expose("pb_size", &N, PDI_OUT);

	std::vector<double> matrix_data(N * N);
	for (size_t i = 0; i < N; i++) {
		for (size_t j = 0; j < N; j++) {
			matrix_data[i * N + j] = N * i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data.data(), PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	hid_t file_id, dataset_id, plist_id;
	herr_t status;
	unsigned int compression_level;
	H5Z_filter_t filter_type;
	size_t cd_nelmts = 1;

	file_id = H5Fopen("decl_hdf5_test_comp_lev1.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	plist_id = H5Dget_create_plist(dataset_id);

	unsigned int level = 0;
	bool has_deflate = get_deflate_level(plist_id, level);

	ASSERT_TRUE(has_deflate);
	ASSERT_EQ(level, 1);

	H5Pclose(plist_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}

TEST(decl_hdf5_deflate, deflate_level2)
{
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  pb_size: int                                                          \n"
		  "data:                                                                   \n"
		  "  matrix_data:                                                          \n"
		  "    size: ['$pb_size','$pb_size']                                       \n"
		  "    type: array                                                         \n"
		  "    subtype: double                                                     \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    - file: decl_hdf5_test_comp_lev2.h5                                 \n"
		  "      write:                                                            \n"
		  "        matrix_data:                                                    \n"
		  "          chunking: [16, 16]                                            \n"
		  "          deflate: 2                                                    \n";


	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	size_t N = 1000;
	PDI_expose("pb_size", &N, PDI_OUT);

	std::vector<double> matrix_data(N * N);
	for (size_t i = 0; i < N; i++) {
		for (size_t j = 0; j < N; j++) {
			matrix_data[i * N + j] = N * i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data.data(), PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	hid_t file_id, dataset_id, plist_id;
	herr_t status;
	unsigned int compression_level;
	H5Z_filter_t filter_type;
	size_t cd_nelmts = 1;

	file_id = H5Fopen("decl_hdf5_test_comp_lev2.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	plist_id = H5Dget_create_plist(dataset_id);

	unsigned int level = 0;
	bool has_deflate = get_deflate_level(plist_id, level);

	ASSERT_TRUE(has_deflate);
	ASSERT_EQ(level, 2);

	H5Pclose(plist_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}

TEST(decl_hdf5_deflate, deflate_level9)
{
	const char* CONFIG_YAML
		= "logging: trace                                                          \n"
		  "metadata:                                                               \n"
		  "  pb_size: int                                                          \n"
		  "data:                                                                   \n"
		  "  matrix_data:                                                          \n"
		  "    size: ['$pb_size','$pb_size']                                       \n"
		  "    type: array                                                         \n"
		  "    subtype: double                                                     \n"
		  "plugins:                                                                \n"
		  "  decl_hdf5:                                                            \n"
		  "    - file: decl_hdf5_test_comp_lev9.h5                                 \n"
		  "      write:                                                            \n"
		  "        matrix_data:                                                    \n"
		  "          chunking: [16, 16]                                            \n"
		  "          deflate: 9                                                    \n";


	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	size_t N = 1000;
	PDI_expose("pb_size", &N, PDI_OUT);

	std::vector<double> matrix_data(N * N);
	for (size_t i = 0; i < N; i++) {
		for (size_t j = 0; j < N; j++) {
			matrix_data[i * N + j] = N * i + j;
		}
	}
	PDI_expose("matrix_data", matrix_data.data(), PDI_OUT);

	PDI_finalize();
	PC_tree_destroy(&conf);

	hid_t file_id, dataset_id, plist_id;
	herr_t status;
	unsigned int compression_level;
	H5Z_filter_t filter_type;
	size_t cd_nelmts = 1;

	file_id = H5Fopen("decl_hdf5_test_comp_lev9.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
	dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
	plist_id = H5Dget_create_plist(dataset_id);

	unsigned int level = 0;
	bool has_deflate = get_deflate_level(plist_id, level);

	ASSERT_TRUE(has_deflate);
	ASSERT_EQ(level, 9);

	H5Pclose(plist_id);
	H5Dclose(dataset_id);
	H5Fclose(file_id);
}
