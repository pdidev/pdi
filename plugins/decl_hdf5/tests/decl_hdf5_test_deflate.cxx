/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   forcumentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   enforrse or promote products derived from this software without specific
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

#include <unistd.h>
#include <pdi.h>
#include <time.h>
#include <paraconf.h>
#include <gtest/gtest.h>
#include <hdf5.h>

TEST(decl_hdf5_deflate, no_deflate) 
 {

    const char* CONFIG_YAML =
        "logging: trace                                                          \n"
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
    size_t N=1000;
    PDI_expose("pb_size", &N, PDI_OUT);

    double ** matrix_data =  new double*[N];
    for (int i = 0; i < N; i++)
    {
        matrix_data[i] = new double[N];
        for (int j = 0; j < N; j++)
        {
            matrix_data[i][j] = N * i + j;
        }
    }
    PDI_expose("matrix_data", matrix_data, PDI_OUT);

    PDI_finalize();
    PC_tree_destroy(&conf);

    for(int i = 0; i < N; i++) delete [] matrix_data[i];
    delete [] matrix_data;

    hid_t file_id, dataset_id, plist_id;
    herr_t status;
    unsigned int compression_level;
    H5Z_filter_t filter_type;
    size_t cd_nelmts=1;

    file_id = H5Fopen("decl_hdf5_test_comp_lev0.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
    plist_id = H5Dget_create_plist(dataset_id);

    status = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &compression_level, 0, NULL, NULL);

    ASSERT_LT(status, 0);
    
    H5Pclose(plist_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);

 }

 TEST(decl_hdf5_deflate, deflate_level1)
 {
    
    const char* CONFIG_YAML =
        "logging: trace                                                          \n"
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
    size_t N=1000;
    PDI_expose("pb_size", &N, PDI_OUT);

    double ** matrix_data =  new double*[N];
    for (int i = 0; i < N; i++)
    {
        matrix_data[i] = new double[N];
        for (int j = 0; j < N; j++)
        {
            matrix_data[i][j] = N * i + j;
        }
    }
    PDI_expose("matrix_data", matrix_data, PDI_OUT);

    PDI_finalize();
    PC_tree_destroy(&conf);

    for(int i = 0; i < N; i++) delete [] matrix_data[i];
    delete [] matrix_data;

    hid_t file_id, dataset_id, plist_id;
    herr_t status;
    unsigned int compression_level;
    H5Z_filter_t filter_type;
    size_t cd_nelmts=1;

    file_id = H5Fopen("decl_hdf5_test_comp_lev1.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
    plist_id = H5Dget_create_plist(dataset_id);

    status = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &compression_level, 0, NULL, NULL);

    ASSERT_EQ(compression_level, 1);
    
    H5Pclose(plist_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);

 }

 TEST(decl_hdf5_deflate, deflate_level2)
 {
    
    const char* CONFIG_YAML =
        "logging: trace                                                          \n"
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
    size_t N=1000;
    PDI_expose("pb_size", &N, PDI_OUT);

    double ** matrix_data =  new double*[N];
    for (int i = 0; i < N; i++)
    {
        matrix_data[i] = new double[N];
        for (int j = 0; j < N; j++)
        {
            matrix_data[i][j] = N * i + j;
        }
    }
    PDI_expose("matrix_data", matrix_data, PDI_OUT);

    PDI_finalize();
    PC_tree_destroy(&conf);

    for(int i = 0; i < N; i++) delete [] matrix_data[i];
    delete [] matrix_data;

    hid_t file_id, dataset_id, plist_id;
    herr_t status;
    unsigned int compression_level;
    H5Z_filter_t filter_type;
    size_t cd_nelmts=1;

    file_id = H5Fopen("decl_hdf5_test_comp_lev2.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
    plist_id = H5Dget_create_plist(dataset_id);

    status = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &compression_level, 0, NULL, NULL);

    ASSERT_EQ(compression_level, 2);
    
    H5Pclose(plist_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);

 }

 TEST(decl_hdf5_deflate, deflate_level9)
 {
    
    const char* CONFIG_YAML =
        "logging: trace                                                          \n"
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
    size_t N=1000;
    PDI_expose("pb_size", &N, PDI_OUT);

    double ** matrix_data =  new double*[N];
    for (int i = 0; i < N; i++)
    {
        matrix_data[i] = new double[N];
        for (int j = 0; j < N; j++)
        {
            matrix_data[i][j] = N * i + j;
        }
    }
    PDI_expose("matrix_data", matrix_data, PDI_OUT);

    PDI_finalize();
    PC_tree_destroy(&conf);

    for(int i = 0; i < N; i++) delete [] matrix_data[i];
    delete [] matrix_data;

    hid_t file_id, dataset_id, plist_id;
    herr_t status;
    unsigned int compression_level;
    H5Z_filter_t filter_type;
    size_t cd_nelmts=1;

    file_id = H5Fopen("decl_hdf5_test_comp_lev9.h5", H5F_ACC_RDONLY, H5P_DEFAULT);
    dataset_id = H5Dopen2(file_id, "matrix_data", H5P_DEFAULT);
    plist_id = H5Dget_create_plist(dataset_id);

    status = H5Pget_filter_by_id2(plist_id, H5Z_FILTER_DEFLATE, NULL, &cd_nelmts, &compression_level, 0, NULL, NULL);

    ASSERT_EQ(compression_level, 9);
    
    H5Pclose(plist_id);
    H5Dclose(dataset_id);
    H5Fclose(file_id);

 }