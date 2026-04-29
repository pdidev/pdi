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

#include <hdf5.h>
#include <pdi.h>

const char CONF_YAML[] =
    "data:\n"
    "  var: int\n"
    "  ii: int\n"
    "  veloc_file: {type: array, subtype: char, size: 256}\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 0\n"
    "    config_file: veloc_config.cfg\n"
    "    checkpoint_label: test_03\n"
    "    iteration: ii\n"
    "    custom_checkpointing:\n"
    "       veloc_file: veloc_file\n"
    "       custom_checkpoint:\n"
    "           original_file: file1.h5\n"
    "           start_on: start\n"
    "           route_file_on: route\n"
    "           end_on: end\n";


int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
    PC_tree_t conf = PC_parse_string(CONF_YAML);
    PDI_init(conf);

    int size;
    MPI_Comm_size(MPI_COMM_WORLD, &size);

    if(size>1){
        std::cerr << "This test should be executed with only one MPI process" << std::endl;
        exit(1);
    }

    int ii = 0;
    int var = 0;
    char veloc_file[256];

    for (ii = 0; ii < 3; ii++) {
        var = 2 * ii;

        PDI_multi_expose("start", "ii", &ii, PDI_OUT,
                            "var", &var, PDI_OUT, NULL);

        PDI_multi_expose("route", "veloc_file", veloc_file, PDI_INOUT, NULL);
        
        if (veloc_file[0] == '\0') {
            std::cerr << "TEST 03_1 FAILED : veloc_file was not filled by route event" << std::endl;
            exit(1);
        }

        PDI_share("var", &var, PDI_OUT);

        hid_t file_id = H5Fcreate(veloc_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        if (file_id < 0) {
            std::cerr << "ERROR: HDF5 file creation failed for: " << veloc_file << std::endl;
            exit(1);
        }

        hid_t space_id = H5Screate(H5S_SCALAR);
        hid_t dset_id  = H5Dcreate(file_id, "var", H5T_NATIVE_INT, space_id,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (dset_id < 0) {
            std::cerr << "ERROR: HDF5 dataset creation failed" << std::endl;
            H5Sclose(space_id);
            H5Fclose(file_id);
            exit(1);
        }

        herr_t write_status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &var);
        if (write_status < 0) {
            std::cerr << "ERROR: HDF5 write failed at iteration " << ii << std::endl;
            H5Dclose(dset_id);
            H5Sclose(space_id);
            H5Fclose(file_id);
            exit(1);
        }

        H5Dclose(dset_id);
        H5Sclose(space_id);
        H5Fclose(file_id);

        if (H5Fis_hdf5(veloc_file) <= 0) {
            std::cerr << "TEST 03_1 FAILED : routed file is not a valid HDF5 file: " << veloc_file << std::endl;
            exit(1);
        }

        PDI_event("end");
        PDI_reclaim("var");
    }

    std::cout << "TEST 03_1 PASSED" << std::endl;

    PDI_finalize();
    MPI_Finalize();

    return 0;
}