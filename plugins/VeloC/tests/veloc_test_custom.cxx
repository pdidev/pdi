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

#include <gtest/gtest.h>
#include <pdi/testing.h>

#include <mpi.h>
#include <hdf5.h>
#include <pdi.h>


class VeloCCustomCheckpoint: public ::PDI::PdiTest
{};

TEST_F(VeloCCustomCheckpoint, CheckpointRecoverLatest)
{
    MPI_Init(nullptr, nullptr);

    InitPdi(PC_parse_string((std::string(R"==(
metadata:
  ii : int 
data:
  veloc_file : {type: array, subtype: char, size: 256}
  var: int 
  cp_status: int
plugins:
  veloc:
    config_file: )==") + VELOC_CONFIG_FILE + R"==(
    checkpoint_label: custom_test_1
    iteration: ii
    status: cp_status 
    custom_checkpointing:
      veloc_file: veloc_file
      custom_checkpoint:
        filename: file1.h5
        start_on_event: c_start
        route_file_on_event: c_route
        end_on_event: c_end
      custom_recover:
        filename: file1.h5
        start_on_event: r_start
        route_file_on_event: r_route
        end_on_event: r_end
)==").c_str()));

    int ii = 0;
    int var = 0;
    char veloc_file[256];

    /* Checkpoint Part */ 

    for (ii = 0; ii < 3; ii++) {
        var = 2 * ii;

        // start checkpoint phase 
        PDI_multi_expose("c_start", "ii", &ii, PDI_OUT, "var", &var, PDI_OUT, NULL);

        // route checkpoint file 
        PDI_multi_expose("c_route", "veloc_file", veloc_file, PDI_INOUT, NULL);

        ASSERT_NE(veloc_file[0], '\0') << "veloc_file was not filled by route event";

        PDI_share("var", &var, PDI_OUT);

        // manual write 
        hid_t file_id = H5Fcreate(veloc_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        ASSERT_GE(file_id, 0) << "HDF5 file creation failed for: " << veloc_file;

        hid_t space_id = H5Screate(H5S_SCALAR);
        hid_t dset_id = H5Dcreate(file_id, "var", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (dset_id < 0) {
            H5Sclose(space_id);
            H5Fclose(file_id);
            FAIL() << "HDF5 dataset creation failed";
        }

        herr_t write_status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &var);
        if (write_status < 0) {
            H5Dclose(dset_id);
            H5Sclose(space_id);
            H5Fclose(file_id);
            FAIL() << "HDF5 write failed at iteration " << ii;
        }

        H5Dclose(dset_id);
        H5Sclose(space_id);
        H5Fclose(file_id);

        EXPECT_GE(H5Fis_hdf5(veloc_file), 0) << "routed file is not a valid HDF5 file: " << veloc_file;

        PDI_event("c_end");
        PDI_reclaim("var");
    }

    /* Recovery Part */ 

    int cp_status = 0; 

    // write status "recovery needed"
    PDI_expose("cp_status", &cp_status, PDI_OUT);

    // start recovery phase of the latest checkpoint 
    PDI_event("r_start");

    // route checkpoint to recover 
    PDI_multi_expose("r_route", "veloc_file", veloc_file, PDI_INOUT, NULL);

    // manual read 
    hid_t rec_file_id = H5Fopen(veloc_file, H5F_ACC_RDONLY, H5P_DEFAULT);
    ASSERT_GE(rec_file_id, 0) << "failed to open HDF5 file for reading: " << veloc_file;

    hid_t rec_dset_id = H5Dopen(rec_file_id, "var", H5P_DEFAULT);
    if (rec_dset_id < 0) {
        H5Fclose(rec_file_id);
        FAIL() << "failed to open dataset 'var'";
    }

    var = -1; 
    herr_t read_status = H5Dread(rec_dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &var);
    if (read_status < 0) {
        H5Dclose(rec_dset_id);
        H5Fclose(rec_file_id);
        FAIL() << "HDF5 read failed";
    }

    int expected_var = 4;
    EXPECT_EQ(var, expected_var);

    H5Dclose(rec_dset_id);
    H5Fclose(rec_file_id);

    PDI_event("r_end");

    FinalizePdi();
    MPI_Finalize();
}

TEST_F(VeloCCustomCheckpoint, CheckpointRecoverFromIter)
{
    MPI_Init(nullptr, nullptr);
    InitPdi(PC_parse_string((std::string(R"==(
metadata:
  ii : int 
data:
  veloc_file : {type: array, subtype: char, size: 256}
  cp_status : int 
plugins:
  veloc:
    config_file: )==") + VELOC_CONFIG_FILE + R"==(
    checkpoint_label: custom_test_2
    iteration: ii
    status : cp_status  
    custom_checkpointing:
      veloc_file: veloc_file
      custom_checkpoint:
        filename: file1.h5
        start_on_event: c_start
        route_file_on_event: c_route
        end_on_event: c_end
      custom_recover:
        filename: file1.h5
        recover_from_iteration: 1
        start_on_event: r_start
        route_file_on_event: r_route
        end_on_event: r_end
)==").c_str()));

    int ii = 0;
    int var = 0;
    char veloc_file[256];

    /* Checkpoint Part */ 

    for (ii = 0; ii < 3; ii++) {
        var = 2 * ii;

        // start checkpoint phase 
        PDI_multi_expose("c_start", "ii", &ii, PDI_OUT, NULL);

        // route checkpoint file 
        PDI_multi_expose("c_route", "veloc_file", veloc_file, PDI_INOUT, NULL);

        ASSERT_NE(veloc_file[0], '\0') << "veloc_file was not filled by route event";

        // manual write 
        hid_t file_id = H5Fcreate(veloc_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        ASSERT_GE(file_id, 0) << "HDF5 file creation failed for: " << veloc_file;

        hid_t space_id = H5Screate(H5S_SCALAR);
        hid_t dset_id = H5Dcreate(file_id, "var", H5T_NATIVE_INT, space_id, H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        if (dset_id < 0) {
            H5Sclose(space_id);
            H5Fclose(file_id);
            FAIL() << "HDF5 dataset creation failed";
        }

        herr_t write_status = H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &var);
        if (write_status < 0) {
            H5Dclose(dset_id);
            H5Sclose(space_id);
            H5Fclose(file_id);
            FAIL() << "HDF5 write failed at iteration " << ii;
        }

        H5Dclose(dset_id);
        H5Sclose(space_id);
        H5Fclose(file_id);

        EXPECT_GE(H5Fis_hdf5(veloc_file), 0) << "routed file is not a valid HDF5 file: " << veloc_file;

        PDI_event("c_end");
    }

    /* Recovery Part */ 

    int cp_status = 0; 

    // write status "recovery needed"
    PDI_expose("cp_status", &cp_status, PDI_OUT);

    // start recovery phase of second to last checkpoint 
    PDI_event("r_start");

    // route checkpoint to recover 
    PDI_multi_expose("r_route", "veloc_file", veloc_file, PDI_INOUT, NULL);

    // manual read 
    hid_t rec_file_id = H5Fopen(veloc_file, H5F_ACC_RDONLY, H5P_DEFAULT);
    ASSERT_GE(rec_file_id, 0) << "failed to open HDF5 file for reading: " << veloc_file;

    hid_t rec_dset_id = H5Dopen(rec_file_id, "var", H5P_DEFAULT);
    if (rec_dset_id < 0) {
        H5Fclose(rec_file_id);
        FAIL() << "failed to open dataset 'var'";
    }

    var = -1; 
    herr_t read_status = H5Dread(rec_dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &var);
    if (read_status < 0) {
        H5Dclose(rec_dset_id);
        H5Fclose(rec_file_id);
        FAIL() << "HDF5 read failed";
    }

    int expected_var = 2;
    EXPECT_EQ(var, expected_var);

    H5Dclose(rec_dset_id);
    H5Fclose(rec_file_id);

    PDI_event("r_end");

    FinalizePdi();
    MPI_Finalize();
}