#include <iostream>
#include <mpi.h>
#include <pdi.h>
#include <hdf5.h>

const char CONF_YAML[] =
    "data:\n"
    "  var: int\n"
    "  ii: int\n"
    "  veloc_file: {type: array, subtype: char, size: 256}\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 1\n"
    "    config_file: veloc_config.cfg\n"
    "    checkpoint_label: test_03\n"
    "    iteration: ii\n"
    "    custom_checkpointing:\n"
    "       veloc_file: veloc_file\n"
    "       manual_recover:\n"
    "           original_file: file1.h5\n"
    "           checkpoint_nr: 1\n"
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

    int expected_var = 2;

    PDI_multi_expose("start", "var", &var, PDI_OUT, NULL);

    PDI_multi_expose("route", "veloc_file", veloc_file, PDI_INOUT, NULL);
    if (veloc_file[0] == '\0') {
        std::cerr << "TEST_03_3 FAILED: veloc_file was not filled by route event" << std::endl;
        exit(1);
    }

    PDI_share("var", &var, PDI_OUT);

    if (H5Fis_hdf5(veloc_file) <= 0) {
        std::cerr << "ERROR: routed file is not a valid HDF5 file: " << veloc_file << std::endl;
        exit(1);
    }

    hid_t file_id = H5Fopen(veloc_file, H5F_ACC_RDONLY, H5P_DEFAULT);
    if (file_id < 0) {
        std::cerr << "ERROR: failed to open HDF5 file for reading: " << veloc_file << std::endl;
        exit(1);
    }

    // Fixed: dataset renamed from "a" to "var"
    hid_t dset_id = H5Dopen(file_id, "var", H5P_DEFAULT);
    if (dset_id < 0) {
        std::cerr << "ERROR: failed to open dataset 'var'" << std::endl;
        H5Fclose(file_id);
        exit(1);
    }

    int read_var = -1;
    herr_t read_status = H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &read_var);
    if (read_status < 0) {
        std::cerr << "ERROR: HDF5 read failed" << std::endl;
        H5Dclose(dset_id);
        H5Fclose(file_id);
        exit(1);
    }

    if (read_var != expected_var) {
        std::cerr << "TEST_03_3 FAILED: dataset value " << read_var
                  << " does not match expected value " << expected_var << std::endl;
        H5Dclose(dset_id);
        H5Fclose(file_id);
        exit(1);
    }

    H5Dclose(dset_id);
    H5Fclose(file_id);

    PDI_event("end");
    PDI_reclaim("var");

    std::cout << "TEST 03_3 PASSED" << std::endl;

    PDI_finalize();
    MPI_Finalize();

    return 0;
}