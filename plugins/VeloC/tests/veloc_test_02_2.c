#include <stdio.h>
#include <assert.h>
#include <mpi.h>
#include <pdi.h>
#include <hdf5.h>

const char CONF_YAML[] =
    "data:\n"
    "  a: int\n"
    "  ii: int\n"
    "  veloc_file: {type: array, subtype: char, size: 256}\n"
    "plugins:\n"
    "  veloc:\n"
    "    failure: 0\n"
    "    config_file: veloc_config.cfg\n"
    "    checkpoint_label: test_02\n"
    "    iteration: ii\n"
    "    protect_data: [a]\n"
    "    manual_recovery:\n"
    "      original_file: file1.h5\n"
    "      veloc_file: veloc_file\n"
    "      start_on: start\n"
    "      route_file_on: route\n"
    "      end_on: end\n";


int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
    PC_tree_t conf = PC_parse_string(CONF_YAML);
    PDI_init(conf);

    int ii = 0;
    int a = 0;
    char veloc_file[256];

    int expected_a = 4;

    PDI_multi_expose("start", "a",  &a,  PDI_OUT, NULL);
    PDI_multi_expose("route", "veloc_file", veloc_file, PDI_INOUT, NULL);
    PDI_share("a", &a, PDI_OUT);

    assert(H5Fis_hdf5(veloc_file) > 0 && "routed file is not a valid HDF5 file");

    hid_t file_id = H5Fopen(veloc_file, H5F_ACC_RDONLY, H5P_DEFAULT);
    assert(file_id >= 0 && "failed to open HDF5 file for reading");

    hid_t dset_id = H5Dopen(file_id, "a", H5P_DEFAULT);
    assert(dset_id >= 0 && "failed to open dataset 'a'");

    int read_a = -1;
    H5Dread(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &read_a);
    assert(read_a == expected_a && "dataset value does not match expected value");

    H5Dclose(dset_id);
    H5Fclose(file_id);

    PDI_event("end");
    PDI_reclaim("a");



    printf("TEST 02_2 PASSED ");

    PDI_finalize();
    MPI_Finalize();

    return 0;
}