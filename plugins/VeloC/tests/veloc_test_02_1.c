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
    "    manual_checkpoint:\n"
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

    for(ii=0; ii<3; ii++){
        a = 2*ii;

        PDI_multi_expose("start", "ii", &ii, PDI_OUT,
                                  "a",  &a,  PDI_OUT, NULL);
        PDI_multi_expose("route", "veloc_file", veloc_file, PDI_INOUT, NULL);
        assert(veloc_file[0] != '\0' && "veloc_file was not filled by route event");
        
        PDI_share("a", &a, PDI_OUT);

        hid_t file_id  = H5Fcreate(veloc_file, H5F_ACC_TRUNC, H5P_DEFAULT, H5P_DEFAULT);
        assert(file_id >= 0 && "HDF5 file creation failed");
        hid_t space_id = H5Screate(H5S_SCALAR);
        hid_t dset_id  = H5Dcreate(file_id, "a", H5T_NATIVE_INT, space_id,
                                    H5P_DEFAULT, H5P_DEFAULT, H5P_DEFAULT);
        assert(dset_id >= 0 && "HDF5 dataset creation failed");
        H5Dwrite(dset_id, H5T_NATIVE_INT, H5S_ALL, H5S_ALL, H5P_DEFAULT, &a);
        H5Dclose(dset_id);
        H5Sclose(space_id);
        H5Fclose(file_id);
        assert(H5Fis_hdf5(veloc_file) > 0 && "routed file is not a valid HDF5 file");

        PDI_event("end");
        PDI_reclaim("a");
    }

    printf("TEST 02_1 PASSED ");

    PDI_finalize();
    MPI_Finalize();

    return 0;
}