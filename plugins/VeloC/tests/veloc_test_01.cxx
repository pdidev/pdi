/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <veloc.h>

#include <iostream>
#include <filesystem>
#include <cassert>
#include <fstream>    
#include <vector>     

using std::filesystem::exists;
using std::cout;
using std::endl;

const int FAILURE_ITER = 30;
const int MAX_ITER = 60;

const char* CONFIG_YAML =
  "pdi:                                                            \n"
  "  metadata:                                                     \n"
  "    ii: int                                                     \n"
  "  data:                                                         \n"
  "    red:  {type: array, subtype: double, size: 10}              \n"
  "    blue: {type: array, subtype: double, size: 10}              \n"
  "  plugins:                                                      \n"
  "    veloc:                                                      \n"
  "      failure: 1                                                \n"
  "      checkpoint_label: \"test01\"                               \n"
  "      veloc_cfg_path: \"./veloc_test.cfg\"                       \n"
  "      iteration_name_in_cp_file: \"ii\"                          \n"
  "      checkpoint_data: [ii, red, blue]                          \n"
  "      when: '$ii % 10 = 0'                                      \n"
  "      on_event: \"checkpoint\"                                  \n";



int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
	MPI_Comm world = MPI_COMM_WORLD;
    int rank;
	MPI_Comm_rank(world, &rank);

    // create directories defined in veloc_test_01.cfg
    const char* dirs[] = {"./scratchdir/", "./persdir/"};
    mode_t mode = 0755;  // Owner rwx, group rx, others rx
    for (int i = 0; i < 2; ++i) {
        if (mkdir(dirs[i], mode) == 0) {
            printf("Created directory: %s\n", dirs[i]);
        } 
        else {
            if (errno != EEXIST) {
                perror("mkdir failed");
            }
            printf("Directory already exists: %s\n", dirs[i]);
        }
    }

    double red[10] = {0};
    double blue[10] = {0};
    int ii = 0;

    double red_at_failure[10] = {30,30,30,30,30,30,30,30,30,30};
    double blue_at_failure[10] = {60,60,60,60,60,60,60,60,60,60};
    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
    PDI_init(PC_get(conf, ".pdi"));

    for (; ii < MAX_ITER; ++ii) {

        PDI_multi_expose("loop", 
            "ii", &ii, PDI_INOUT,
            "red", red, PDI_INOUT,
            "blue", blue, PDI_INOUT,
            NULL);

        if(ii == FAILURE_ITER+1) {

            for (int i =0; i < 10 ; i++){

            assert(red_at_failure[i] == red[i] && "Test 1 failed : Data recovered does not match data before failure");
            assert(blue_at_failure[i] == blue[i] && "Test 1 failed : Data recovered does not match data before failure");
            }

        }

        for (int i = 0; i < 10; ++i){
            red[i] = ii;
            blue[i] = ii*2;
        }

        PDI_event("checkpoint");
    }


    PDI_multi_expose("finalization", 
            "ii", &ii, PDI_INOUT,
            "red", red, PDI_INOUT,
            "blue", blue, PDI_INOUT,
            NULL);

    PDI_event("checkpoint");

    assert(exists("./persdir/test01-0-60.dat") && "Test 1 failed : Checkpoint file not found/");

    std::cout << "Test Passed" << std::endl; 

    PDI_finalize();
    MPI_Finalize();

    return 1; 

}
