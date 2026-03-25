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
#include <sys/stat.h>    

using std::filesystem::exists;
using std::cout;
using std::endl;

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
  "      checkpoint_label: \"test\"                               \n"
  "      veloc_cfg_path: \"./veloc_test.cfg\"                       \n"
  "      iteration_name_in_cp_file: \"ii\"                          \n"
  "      checkpoint_data: [ii, red, blue]                          \n"
  "      when: '$ii % 10 = 0'                                      \n"
  "      on_event: \"checkpoint\"                                  \n";

const int FAILURE_ITER = 20;
const int MAX_ITER = 60;

int main(int argc, char* argv[])
{
    MPI_Init(&argc, &argv);
	MPI_Comm world = MPI_COMM_WORLD;
    int rank;
	MPI_Comm_rank(world, &rank);

    // check that directories exist 
    const char* dirs[] = {"./scratchdir/", "./persdir/"};
    
    for (int i = 0; i < 2; ++i) {
        if(!std::filesystem::exists(dirs[i])){
            cout << "Checkpoint directories not found. "<< 
            "Make sure to run veloc_test_0a before running this test" << endl; 
            return 1;
        }
    }


    // change last checkpoint to trigger checksum error 
    if (!std::filesystem::exists("./persdir/test-0-30.dat")) {
        std::cerr << "ERROR: test-0-30.dat does not exist in persdir\n";
        return 1;
    }

    std::ofstream file;
    file.open("./persdir/test-0-30.dat", std::ios::app);
    if (!file) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }

    // Append character to file to trigger checksum error 
    std::string append_error = "#try123";
    file << append_error;
    file.flush(); 
    file.close();


    std::ofstream file2;
    file2.open("./scratchdir/test-0-30.dat", std::ios::app);
    if (!file2) {
        std::cerr << "Error opening file!" << std::endl;
        return 1;
    }
    file2 << append_error;
    file2.flush(); 
    file2.close();
    
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
    PDI_init(PC_get(conf, ".pdi"));

    double red[10] = {0};
    double blue[10] = {1};
    int ii = 0;
    double red_at_failure[10] = {0, 20, 40, 60, 80, 100, 120, 140, 160, 180};
    double blue_at_failure[10] = {1, 21, 41, 61, 81, 101, 121, 141, 161, 181};

    for (; ii < MAX_ITER; ++ii) {

        PDI_multi_expose("loop", 
            "ii", &ii, PDI_INOUT,
            "red", red, PDI_INOUT,
            "blue", blue, PDI_INOUT,
            NULL);

        if(ii == (FAILURE_ITER)) {
            for (int i =0; i < 10 ; i++){
                assert(red_at_failure[i] == red[i] && 
                    "Test 1 failed : Data recovered does not match data before failure");
                assert(blue_at_failure[i] == blue[i] && 
                    "Test 1 failed : Data recovered does not match data before failure");
            }
        }

        for (int i = 0; i < 10; ++i){
            if(i > 0){
                red[i] = red[i-1] + ii;
                blue[i] = blue[i-1] + ii;
            }
        }

        PDI_event("checkpoint");
    }

    PDI_multi_expose("finalization", 
            "ii", &ii, PDI_INOUT,
            "red", red, PDI_INOUT,
            "blue", blue, PDI_INOUT,
            NULL);

    PDI_event("checkpoint");

    PDI_event("assert_nr_checkpoints");

    std::ifstream f("veloc_cp_count.txt");
    long int count = -1;
    f >> count;
    f.close();
    cout << "count is = " << count << endl; 
    std::filesystem::remove("veloc_cp_count.txt");

    assert(count == 4 && "write_checkpoint() was called exactly 4 times");

    assert(exists("./persdir/test-0-0.dat") && "Test 1 failed : Checkpoint file not found/");
    assert(exists("./persdir/test-0-10.dat") && "Test 1 failed : Checkpoint file not found/");
    assert(exists("./persdir/test-0-20.dat") && "Test 1 failed : Checkpoint file not found/");
    assert(exists("./persdir/test-0-30.dat") && "Test 1 failed : Checkpoint file not found/");

    cout << "Test 0 passed " << endl;

    PDI_finalize();
    MPI_Finalize();

    return 1; 

}
