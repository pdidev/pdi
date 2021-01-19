/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <assert.h>
#include <unistd.h>
#include <pdi.h>

const char* CONFIG_YAML =
    "logging: trace                \n"
    "metadata:                     \n"
    "  meta0: int                  \n"
    "  meta1: int                  \n"
    "  meta2: int                  \n"
    "  meta3: int                  \n"
    "  meta4: int                  \n"
    "data:                         \n"
    "  test_var: double            \n"
    "plugins:                      \n"
    "  decl_hdf5:                  \n"
    "    file: ${meta1}.h5         \n"
    "    write: [ test_var, meta2 ]\n"
    ;

int main( int argc, char* argv[] )
{
	int value[5] = {5,4,3,2,1};
	double test_var = 0;
	
	remove("5.h5");
	
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	
	PDI_multi_expose("testing",
	    "meta0",&value[0], PDI_OUT,
	    "meta1",&value[0], PDI_OUT,
	    "meta2",&value[1], PDI_OUT,
	    "meta3",&value[2], PDI_OUT,
	    "meta4",&value[3], PDI_OUT,
	    "test_var",&test_var, PDI_OUT,
	    NULL);
	    
	PDI_finalize();
	PC_tree_destroy(&conf);
	
	FILE* fp = fopen("5.h5", "r");
	assert( fp != NULL && "File not found.");
	fclose(fp);
}
