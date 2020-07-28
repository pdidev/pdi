/*******************************************************************************
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
#include <pdi.h>

// Tests group and variable definitions
int main(int argc, char* argv[])
{	
	PDI_init(PC_parse_path(argv[1]));

    // init data
    int int_scalar = 42;
    int int_array[32];
    for (int i = 0; i < 32; i++) {
        int_array[i] = i;
    }

    // expose attributes
    int scalar_attr = 100;
    PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
    int array_attr = 101;
    PDI_expose("array_attr", &array_attr, PDI_OUT);
    int scalar_group_attr = 200;
    PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);
    int array_group_attr = 201;
    PDI_expose("array_group_attr", &array_group_attr, PDI_OUT);

    // write data
    PDI_multi_expose("write",
                     "int_scalar", &int_scalar, PDI_OUT,
                     "int_array", int_array, PDI_OUT,
                     NULL);


    // zero data
    int_scalar = 0;
    for (int i = 0; i < 32; i++) {
        int_array[i] = 0;
    }

    // reset metadata
    scalar_attr = 0;
    PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
    array_attr = 0;
    PDI_expose("array_attr", &array_attr, PDI_OUT);
    scalar_group_attr = 0;
    PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);
    array_group_attr = 0;
    PDI_expose("array_group_attr", &array_group_attr, PDI_OUT);

    // read data  
    PDI_multi_expose("read",
                     "int_scalar", &int_scalar, PDI_IN,
                     "int_array", int_array, PDI_IN,
                     "scalar_attr", &scalar_attr, PDI_INOUT,
                     "scalar_group_attr", &scalar_group_attr, PDI_INOUT,
                     "array_attr", &array_attr, PDI_INOUT,
                     "array_group_attr", &array_group_attr, PDI_INOUT,
                     NULL);

    
    // verify
    printf("scalar_group_attr: %d ?= %d\n", scalar_group_attr, 200);
    assert(scalar_group_attr == 200);
    
    printf("scalar_attr: %d ?= %d\n", scalar_attr, 100);
    assert(scalar_attr == 100);

    printf("array_attr: %d ?= %d\n", array_attr, 101);
    assert(array_attr == 101);

    printf("array_group_attr: %d ?= %d\n", array_group_attr, 201);
    assert(array_group_attr == 201);
    
    printf("int_scalar: %d ?= %d\n", int_scalar, 42);
    assert(int_scalar == 42);
    for (int i = 0; i < 32; i++) {
        printf("%d ?= %d\n", int_array[i], i);
        assert(int_array[i] == i);
    }

    PDI_finalize();
    return 0;
}
