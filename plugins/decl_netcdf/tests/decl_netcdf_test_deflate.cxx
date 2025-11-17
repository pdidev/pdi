/*******************************************************************************
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

#include <pdi.h>
// #include <paraconf.h>
// #include <iostream>
// #include <cstdlib>
#include <gtest/gtest.h>

constexpr char pdi_config[] = R"(
logging: trace
metadata:
  pb_size: int
  input: int
  group_attr: int
  scalar_attr: int
  array_attr: {type: array, subtype: int, size: 4}
  scalar_group_attr: int
  array_group_attr: {type: array, subtype: int, size: 4}
  deflate: int
data:
  int_scalar: int
  int_array: {type: array, subtype: int, size: $pb_size}
plugins:
  decl_netcdf:
    - file: 'test_10_deflate_0.nc'
      variables:
        int_scalar:
          type: int
          group: 'scalar_group'
          deflate: 0 
          attributes: 
            scalar_attr: $scalar_attr
        int_array:
          type: array
          subtype: int
          size: $pb_size
          group: 'array_group'
          dimensions: ['time']
          deflate: 0
          attributes:
            array_attr: $array_attr
      groups:
        scalar_group:
          attributes:
              scalar_group_attr: $scalar_group_attr
        array_group:
          attributes:
              array_group_attr: $array_group_attr
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_10_deflate_2.nc'
      variables:
        int_scalar:
          type: int
          group: 'scalar_group'
          deflate: 2 
          attributes: 
            scalar_attr: $scalar_attr
        int_array:
          type: array
          subtype: int
          size: $pb_size
          group: 'array_group'
          dimensions: ['time']
          deflate: 2
          attributes:
            array_attr: $array_attr
      groups:
        scalar_group:
          attributes:
              scalar_group_attr: $scalar_group_attr
        array_group:
          attributes:
              array_group_attr: $array_group_attr
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_10_deflate_6.nc'
      variables:
        int_scalar:
          type: int
          group: 'scalar_group'
          deflate: 6 
          attributes: 
            scalar_attr: $scalar_attr
        int_array:
          type: array
          subtype: int
          size: $pb_size
          group: 'array_group'
          dimensions: ['time']
          deflate: 6
          attributes:
            array_attr: $array_attr
      groups:
        scalar_group:
          attributes:
              scalar_group_attr: $scalar_group_attr
        array_group:
          attributes:
              array_group_attr: $array_group_attr
      when: '${input}=0'
      write: [int_scalar, int_array]
    - file: 'test_10_deflate_9.nc'
      variables:
        int_scalar:
          type: int
          group: 'scalar_group'
          deflate: 9 
          attributes: 
            scalar_attr: $scalar_attr
        int_array:
          type: array
          subtype: int
          size: $pb_size
          group: 'array_group'
          dimensions: ['time']
          deflate: 9
          attributes:
            array_attr: $array_attr
      groups:
        scalar_group:
          attributes:
              scalar_group_attr: $scalar_group_attr
        array_group:
          attributes:
              array_group_attr: $array_group_attr
      when: '${input}=0'
      write: [int_scalar, int_array]
)";

// Tests simple write and read of scalar and array depending on event
int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(pdi_config));

	// init data
	int input = 0;
	int int_scalar = 42;
    int N=1000000;
	int* int_array = new int[N];
	int deflate = 1;
	for (int i = 0; i < N; i++) {
		int_array[i] = i;
	}

	// expose attributes
	int scalar_attr = 100;

	PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);
    PDI_expose("pb_size", &N, PDI_OUT);

	int array_attr[4];
	array_attr[0] = 101;
	array_attr[1] = 102;
	array_attr[2] = 103;
	array_attr[3] = 104;
	PDI_expose("array_attr", array_attr, PDI_OUT);

	int scalar_group_attr = 200;
	PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);

	int array_group_attr[4];
	array_group_attr[0] = 201;
	array_group_attr[1] = 202;
	array_group_attr[2] = 203;
	array_group_attr[3] = 204;
	PDI_expose("array_group_attr", array_group_attr, PDI_OUT);

	// write data
	input = 0;
	PDI_expose("input", &input, PDI_OUT);
	PDI_expose("deflate", &deflate, PDI_OUT);
	PDI_expose("int_scalar", &int_scalar, PDI_OUT);
	PDI_expose("int_array", int_array, PDI_OUT);

	// // zero data
	// int_scalar = 0;
	// for (int i = 0; i < N; i++) {
	// 	int_array[i] = 0;
	// }

	// // reset metadata
	// scalar_attr = 0;
	// PDI_expose("scalar_attr", &scalar_attr, PDI_OUT);

	// array_attr[0] = 0;
	// array_attr[1] = 0;
	// array_attr[2] = 0;
	// array_attr[3] = 0;
	// PDI_expose("array_attr", array_attr, PDI_OUT);

	// scalar_group_attr = 0;
	// PDI_expose("scalar_group_attr", &scalar_group_attr, PDI_OUT);

	// array_group_attr[0] = 0;
	// array_group_attr[1] = 0;
	// array_group_attr[2] = 0;
	// array_group_attr[3] = 0;
	// PDI_expose("array_group_attr", array_group_attr, PDI_OUT);
    
    delete[] int_array;
	PDI_finalize();
}
