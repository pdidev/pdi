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

#include <assert.h>
#include <math.h>
#include <unistd.h>
#include <pdi.h>

#define FILE "d2s_test.h5"

int main()
{
	printf("PDI d2s_precision_read_test started\n");
	const char* CONFIG_YAML
		= "logging: trace                                                   \n"
		  "data:                                                            \n"
		  "  double_data: { size: [100, 10], type: array, subtype: double }   \n"
		  "plugins:                                                         \n"
		  "  decl_hdf5:                                                     \n"
		  "    - file: d2s_test.h5                                          \n"
		  "      datasets:                                                  \n"
		  "        float_data: {size: [100, 10], type: array, subtype: float }\n"
		  "      read:                                                     \n"
		  "        double_data:                                              \n"
		  "          dataset: float_data                                    \n";

	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(conf);
	double test_array[1000];
	for (int i = 0; i < 1000; i++) {
		test_array[i] = 3.14;
	}
	PDI_expose("double_data", test_array, PDI_IN);

	for (int i = 0; i < 1000; i++) {
		if (fabs(test_array[i] - (i * 10.0 + 3.141592653589793)) > 1.e-4) {
			fprintf(
				stderr,
				"test_array[%d] %f != %f, diff = %f\n ",
				i,
				test_array[i],
				i * 10.0 + 3.141592653589793,
				fabs(test_array[i] - (i * 10.0 + 3.141592653589793))
			);
			return 1;
		}
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	printf("PDI d2s_precision_read_test finalized\n");
	return 0;
}
