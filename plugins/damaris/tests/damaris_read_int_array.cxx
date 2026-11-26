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

#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

#define IMX 10

constexpr char CONFIG_FOR_READING_RESULT[] = R"(
logging: trace
metadata:
  nn: int
  nbcalls: int
data:
  pdi_values: {size: ['$nn'], type: array, subtype: int}
  damaris_values: {size: ['$nn'], type: array, subtype: int}
plugins:
  trace: ~
  decl_hdf5:
    - file: './data_iter${nbcalls}.h5'
      read:
        pdi_values:
          dataset: int_values
        nn:
    - file: './HDF5_files/damaris_scalar_type_It${nbcalls}.h5'
      read:
        damaris_values:
          dataset: int_values
)";

int main(int argc, char* argv[])
{
	// comparison of the results

	// reinitialize pdi for reading results
	PDI_init(PC_parse_string(CONFIG_FOR_READING_RESULT));

	int nn_first_call = IMX / 2;
	int size_pdi = -20;
	int damaris_values[IMX];
	int pdi_values[IMX];
	int size_expected[2] = {nn_first_call, IMX};

	printf("I read the value obtained by damaris and pdi.\n");

	for (int nb_calls = 0; nb_calls < 2; ++nb_calls) {
		PDI_expose("nbcalls", &nb_calls, PDI_INOUT);
		PDI_expose("nn", &size_pdi, PDI_INOUT);
		if (size_pdi != size_expected[nb_calls]) {
			printf("For iteration=%d,  error in reading the size of the array, size_pdi=%d\n", nb_calls, size_pdi);
			exit(EXIT_FAILURE);
		}

		for (int ii = 0; ii < size_expected[nb_calls]; ++ii) {
			damaris_values[ii] = 6;
			pdi_values[ii] = 9;
		}

		PDI_multi_expose("read_pdi", "pdi_values", pdi_values, PDI_IN, NULL);
		PDI_multi_expose("read_damaris", "damaris_values", damaris_values, PDI_IN, NULL);

		for (int ii = 0; ii < size_expected[nb_calls]; ++ii) {
			if (pdi_values[ii] != damaris_values[ii]) {
				printf("For iteration=%d, values pdi %d != %d  damaris\n", nb_calls, pdi_values[ii], damaris_values[ii]);
				exit(EXIT_FAILURE);
			}
		}
	}

	PDI_finalize();

	return EXIT_SUCCESS;
}
