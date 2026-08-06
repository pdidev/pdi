/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Institut National de Recherche en Sciences et Technologies du Numérique (INRIA)
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

#include <mpi.h>
#include <assert.h>
#include <stdio.h>
#include <unistd.h>
#include <pdi.h>

#include <unistd.h>

#define IMX 5

constexpr char CONFIG_FOR_READING_RESULT[] = R"(
logging: trace
metadata:
  nn: int
data:
  damaris_values: {size: ['$nn'], type: array, subtype: int}
plugins:
  decl_hdf5:
    - file: './HDF5_files/distinguish_data_and_dataset_It0.h5'
      on_event: 'read_size'
      read:
        nn:
          size_of: written_values_ds
)";

int main(int argc, char* argv[])
{
	PDI_init(PC_parse_string(CONFIG_FOR_READING_RESULT));

	int damaris_size = 0;
	PDI_multi_expose("read_size", "nn", &damaris_size, PDI_INOUT, NULL);

	if (IMX != damaris_size) {
		printf("Error: IMX (= %d) != damaris_size(= %d) \n", IMX, damaris_size);
		exit(EXIT_FAILURE);
	}

	// PDI_multi_expose("read", "damaris_values", damaris_values, PDI_INOUT, NULL);

	// for (int ii = 0; ii < size; ++ii) {
	//   if (written_values[ii] != damaris_values[ii]) {
	//     printf("written_values[%d] (= %d) != damaris_values[%d] (= %d) \n", ii, written_values[ii], ii, damaris_values[ii]);
	//     exit(EXIT_FAILURE);
	//   }
	// }

	PDI_finalize();

	return EXIT_SUCCESS;
}
