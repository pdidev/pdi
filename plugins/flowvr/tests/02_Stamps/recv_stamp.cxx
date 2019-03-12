/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <iostream>

#include <cassert>
#include <memory>

#include <pdi.h>

#include "common.h"

int main(int argc, char* argv[])
{	
	PC_tree_t conf = PC_parse_path("recv_stamp.yml");
	PDI_init(conf);
	
	int status = 0;
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	for (int it = 1; it < ITER; it++) {
		{
			std::unique_ptr<int> int_stamp {new_int_stamp()};
			int int_stamp_pdi;
			PDI_expose("int_stamp", &int_stamp_pdi, PDI_IN);
			if (*int_stamp != int_stamp_pdi) {
				std::cerr << "(Int)Expected: " << *int_stamp << " Got: " << int_stamp_pdi << std::endl;
				status = 1;
				break;
			}
		}
		{
			std::unique_ptr<float> float_stamp {new_float_stamp()};
			float float_stamp_pdi;
			PDI_expose("float_stamp", &float_stamp_pdi, PDI_IN);
			if (*float_stamp != float_stamp_pdi) {
				std::cerr << "(Float) Expected: " << *float_stamp << " Got: " << float_stamp_pdi << std::endl;
				status = 1;
				break;
			}
		}
		{
			std::unique_ptr<char[]> string_stamp {new_string_stamp()};
			char string_stamp_pdi[CHAR_ARRAY_SIZE];
			PDI_expose("string_stamp", string_stamp_pdi, PDI_IN);
			if (strcmp(string_stamp.get(), string_stamp_pdi)) {
				std::cerr << "(String) Expected: " << string_stamp.get() << " Got: " << string_stamp_pdi << std::endl;
				status = 1;
				break;
			}
		}
		{
			std::unique_ptr<int[]> int_array_stamp {new_int_array_stamp()};
			int int_array_stamp_pdi[INT_ARRAY_SIZE];
			PDI_expose("int_array_stamp", int_array_stamp_pdi, PDI_IN);
			for (int i = 0; i < INT_ARRAY_SIZE; i++) {
				if (int_array_stamp[i] != int_array_stamp_pdi[i]) {
					std::cerr << "(Int[" << i <<"]) Expected: " << int_array_stamp[i] << " Got: " << int_array_stamp_pdi[i] << std::endl;
					status = 1;
					break;
				}
			}
		}
		{
			std::unique_ptr<float[]> float_array_stamp {new_float_array_stamp()};
			float float_array_stamp_pdi[FLOAT_ARRAY_SIZE];
			PDI_expose("float_array_stamp", float_array_stamp_pdi, PDI_IN);
			for (int i = 0; i < FLOAT_ARRAY_SIZE; i++) {
				if (float_array_stamp[i] != float_array_stamp_pdi[i]) {
					std::cerr << "(Float[" << i <<"]) Expected: " << float_array_stamp[i] << " Got: " << float_array_stamp_pdi[i] << std::endl;
					status = 1;
					break;
				}
			}
		}
		
		PDI_expose("wait", &wait, PDI_IN);
	}

	if (status == 0) {
		//if test passed, exit application, if not wait for timeout
		PDI_event("abort");
	}

	PDI_finalize();
	PC_tree_destroy(&conf);
	
	return status;
}