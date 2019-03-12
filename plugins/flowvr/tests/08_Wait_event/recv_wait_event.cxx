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
	PC_tree_t conf = PC_parse_path("recv_wait_event.yml");
	PDI_init(conf);
	
	int status = 0;
	PDI_event("wait");
	for (int i = 1; i < ITER; i++) {
		int* message_shr;
		PDI_access("message", (void**)&message_shr, PDI_IN);		
		std::unique_ptr<int[]> array_uptr {new_array()};

		for (int j = 0; j < ARRAY_SIZE; j++) {
			if (array_uptr[j] != message_shr[j]) {
				std::cerr << "(Int[" << j <<"]) Expected: " << array_uptr[j] << " Got: " << message_shr[j] << std::endl;
				status = 1;
				break;
			}
		}
		PDI_release("message");

		PDI_event("wait_loop");
	}

	if (status == 0) {
		//if test passed, exit application, if not wait for timeout
		PDI_event("abort");
	}

	PDI_finalize();
	PC_tree_destroy(&conf);

	return 0;
}
