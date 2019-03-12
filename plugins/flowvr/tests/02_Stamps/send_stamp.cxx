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
#include <memory>
#include <unistd.h>

#include <pdi.h>

#include "common.h"

int main(int argc, char* argv[])
{	
	PC_tree_t conf = PC_parse_path("send_stamp.yml");
	PDI_init(conf);
	
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	while (wait) {
		{
			std::unique_ptr<int> int_stamp {new_int_stamp()};
			PDI_expose("int_stamp", int_stamp.get(), PDI_OUT);
		}
		{
			std::unique_ptr<float> float_stamp {new_float_stamp()};
			PDI_expose("float_stamp", float_stamp.get(), PDI_OUT);
		}
		{
			std::unique_ptr<char[]> string_stamp {new_string_stamp()};
			PDI_expose("string_stamp", string_stamp.get(), PDI_OUT);
		}
		{
			std::unique_ptr<int[]> int_array_stamp {new_int_array_stamp()};
			PDI_expose("int_array_stamp", int_array_stamp.get(), PDI_OUT);
		}
		{
			std::unique_ptr<float[]> float_array_stamp {new_float_array_stamp()};
			PDI_expose("float_array_stamp", float_array_stamp.get(), PDI_OUT);
		}

		usleep(100 * 1000); // 1000 * 1000 is 1 second
		PDI_expose("wait", &wait, PDI_IN);
	}
	
	PDI_finalize();
	PC_tree_destroy(&conf);
	return 0;
}
