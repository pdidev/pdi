/*******************************************************************************
 * Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

void share()
{
	int to_share[2] = {1, 1};
	assert(PDI_OK == PDI_share("to_share", to_share, PDI_OUT));
}

void access() //access
{
	int to_access[2] = {1, 1};
	assert(PDI_OK == PDI_access("to_access", (void**)&to_access, PDI_IN));
}

void release() //access/release
{
	int to_release[2] = {1, 1};
	PDI_access("to_access", (void**)&to_release, PDI_IN);
	assert(PDI_OK == PDI_release("to_release"));
}

void reclaim() //share/reclaim
{
	int to_reclaim[2] = {1, 1};
	PDI_share("to_release", to_reclaim, PDI_OUT);
	assert(PDI_OK == PDI_reclaim("to_reclaim"));
}

void event()
{
	assert(PDI_OK == PDI_event("event_one"));
}

void expose()
{
	int to_expose[2] = {1, 1};
	assert(PDI_OK == PDI_expose("to_expose", to_expose, PDI_OUT));
}

void multi_expose()
{
	int to_multi_expose[2] = {1, 1};
	int to_multi_expose_two[2] = {2, 2};
	assert(
		PDI_OK
		== PDI_multi_expose("event_two", "to_multi_expose", &to_multi_expose, PDI_OUT, "to_multi_expose_two", to_multi_expose_two, PDI_OUT, NULL)
	);
}

void errhandler()
{
	PDI_errhandler_t current_handler = PDI_errhandler(PDI_NULL_HANDLER);
	assert(current_handler.func == NULL && current_handler.context == NULL);
}

int tests(int argc, char* argv[])
{
	assert(PDI_OK == PDI_init(PC_parse_path(argv[1])));

	assert(PDI_errmsg());
	errhandler();

	share();
	access();
	release();
	reclaim();
	event();
	expose();
	multi_expose();

	assert(PDI_OK == PDI_finalize());

	return 0;
}
