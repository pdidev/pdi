/*******************************************************************************
 * Copyright (C) 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#ifdef __cplusplus
#include <cstdio>
#include <cstdlib>
#include <cstring>
#else
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#endif
#ifndef WITHOUT_PARACONF
#include <paraconf.h>
#endif

void share() //share
{
	int to_share[2] = {1, 1};
	if (PDI_OK != PDI_share("to_share", to_share, PDI_OUT)) {
		fprintf(stderr, "*** Error in PDI_share\n");
		exit(EXIT_FAILURE);
	}
}

void access() //share/access
{
	int to_access[2] = {1, 1};
	PDI_share("to_access", to_access, PDI_OUT);
	if (PDI_OK != PDI_access("to_access", (void**)&to_access, PDI_IN)) {
		fprintf(stderr, "*** Error in PDI_access\n");
		exit(EXIT_FAILURE);
	}
}

void release() //share/release
{
	int to_release[2] = {1, 1};
	PDI_share("to_release", to_release, PDI_OUT);
	PDI_access("to_release", (void**)&to_release, PDI_IN);
	if (PDI_OK != PDI_release("to_release")) {
		fprintf(stderr, "*** Error in PDI_release\n");
		exit(EXIT_FAILURE);
	}
}

void reclaim() //share/reclaim
{
	int to_reclaim[2] = {1, 1};
	PDI_share("to_reclaim", to_reclaim, PDI_OUT);
	if (PDI_OK != PDI_reclaim("to_reclaim")) {
		fprintf(stderr, "*** Error in PDI_reclaim\n");
		exit(EXIT_FAILURE);
	}
}

void event()
{
	if (PDI_OK != PDI_event("event_one")) {
		fprintf(stderr, "*** Error in PDI_event\n");
		exit(EXIT_FAILURE);
	}
}

void expose()
{
	int to_expose[2] = {1, 1};
	if (PDI_OK != PDI_expose("to_expose", to_expose, PDI_OUT)) {
		fprintf(stderr, "*** Error in PDI_expose\n");
		exit(EXIT_FAILURE);
	}
}

void multi_expose()
{
	int to_multi_expose[2] = {1, 1};
	int to_multi_expose_two[2] = {2, 2};
	if (PDI_OK
	    != PDI_multi_expose("event_two", "to_multi_expose", &to_multi_expose, PDI_OUT, "to_multi_expose_two", to_multi_expose_two, PDI_OUT, NULL))
	{
		fprintf(stderr, "*** Error in PDI_multi_expose\n");
		exit(EXIT_FAILURE);
	}
}

void errhandler()
{
	PDI_errhandler_t current_handler = PDI_errhandler(PDI_NULL_HANDLER);
	if (NULL != current_handler.context) {
		fprintf(stderr, "*** Error in PDI_errhandler\n");
		exit(EXIT_FAILURE);
	}
}

int tests(int argc, char* argv[])
{
	static const char* CONFIG_YAML
		= "logging: trace														\n"
		  "data:																\n"
		  "  to_share: {type: array, subtype: int, size: 2}						\n"
		  "  to_access: {type: array, subtype: int, size: 2}					\n"
		  "  to_release: {type: array, subtype: int, size: 2}					\n"
		  "  to_reclaim: {type: array, subtype: int, size: 2}					\n"
		  "  to_expose: {type: array, subtype: int, size: 2}					\n"
		  "  to_multi_expose: {type: array, subtype: int, size: 2}				\n"
		  "  to_multi_expose_two: {type: array, subtype: int, size: 2}			\n";

#ifndef WITHOUT_PARACONF
	if (PDI_OK != PDI_init(PC_parse_string(CONFIG_YAML))) {
		fprintf(stderr, "*** Error in PDI_initialisation\n");
		exit(EXIT_FAILURE);
	}
#endif // WITHOUT_PARACONF

	const char* errmsg = PDI_errmsg();
	if (strcmp(errmsg, "") != 0) {
		fprintf(stderr, "*** Error in PDI_errmsg\n");
		exit(EXIT_FAILURE);
	}

	errhandler();

	share();
	access();
	release();
	reclaim();
	event();
	expose();
	multi_expose();

	if (PDI_OK != PDI_finalize()) {
		fprintf(stderr, "*** Error in PDI_finalisation\n");
		exit(EXIT_FAILURE);
	}

	return EXIT_SUCCESS;
}
