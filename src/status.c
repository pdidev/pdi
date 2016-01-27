/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#include <paraconf.h>

#include "status.h"

// File private stuff

typedef struct errctx_s {
	
	PDI_errhandler_t handler;
	
	char *buffer;
	
	long buffer_size;
	
} errctx_t;

static void assert_status(PDI_status_t status, const char* message, void* context)
{
	if ( status ) {
		fprintf(stderr, "Error in PDI: %s\n", message);
		abort();
	}
}

// TODO: make this thread-safe by using a distinct buffer / thread
static errctx_t *get_context()
{
	static errctx_t result = {
		{ assert_status, NULL },
		NULL,
		0
	};
	
	return &result;
}

static void forward_PC_error(PC_status_t status, const char *message, void *context)
{
	get_context()->handler.func(PDI_ERR_CONFIG, message, get_context()->handler.context);
}

// library private stuff

PDI_status_t handle_error(PDI_status_t status, const char *message, ...)
{
	va_list ap;
	va_start(ap, message);
	int realsize = vsnprintf(get_context()->buffer, get_context()->buffer_size, message, ap);
	va_end(ap);
	if ( realsize >= get_context()->buffer_size ) {
		get_context()->buffer_size = realsize+1;
		get_context()->buffer = realloc(get_context()->buffer, get_context()->buffer_size);
		va_start(ap, message);
		vsnprintf(get_context()->buffer, get_context()->buffer_size, message, ap);
		va_end(ap);
	}
	if ( get_context()->handler.func ) get_context()->handler.func(status, get_context()->buffer, get_context()->handler.context);
	return status;
}

PC_errhandler_t intercept_PC_errors()
{
	PC_errhandler_t forwarder = { forward_PC_error, NULL };
	return PC_errhandler(forwarder);
}

// public stuff

const PDI_errhandler_t PDI_ASSERT_HANDLER = {
	&assert_status,
	NULL
};

const PDI_errhandler_t PDI_NULL_HANDLER = {
	NULL,
	NULL
};

PDI_errhandler_t PDI_errhandler(PDI_errhandler_t new_handler)
{
	PDI_errhandler_t old_handler = get_context()->handler;
	get_context()->handler = new_handler;
	return old_handler;
}
