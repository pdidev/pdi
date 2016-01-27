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

#include "error.h"

static PDI_errhandler_t handler = { PDI_assert, NULL };

static char *buffer = NULL;

static size_t buffer_size = 0;

// TODO: make this thread-safe by using a distinct buffer / thread
PDI_errhandler_t PDI_errhandler(PDI_errhandler_t new_handler)
{
	PDI_errhandler_t old_handler = handler;
	handler = new_handler;
	return old_handler;
}

PDI_status_t error(PDI_status_t status, const char *message, ...)
{
	va_list ap;
	va_start(ap, message);
	int realsize = vsnprintf(buffer, buffer_size, message, ap);
	va_end(ap);
	if ( realsize >= buffer_size ) {
		buffer_size = realsize+1;
		buffer = realloc(buffer, buffer_size);
		va_start(ap, message);
		vsnprintf(buffer, buffer_size, message, ap);
		va_end(ap);
	}
	if ( handler.func ) handler.func(status, buffer, handler.context);
	return status;
}

void PDI_assert(PDI_status_t status, const char* message, void* context)
{
	if ( status ) {
		fprintf(stderr, "Error in PDI: %s\n", message);
		abort();
	}
}

