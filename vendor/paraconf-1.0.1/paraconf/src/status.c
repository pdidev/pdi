/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>

#include "paraconf.h"

#include "status.h"

// file private stuff

typedef struct errctx_s {
	
	PC_errhandler_t handler;
	
	char *buffer;
	
} errctx_t;

static pthread_key_t context_key;

static pthread_once_t context_key_once = PTHREAD_ONCE_INIT;

static void assert_status(PC_status_t status, const char *message, void *context)
{
	(void) context; // prevent unused warning
	if ( status ) {
		fprintf(stderr, "Error in paraconf: %s\n", message);
		abort();
	}
}

/**
 * \param context taken as a void* but in fact a errctx_t*
 */
static void context_destroy(void *context)
{
	free(context);
}

static void context_init()
{
	pthread_key_create(&context_key, context_destroy);
}

static errctx_t *get_context()
{
	pthread_once(&context_key_once, &context_init);
	
	errctx_t *context = pthread_getspecific(context_key);
	if ( !context ) {
		context = malloc(sizeof(errctx_t));
		context->buffer = NULL;
		context->handler = PC_ASSERT_HANDLER;
		pthread_setspecific(context_key, context);
	}
	
	return context;
}

// library private stuff

PC_status_t PC_make_err(PC_status_t status, const char *message, ...)
{
	va_list ap;
	
	errctx_t *ctx = get_context();
	
	char* oldbuf = ctx->buffer; // might be used as one of the va_args
	va_start(ap, message);
	int buffer_size = vsnprintf(NULL, 0, message, ap)+1;
	va_end(ap);
	ctx->buffer = malloc(buffer_size);
	va_start(ap, message);
	vsnprintf(ctx->buffer, buffer_size, message, ap);
	va_end(ap);
	free(oldbuf);
	if ( ctx->handler.func ) ctx->handler.func(status, ctx->buffer, ctx->handler.context);
	return status;
}

// public stuff

const PC_errhandler_t PC_ASSERT_HANDLER = { assert_status, NULL };

const PC_errhandler_t PC_NULL_HANDLER = { NULL, NULL };

PC_errhandler_t PC_errhandler(PC_errhandler_t new_handler)
{
	PC_errhandler_t old_handler = get_context()->handler;
	get_context()->handler = new_handler;
	return old_handler;
}

char *PC_errmsg()
{
	return get_context()->buffer;
}
