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

/**
 * \file status.c
 * \brief Manage error and context
 * \author J. Bigot (CEA)
 */

#include <memory>

#include <pthread.h>

#include <paraconf.h>

#include "status.h"

namespace PDI {

using std::unique_ptr;

Error::Error(PDI_status_t errcode, const char* message, va_list ap):
	m_status(errcode)
{
	va_list ap2; va_copy(ap2, ap);
	m_what.resize(vsnprintf(NULL, 0, message, ap));
	vsnprintf(&m_what[0], m_what.size(), message, ap2);
}

Error::Error(PDI_status_t errcode, const char* message, ...):
	m_status(errcode)
{
	va_list ap;
	va_start(ap, message);
	m_what.resize(vsnprintf(NULL, 0, message, ap));
	va_end(ap);
	va_start(ap, message);
	vsnprintf(&m_what[0], m_what.size(), message, ap);
	va_end(ap);
}

}

// File private stuff

typedef struct errctx_s {

	PDI_errhandler_t handler;
	
	char *buffer;
	
	long buffer_size;
	
} errctx_t;

static pthread_key_t context_key;

static pthread_once_t context_key_once = PTHREAD_ONCE_INIT;

/** Handler for fatal errors
  */
static void assert_status(PDI_status_t status, const char *message, void *)
{
	if (status) {
		fprintf(stderr, "FATAL ERROR, in PDI: %s\n", message);
		abort();
	}
}

/** Handler for warning
  */
static void warn_status(PDI_status_t status, const char *message, void *)
{
	if (status) {
		fprintf(stderr, "Warning, in PDI: %s\n", message);
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
	
	errctx_t *context = (errctx_t *) pthread_getspecific(context_key);
	if (!context) {
		context = (errctx_t *) malloc(sizeof(errctx_t));
		context->buffer = NULL;
		context->buffer_size = 0;
		context->handler = PDI_ASSERT_HANDLER;
		pthread_setspecific(context_key, context);
	}
	
	return context;
}

static void forward_PC_error(PC_status_t, const char *message, void *)
{
	if (get_context()->handler.func) get_context()->handler.func(PDI_ERR_CONFIG, message, get_context()->handler.context);
}

// library private stuff

PDI_status_t PDI_make_err(PDI_status_t status, const char *message, ...)
{
	va_list ap;
	va_start(ap, message);
	throw PDI::Error(status, message, ap);
	va_end(ap);
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

const PDI_errhandler_t PDI_WARN_HANDLER = {
	&warn_status,
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

char *PDI_errmsg()
{
	return get_context()->buffer;
}
