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

#include <cassert>
#include <memory>
#include <string>

#include <pthread.h>

#include "pdi/status.h"

// File private stuff

namespace {

using std::string;

struct Error_context
{
	PDI_errhandler_t handler;
	
	string errmsg;
	
};

/** Handler for fatal errors
  */
void assert_status(PDI_status_t status, const char *message, void *)
{
	if (status) {
		fprintf(stderr, "FATAL ERROR, in PDI: %s\n", message);
		abort();
	}
}

/** Handler for warning
  */
void warn_status(PDI_status_t status, const char *message, void *)
{
	if (status) {
		fprintf(stderr, "Warning, in PDI: %s\n", message);
	}
}

pthread_key_t context_key;

void context_destroy(void *context)
{
	delete static_cast<Error_context*>(context);
}

void context_init()
{
	pthread_key_create(&context_key, context_destroy);
}

Error_context *get_context()
{
	static pthread_once_t context_key_once = PTHREAD_ONCE_INIT;
	pthread_once(&context_key_once, context_init);
	Error_context *context = static_cast<Error_context *>(pthread_getspecific(context_key));
	if (!context) {
		context = new Error_context{PDI_ASSERT_HANDLER, ""};
		pthread_setspecific(context_key, context);
	}
	assert(context);
	return context;
}

void forward_PC_error(PC_status_t, const char *message, void *)
{
	throw PDI::Error{PDI_ERR_CONFIG, message};
}

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

const char *PDI_errmsg()
{
	return get_context()->errmsg.c_str();
}

namespace PDI
{

using std::unique_ptr;

Error::Error(PDI_status_t errcode, const char *message, va_list ap):
	m_status(errcode)
{
	va_list ap2; va_copy(ap2, ap);
	m_what.resize(vsnprintf(NULL, 0, message, ap));
	vsnprintf(&m_what[0], m_what.size(), message, ap2);
}

Error::Error(PDI_status_t errcode, const char *message, ...):
	m_status(errcode)
{
	va_list ap;
	va_start(ap, message);
	// get the string size and allocate enough space for it plus a terminating null byte
	m_what.resize(vsnprintf(NULL, 0, message, ap)+1);
	va_end(ap);
	va_start(ap, message);
	vsnprintf(&m_what[0], m_what.size(), message, ap);
	// remove the terminating null byte
	m_what.resize(m_what.size()-1);
	va_end(ap);
}

PDI_status_t return_err(const Error& err)
{
	Error_context* ctx = get_context();
	ctx->errmsg = err.what();
	if (ctx->handler.func) ctx->handler.func(err.m_status, err.what(), ctx->handler.context);
	return err.m_status;
}

Paraconf_raii_forwarder::Paraconf_raii_forwarder():
		m_handler{PC_errhandler(PC_errhandler_t{ forward_PC_error, NULL })}
{
}

Paraconf_raii_forwarder::~Paraconf_raii_forwarder()
{
	PC_errhandler(m_handler);
}

}
