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

#ifndef MOCK_PDI_H_
#define MOCK_PDI_H_

#include <stddef.h>

#ifndef WITHOUT_PARACONF
#include <paraconf.h>
#endif

#ifdef __cplusplus
extern "C" {
#endif

typedef enum PDI_status_e {
	PDI_OK = 0,
	PDI_UNAVAILABLE,
	PDI_ERR_CONFIG,
	PDI_ERR_VALUE,
	PDI_ERR_PLUGIN,
	PDI_ERR_IMPL,
	PDI_ERR_SYSTEM,
	PDI_ERR_STATE,
	PDI_ERR_RIGHT,
	PDI_ERR_TYPE
} PDI_status_t;

typedef void (*PDI_errfunc_f)(PDI_status_t status, const char* message, void* context);

typedef struct PDI_errhandler_s {
	PDI_errfunc_f func;

	void* context;

} PDI_errhandler_t;

static const PDI_errhandler_t PDI_NULL_HANDLER = {NULL, NULL};

static const PDI_errhandler_t PDI_ASSERT_HANDLER = {NULL, NULL};

static const PDI_errhandler_t PDI_WARN_HANDLER = {NULL, NULL};

static inline const char* PDI_errmsg(void)
{
	return "";
}

static inline PDI_errhandler_t PDI_errhandler(PDI_errhandler_t handler)
{
	return PDI_NULL_HANDLER;
}

#ifndef WITHOUT_PARACONF
static inline PDI_status_t PDI_init(PC_tree_t conf)
{
	return PDI_OK;
}
#endif

static inline PDI_status_t PDI_finalize(void)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_version(unsigned long* provided, unsigned long expected)
{
	return PDI_OK;
}

typedef enum PDI_inout_e {
	PDI_NONE = 0,
	PDI_IN = 1,
	PDI_OUT = 2,
	PDI_INOUT = 3
} PDI_inout_t;

static inline PDI_status_t PDI_share(const char* name, const void* data, PDI_inout_t access)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_access(const char* name, void** buffer, PDI_inout_t inout)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_release(const char* name)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_reclaim(const char* name)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_event(const char* event)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_expose(const char* name, const void* data, PDI_inout_t access)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_multi_expose(const char* event_name, const char* name, const void* data, PDI_inout_t access, ...)
{
	return PDI_OK;
}

#ifdef PDI_WITH_DEPRECATED

static inline PDI_status_t PDI_transaction_begin(const char* name)
{
	return PDI_OK;
}

static inline PDI_status_t PDI_transaction_end(void)
{
	return PDI_OK;
}

#endif // PDI_WITH_DEPRECATED

#ifdef __cplusplus
} // extern C
#endif

#endif // MOCK_PDI_H_
