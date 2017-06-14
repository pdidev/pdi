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
\file utils.c
\brief Various tools copying, strcat-like ...
\author J. Bigot (CEA)
**/

#include <string.h>
#include <stdlib.h>
#include <stdint.h>

#include "pdi.h"
#include "pdi/datatype.h"
#include "pdi/value.h"

#include "status.h"
#include "utils.h"

#define PRINTF_BUFFER_SIZE 256

char *vmsprintf(const char *fmt, va_list ap)
{
	int index_size = PRINTF_BUFFER_SIZE;
	char *index = malloc(index_size);
	while (vsnprintf(index, index_size, fmt, ap) > index_size) {
		index_size *= 2;
		index = realloc(index, index_size);
	}
	return index;
}

char *msprintf(const char *fmt, ...)
{
	va_list ap;
	va_start(ap, fmt);
	char *res = vmsprintf(fmt, ap);
	va_end(ap);
	return res;
}

char *mstrcat(char *dest, size_t dlen, const char *src, size_t slen)
{
	char *result = realloc(dest, dlen + slen + 1);
	memcpy(result + dlen, src, slen);
	result[dlen + slen] = 0;
	return result;
}

#ifndef STRDUP_WORKS
char *strdup(const char *s)
{
	char *p = malloc(strlen(s) + 1);
	if (p) strcpy(p, s);
	return p;
}
#endif
