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
\file utils.h
\brief Various tools copying, strcat-like ... 
\author J. Bigot (CEA)
*/

#ifndef PDI_UTILS_H__
#define PDI_UTILS_H__

#include "config.h"

#include <pdi.h>
#include <pdi/datatype.h>

/** Copies some (possibly sparse) data into a new (dense) buffer
 * \param type the type of the source data
 * \param to the destination buffer that should be large enough to hold the data
 * \param from the source buffer
 * \return an error code
 */
PDI_status_t tcopy(const PDI_type_t* type, void* to, void* from);

/** A mallocing sprintf
 * 
 * Behaves similarly to sprintf except it allocates the buffer where it writes
 * using malloc.
 * \param fmt the format string
 * \param ... the printf-style arguments
 * \return the newly allocated buffer
 * \see sprintf
 */
char *msprintf(const char *fmt, ...);

/** A mallocing vsprintf
 * 
 * Behaves similarly to vsprintf except it allocates the buffer where it writes
 * using malloc.
 * \param fmt the format string
 * \param ap the va_list of printf-style arguments
 * \return the newly allocated buffer
 * \see vsprintf
 */
char *vmsprintf(const char *fmt, va_list ap);

/** A mallocing strcat
 * 
 * Behaves somewhat similarly to strcat but
 * - it grows the destination buffer using realloc
 * - both strings have their length specified instead of being NULL-terminated
 * \param dest the destination buffer that will be extended (and NULL-terminated)
 * \param dlen the amount of characters to keep from the destination buffer
 * \param src the buffer to concatenate to dest
 * \param dlen the amount of characters to use from the concatenated buffer
 * \return the destination buffer
 * \see strcat
 */
char *mstrcat(char* dest, size_t dlen, const char* src, size_t slen);

#ifndef STRDUP_WORKS
/** returns a pointer to a new string which is a duplicate of the string s.
 * Memory for the new string is obtained with malloc, and can be freed with free.
 * 
 * This is a re-implementation of POSIX strdup for portability.
 * 
 * \param s the string to duplicate
 * \return a pointer to the duplicated string on success, NULL if insufficient
 * memory was available, with errno set to indicate the cause of the error.
 */
char *strdup( const char *s );
#endif

#endif
