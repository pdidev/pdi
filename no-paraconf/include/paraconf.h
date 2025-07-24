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

/** \file paraconf.h for deactivation
 */

#ifndef PARACONF_H_
#define PARACONF_H__H_

#ifdef __cplusplus
extern "C" {
#endif

/** \addtogroup error
 * \{
 */

typedef struct PC_tree_s {
} PC_tree_t;
struct _IO_FILE;
typedef struct _IO_FILE FILE;

static inline PC_tree_t PC_parse_string(const char* document)
{
	PC_tree_t mock = {};
	return mock;
}

static inline PC_tree_t PC_parse_file(FILE *file)
{
	PC_tree_t mock = {};
	return mock;
}

#ifdef __cplusplus
} // extern C
#endif

#endif // PARACONF_H_
