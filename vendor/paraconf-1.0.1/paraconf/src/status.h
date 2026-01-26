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

#ifndef STATUS_H__
#define STATUS_H__

#include "paraconf.h"

#define PC_handle_err(callstatus, free_stamp)\
do { \
	status = callstatus; \
	if ( status ) goto free_stamp; \
} while( 0 )

#define PC_handle_tree(free_stamp)\
do { \
	if ( PC_status(restree) ) goto free_stamp; \
} while( 0 )

#define PC_handle_err_tree(callstatus, free_stamp)\
do { \
	restree.status = callstatus; \
	if ( PC_status(restree) ) goto free_stamp; \
} while( 0 )

#define PC_handle_tree_err(calltree, free_stamp)\
do { \
	status = calltree.status; \
	if ( status ) goto free_stamp; \
} while( 0 )

PC_status_t PC_make_err(PC_status_t status, const char* message, ...);

#endif // STATUS_H__
