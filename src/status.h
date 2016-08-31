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

#ifndef ERROR_H__
#define ERROR_H__

#include "pdi.h"

#define handle_err(callstatus, free_stamp)\
do { \
	status = callstatus; \
	if ( status ) goto free_stamp; \
} while( 0 )

#define handle_PC_err(callstatus, free_stamp)\
do { \
	PC_errhandler_t pc_handler = intercept_PC_errors();\
	if ( callstatus ) status = PDI_ERR_CONFIG; \
	PC_errhandler(pc_handler);\
	if ( status ) goto free_stamp; \
} while( 0 )

/** Create a new PDI error and calls the user specified handler to handle it
 * \param[in] errcode the error code of the error to create
 * \param[in] message an errror message as a printf-style format
 * \param[in] ... the printf-style parameters for the message
 * \see printf
 * \return the newly created error
 */
PDI_status_t handle_error(PDI_status_t errcode, const char *message, ...);

/** install a paraconf error-handler that forwards errors to PDI
 * 
 * Used in the handle_PC_err macro
 * 
 * \return the previously installed paraconf error-handler
 */
PC_errhandler_t intercept_PC_errors();

#endif // ERROR_H__
