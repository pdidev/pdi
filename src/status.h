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
 * \file status.h
 * \brief Macros used to handle errors
 * \author J. Bigot (CEA)
 */

#ifndef ERROR_H__
#define ERROR_H__

#include <exception>
#include <string>

#include "pdi.h"

namespace PDI
{

class Error:
	public std::exception
{
public:
	PDI_status_t m_status;
	
	std::string m_what;
	
	Error(PDI_status_t errcode = PDI_OK, const char *message = "", ...);
	
	Error(PDI_status_t errcode, const char *message, va_list args);
	
	const char *what() const noexcept override
	{
		return m_what.c_str();
	}
	
	explicit operator bool ()
	{
		return m_status != PDI_OK && m_status != PDI_UNAVAILABLE;
	}
};

} // namespace PDI

/** Handle a PDI return code, jumping to the error handler code on error
 * \param callstatus the call status to handle, will be used only once, this
 *        can be the call itself
 * \param free_stamp the label of the error handling code
 */
#define PDI_handle_err(callstatus, free_stamp)\
	do { \
		PDI_status_t newstatus = (callstatus); \
		if ( newstatus ) status = newstatus; \
		if ( status && status != PDI_UNAVAILABLE ) goto free_stamp; \
	} while( 0 )


/** Handle a Paraconf return code, jumping to the error handler code on error
 * \param callstatus the call status to handle, will be used only once, this
 *        can be the call itself
 * \param free_stamp the label of the error handling code
 */
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
PDI_status_t PDI_make_err(PDI_status_t errcode, const char *message, ...);

/** install a paraconf error-handler that forwards errors to PDI
 *
 * Used in the handle_PC_err macro
 *
 * \return the previously installed paraconf error-handler
 */
PC_errhandler_t intercept_PC_errors();

#endif // ERROR_H__
