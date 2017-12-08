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

#include <paraconf.h>

#include "pdi.h"

namespace PDI
{

class Error:
	public std::exception
{
public:
	PDI_status_t m_status;
	
	std::string m_what;
	
	/** Creates a PDI error
	 * \param[in] errcode the error code of the error to create
	 * \param[in] message an errror message as a printf-style format
	 * \param[in] ... the printf-style parameters for the message
	 * \see printf
	 */
	Error(PDI_status_t errcode = PDI_OK, const char *message = "", ...);
	
	Error(PDI_status_t errcode, const char *message, va_list args);
	
	const char *what() const noexcept override
	{
		return m_what.c_str();
	}
	
};

/** Automatically installs a paraconf error-handler that forwards errors to
 *  PDI on construction and uninstalls it on destruction.
 */
class Paraconf_raii_forwarder
{
	PC_errhandler_t m_handler;
	
public:
	Paraconf_raii_forwarder();
	
	~Paraconf_raii_forwarder();
	
};

PDI_status_t return_err(const Error &err);

} // namespace PDI

#endif // ERROR_H__
