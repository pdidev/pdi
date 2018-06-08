/*******************************************************************************
 * Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef PDI_ERROR_H_
#define PDI_ERROR_H_

#include <exception>
#include <string>

#include <paraconf.h>

#include <pdi/logger.h>
#include <pdi/pdi_fwd.h>

namespace PDI {

class PDI_EXPORT Error:
	public std::exception
{
	std::string m_what;
	
	PDI_status_t m_status;

	/// Global logger of PDI
	Logger logger {spdlog::get("logger")};
	
public:
	/** Creates a PDI error
	 * \param[in] errcode the error code of the error to create
	 * \param[in] message an errror message as a printf-style format
	 * \param[in] ... the printf-style parameters for the message
	 * \see printf
	 */
	Error(PDI_status_t errcode = PDI_OK, const char* message = "", ...);
	
	Error(PDI_status_t errcode, const char* message, va_list args);
	
	const char* what() const noexcept override;
	
	PDI_status_t status() const noexcept;
	
};

} // namespace PDI

#endif // PDI_ERROR_H_
