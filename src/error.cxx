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

#include "config.h"

#include <cassert>
#include <memory>
#include <string>

#include <pthread.h>

#include "pdi/error.h"

namespace PDI {

Error::Error(PDI_status_t errcode, const char* message, va_list ap):
	m_status(errcode)
{
	va_list ap2; va_copy(ap2, ap);
	// resize to contain the string + terminating null byte
	m_what.resize(vsnprintf(NULL, 0, message, ap)+1);
	vsnprintf(&m_what[0], m_what.size(), message, ap2);
	// remove the terminating null byte
	m_what.resize(m_what.size() - 1);
}

Error::Error(PDI_status_t errcode, const char* message, ...):
	m_status(errcode)
{
	va_list ap;
	va_start(ap, message);
	// resize to contain the string + terminating null byte
	m_what.resize(vsnprintf(NULL, 0, message, ap)+1);
	va_end(ap);
	va_start(ap, message);
	vsnprintf(&m_what[0], m_what.size(), message, ap);
	// remove the terminating null byte
	m_what.resize(m_what.size() - 1);
	va_end(ap);
}

const char* Error::what() const noexcept
{
	return m_what.c_str();
}

PDI_status_t Error::status() const noexcept
{
	return m_status;
}

} // namespace PDI
