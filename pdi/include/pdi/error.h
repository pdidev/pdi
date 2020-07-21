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

#ifndef PDI_ERROR_H_
#define PDI_ERROR_H_

#include <exception>
#include <string>

#include <paraconf.h>

#include <pdi/pdi_fwd.h>

#include <spdlog/fmt/fmt.h>

namespace PDI {

class PDI_EXPORT Error:
	public std::exception
{
	std::string m_what;
	
	PDI_status_t m_status;
	
public:
	/** Creates a PDI error
	 * \param[in] errcode the error code of the error to create
	 * \param[in] fmt an errror message as a python-style format
	 * \param[in] args the python-style parameters for the message
	 * \see printf
	 */
	template<typename... Args>
	Error(PDI_status_t errcode, const char* fmt, const Args& ... args):
		m_what{fmt::format(fmt, args...)},
		m_status{errcode}
	{}
	
	Error(PDI_status_t errcode, const char* fmt);
	
	const char* what() const noexcept override;
	
	PDI_status_t status() const noexcept;
	
};

struct PDI_EXPORT Unavailable_error : public Error {
	template<typename... Args>
	Unavailable_error(const char* fmt, const Args& ... args):
		Error(PDI_UNAVAILABLE, ("Unavailable_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Config_error : public Error {
	template<typename... Args>
	Config_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_CONFIG, ("Config_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Value_error : public Error {
	template<typename... Args>
	Value_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_VALUE, ("Value_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Plugin_error : public Error {
	template<typename... Args>
	Plugin_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_PLUGIN, ("Plugin_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Impl_error : public Error {
	template<typename... Args>
	Impl_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_IMPL, ("Impl_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT System_error : public Error {
	template<typename... Args>
	System_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_SYSTEM, ("System_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT State_error : public Error {
	template<typename... Args>
	State_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_STATE, ("State_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Right_error : public Error {
	template<typename... Args>
	Right_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_RIGHT, ("Right_error: " + std::string(fmt)).c_str(), args...)
	{}
};

struct PDI_EXPORT Type_error : public Error {
	template<typename... Args>
	Type_error(const char* fmt, const Args& ... args):
		Error(PDI_ERR_TYPE, ("Type_error: " + std::string(fmt)).c_str(), args...)
	{}
};

} // namespace PDI

#endif // PDI_ERROR_H_
