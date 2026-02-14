/*******************************************************************************
 * Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
#include <sstream>
#include <string>

#include <paraconf.h>

#include <spdlog/spdlog.h>

#include <pdi/pdi_fwd.h>

namespace PDI {

class PDI_EXPORT Error: public std::exception
{
protected:
	/// status of the error
	PDI_status_t m_status;

	/// message of the error
	std::string m_what;

public:
	/** Creates a PDI error without a message
	 * \param[in] errcode the error code of the error to create
	 */
	Error(PDI_status_t errcode);

	/** Creates a PDI error
	 * \param[in] errcode the error code of the error to create
	 * \param[in] format_str an errror message as a python-style format
	 * \param[in] args the python-style parameters for the message
	 * \see printf
	 */
	template <typename S, typename... Args>
	Error(PDI_status_t errcode, const S& format_str, Args&&... args)
		: m_status{errcode}
		, m_what{fmt::format(format_str, std::forward<Args>(args)...)}
	{}

	/** Creates a PDI error
	 * \param[in] errcode the error code of the error to create
	 * \param[in] message an errror message
	 */
	Error(PDI_status_t errcode, const char* message);

	const char* what() const noexcept override;

	/** Returns status of the error
	 * \return status of the error
	 */
	PDI_status_t status() const noexcept;
};

class PDI_EXPORT Unavailable_error: public Error
{
public:
	template <typename S, typename... Args>
	Unavailable_error(const S& format_str, Args&&... args)
		: Error(PDI_UNAVAILABLE, std::string("Unavailable_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Unavailable_error(Unavailable_error&&) = default;

	Unavailable_error(const Unavailable_error&) = default;
};

class PDI_EXPORT Config_error: public Error
{
public:
	template <typename S, typename... Args>
	Config_error(PC_tree_t tree, const S& format_str, Args&&... args)
		: Error(PDI_ERR_CONFIG)
	{
		std::ostringstream err_msg;
		if (!PC_status(tree) && tree.node) {
			if (tree.node->start_mark.line == tree.node->end_mark.line) {
				err_msg << "Config_error in line " << (tree.node->start_mark.line + 1) << ":"<<(tree.node->start_mark.column+ 1)<<" ";
			} else {
				err_msg << "Config_error in lines " << tree.node->start_mark.line + 1 << " - " << tree.node->end_mark.line << ": ";
			}
		} else {
			err_msg << "Config_error: ";
		}
		err_msg << fmt::format(format_str, std::forward<Args>(args)...);
		m_what = err_msg.str();
	}

	Config_error(Config_error&&) = default;

	Config_error(const Config_error&) = default;
};

class PDI_EXPORT Value_error: public Error
{
public:
	template <typename S, typename... Args>
	Value_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_VALUE, std::string("Value_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Value_error(Value_error&&) = default;

	Value_error(const Value_error&) = default;
};

class PDI_EXPORT Plugin_error: public Error
{
public:
	template <typename S, typename... Args>
	Plugin_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_PLUGIN, std::string("Plugin_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Plugin_error(Plugin_error&&) = default;

	Plugin_error(const Plugin_error&) = default;
};

class PDI_EXPORT Impl_error: public Error
{
public:
	template <typename S, typename... Args>
	Impl_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_IMPL, std::string("Impl_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Impl_error(Impl_error&&) = default;

	Impl_error(const Impl_error&) = default;
};

class PDI_EXPORT System_error: public Error
{
public:
	template <typename S, typename... Args>
	System_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_SYSTEM, std::string("System_error: ") + format_str, std::forward<Args>(args)...)
	{}

	System_error(System_error&&) = default;

	System_error(const System_error&) = default;
};

class PDI_EXPORT State_error: public Error
{
public:
	template <typename S, typename... Args>
	State_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_STATE, std::string("State_error: ") + format_str, std::forward<Args>(args)...)
	{}

	State_error(State_error&&) = default;

	State_error(const State_error&) = default;
};

class PDI_EXPORT Right_error: public Error
{
public:
	template <typename S, typename... Args>
	Right_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_RIGHT, std::string("Right_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Right_error(Right_error&&) = default;

	Right_error(const Right_error&) = default;
};

class PDI_EXPORT Type_error: public Error
{
public:
	template <typename S, typename... Args>
	Type_error(const S& format_str, Args&&... args)
		: Error(PDI_ERR_TYPE, std::string("Type_error: ") + format_str, std::forward<Args>(args)...)
	{}

	Type_error(Type_error&&) = default;

	Type_error(const Type_error&) = default;
};

} // namespace PDI

#endif // PDI_ERROR_H_
