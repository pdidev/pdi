/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
	template <typename... Args>
	inline constexpr Error(PDI_status_t errcode, fmt::format_string<Args...> format_str, Args&&... args)
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

class PDI_EXPORT Spectree_error: public Error
{
public:
	template <typename... Args>
	Spectree_error(PC_tree_t tree, fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_SPECTREE)
	{
		std::ostringstream err_msg;
		if (!PC_status(tree) && tree.node) {
			if (tree.node->start_mark.line == tree.node->end_mark.line) {
				err_msg << "Spectree_error in line " << tree.node->start_mark.line + 1 << ": ";
			} else {
				err_msg << "Spectree_error in lines " << tree.node->start_mark.line + 1 << " - " << tree.node->end_mark.line << ": ";
			}
		} else {
			err_msg << "Spectree_error: ";
		}
		err_msg << fmt::format(format_str, std::forward<Args>(args)...);
		m_what = err_msg.str();
	}

	Spectree_error(Spectree_error&&) = default;

	Spectree_error(const Spectree_error&) = default;
};

class PDI_EXPORT Value_error: public Error
{
public:
	template <typename... Args>
	inline constexpr Value_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_VALUE, "Value_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	Value_error(Value_error&&) = default;

	Value_error(const Value_error&) = default;
};

class PDI_EXPORT Plugin_error: public Error
{
public:
	template <typename... Args>
	inline constexpr Plugin_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_PLUGIN, "Plugin_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	Plugin_error(Plugin_error&&) = default;

	Plugin_error(const Plugin_error&) = default;
};

class PDI_EXPORT Impl_error: public Error
{
public:
	template <typename... Args>
	inline constexpr Impl_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_IMPL, "Impl_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	Impl_error(Impl_error&&) = default;

	Impl_error(const Impl_error&) = default;
};

class PDI_EXPORT System_error: public Error
{
public:
	template <typename... Args>
	inline constexpr System_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_SYSTEM, "System_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	System_error(System_error&&) = default;

	System_error(const System_error&) = default;
};

class PDI_EXPORT State_error: public Error
{
public:
	template <typename... Args>
	inline constexpr State_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_STATE, "State_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	State_error(State_error&&) = default;

	State_error(const State_error&) = default;
};

class PDI_EXPORT Permission_error: public Error
{
public:
	template <typename... Args>
	inline constexpr Permission_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_PERMISSION, "Permission_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	Permission_error(Permission_error&&) = default;

	Permission_error(const Permission_error&) = default;
};

class PDI_EXPORT Type_error: public Error
{
public:
	template <typename... Args>
	inline constexpr Type_error(fmt::format_string<Args...> format_str, Args&&... args)
		: Error(PDI_ERR_TYPE, "Type_error: {}", fmt::format(format_str, std::forward<Args>(args)...))
	{}

	Type_error(Type_error&&) = default;

	Type_error(const Type_error&) = default;
};

} // namespace PDI

#endif // PDI_ERROR_H_
