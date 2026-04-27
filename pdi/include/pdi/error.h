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

#include <concepts>
#include <exception>
#include <iostream>
#include <ranges>
#include <sstream>
#include <stdexcept>
#include <string>

#include <spdlog/spdlog.h>

#include <pdi/pdi_fwd.h>
#include <pdi/paraconf_wrapper.h>

namespace PDI {

/// A concept that represent a "range" (list) of errors, in any kind of storage
template <typename R>
concept range_of_exception_ptrs
	= std::ranges::input_range<R> && std::ranges::sized_range<R> && std::convertible_to<std::ranges::range_reference_t<R>, std::exception_ptr>;

/** An error class from which all PDI error are children.
 * 
 * Offers access to 
 * - an error message (`what`) from std::exception
 * - an error code (`status`) 
 * - a longer error message for verbose output (`full_msg`)
 * - the ability to be rethrown with added context (`rethrow_with_context`)
 */
class PDI_EXPORT Error: virtual public std::exception
{
public:
	virtual ~Error() noexcept;

	/** Gives access to the status of the error
	 * \returns the status of the error
	 */
	virtual PDI_status_t status() const noexcept = 0;

	/** Rethrow the error with some context prepended to its description
	 * \param context the context to prepend to the error message
	 */
	[[noreturn]] virtual void rethrow_with_context(std::string context) const = 0;

	/** Gives access to a full error message 
	 * 
	 * The message includes all available context as well as the info about error type
	 * 
	 * \returns the full message
	 */
	virtual std::string full_msg() const = 0;

	/** Rethrow the error with some context prepended to its description
	 * \param[in] format_str the context to prepend to the error message as a python-style {fmt} format
	 * \param[in] args the fmt parameters for the message
	 */
	template <typename... Args>
	[[noreturn]] void inline rethrow_with_context(fmt::format_string<Args...> format_str, Args&&... args) const
	{
		rethrow_with_context(fmt::format(format_str, std::forward<Args>(args)...));
		std::abort(); // rethrow_with_context should never return, this is just to silence compiler warnings
	}
};

namespace impl {

/// a "trait" that implements `what` from std::exception
class PDI_EXPORT what_impl: virtual public std::exception
{
private:
	/// message of the error
	std::string m_what;

protected:
	/** build a new what_impl by specifying the message of the error
	 * \param what the message of the error
	 */
	what_impl(std::string what) noexcept;

public:
	const char* what() const noexcept override;
};

/// a "trait" that implements `status` from PDI::Error
template <PDI_status_t STATUS>
class PDI_EXPORT status_impl: virtual public Error
{
public:
	PDI_status_t status() const noexcept override { return STATUS; }
};

/// a basic implementation of PDI:Error
template <PDI_status_t STATUS>
class PDI_EXPORT Error_impl
	: public what_impl
	, public status_impl<STATUS>
{
public:
	/** build a new Error_impl by specifying the message of the error
	 * \param what the message of the error
	 */
	Error_impl(std::string what) noexcept;

	/** build a new Error_impl by specifying the message of the error
	 * \param[in] format_str an error message as a python-style {fmt} format
	 * \param[in] args the fmt parameters for the message
	 */
	template <typename... Args>
	inline Error_impl(fmt::format_string<Args...> format_str, Args&&... args) noexcept
		: Error_impl(fmt::format(format_str, std::forward<Args>(args)...))
	{}

	using what_impl::what;

	using status_impl<STATUS>::status;

	std::string full_msg() const override;

	[[noreturn]] void rethrow_with_context(std::string msg) const override;
};

} // namespace impl

/** An error class to use when a value expression is invalid
 */
using Value_error = impl::Error_impl<PDI_ERR_VALUE>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_VALUE>;

/** An error class to use when trying to load a non-existing plugin
 */
using Plugin_error = impl::Error_impl<PDI_ERR_PLUGIN>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_PLUGIN>;

/** An error class to use for implementation limitations (typically an unimplemented feature)
 */
using Impl_error = impl::Error_impl<PDI_ERR_IMPL>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_IMPL>;

/** An error class to use when a call to a function has been made at a wrong time (e.g. closing an
 *  unopened transaction)
 */
using State_error = impl::Error_impl<PDI_ERR_STATE>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_STATE>;

/** An error class to use when a conflict of ownership over a content has been raised
 */
using Permission_error = impl::Error_impl<PDI_ERR_PERMISSION>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_PERMISSION>;

/** An error class to use when a system error occurred (OS, etc.)
 */
using System_error = impl::Error_impl<PDI_ERR_SYSTEM>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_SYSTEM>;

/** An error class to use for invalid types
 */
using Type_error = impl::Error_impl<PDI_ERR_TYPE>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_TYPE>;

/** An error class to use when an action described in the specification tree is invalid
 */
using Invalid_action_error = impl::Error_impl<PDI_ERR_INVALIDACTION>;
extern template class PDI_EXPORT impl::Error_impl<PDI_ERR_INVALIDACTION>;

/** An error class to use when there is an invalid entry in the specification tree
 */
class PDI_EXPORT Spectree_error
	: public impl::what_impl
	, public impl::status_impl<PDI_ERR_SPECTREE>
{
private:
	std::optional<Yaml_region> m_location;

	Spectree_error(std::optional<Yaml_region> location, std::string what);

public:
	/** Creates a new Spectree_error
	 * \param[in] tree the subtree that's in error
	 * \param[in] what an error message
	 */
	Spectree_error(PC_tree_t tree, std::string what);

	/** Creates a new Spectree_error
	 * \param[in] tree the subtree that's in error
	 * \param[in] format_str an error message as a python-style format
	 * \param[in] args the fmt parameters for the message
	 */
	template <typename... Args>
	inline Spectree_error(PC_tree_t tree, fmt::format_string<Args...> format_str, Args&&... args)
		: Spectree_error(tree, fmt::format(format_str, std::forward<Args>(args)...))
	{}

	using what_impl::what;

	using status_impl::status;

	std::string full_msg() const;

	[[noreturn]] void rethrow_with_context(std::string msg) const;
};

/** An error class to use when multiple errors of different kind have happened
 */
class PDI_EXPORT Multiple_errors
	: public impl::status_impl<PDI_ERR_MULTIPLE>
	, public impl::what_impl
{
private:
	/// The list or original errors
	std::vector<std::exception_ptr> m_nested_ptrs;

public:
	/** Creates a new Multiple_errors
	 * \param[in] causes the list or original errors
	 * \param[in] what an error message
	 */
	Multiple_errors(std::vector<std::exception_ptr> causes, std::string what) noexcept;

	/** Creates a new Multiple_errors
	 * \param[in] causes the list or original errors
	 * \param[in] format_str an error message as a python-style format
	 * \param[in] args the fmt parameters for the message
	 */
	template <typename... Args>
	inline Multiple_errors(range_of_exception_ptrs auto&& causes, fmt::format_string<Args...> format_str, Args&&... args) noexcept
		: Multiple_errors(std::vector<std::exception_ptr>(causes.begin(), causes.end()), fmt::format(format_str, std::forward<Args>(args)...))
	{}

	/** Gives access to the list or original errors
	 * \return the list or original errors
	 */
	std::vector<std::exception_ptr> const & nested_ptrs() const;

	using status_impl::status;

	using what_impl::what;

	std::string full_msg() const override;

	[[noreturn]] void rethrow_with_context(std::string msg) const override;
};

/** Throws a new exception by adding context to an existing exception
 * 
 * \param[in] err original error
 * \param[in] msg some context to prepend to the error
 */
[[noreturn]] void PDI_EXPORT rethrow_with_simple_context(std::exception_ptr err, std::string msg);

/** Throws a new exception by adding context to an existing exception
 * 
 * \param[in] err original error
 * \param[in] format_str some context to prepend to the error as a python-style {fmt} format
 * \param[in] args the fmt parameters for the message
 */
template <typename... Args>
[[noreturn]] static inline void rethrow_with_context(std::exception_ptr err, fmt::format_string<Args...> format_str, Args&&... args)
{
	rethrow_with_simple_context(err, fmt::format(format_str, std::forward<Args>(args)...));
}

/** Throws a new exception by adding context to an existing set of exceptions
 * 
 * - if the set of exception (errors) is empty, do nothing,
 * - if it contains a single exception, throw it with additional context
 * - if it contains multiple exceptions, throw a `Multiple_errors`
 * 
 * \param[in] errors a list of original errors
 * \param[in] msg some context to prepend to the error
 */
void PDI_EXPORT rethrow_with_simple_context(std::vector<std::exception_ptr> errors, std::string msg);

/** Throws a new exception by adding context to an existing set of exceptions
 * 
 * - if the set of exception (errors) is empty, do nothing,
 * - if it contains a single exception, throw it with additional context
 * - if it contains multiple exceptions, throw a `Multiple_errors`
 * 
 * \param[in] errors a list of original errors
 * \param[in] format_str some context to prepend to the error as a python-style {fmt} format
 * \param[in] args the fmt parameters for the message
 */
template <typename... Args>
static inline void rethrow_with_context(range_of_exception_ptrs auto&& errors, fmt::format_string<Args...> format_str, Args&&... args)
{
	rethrow_with_simple_context(std::vector<std::exception_ptr>(errors.begin(), errors.end()), fmt::format(format_str, std::forward<Args>(args)...));
}

} // namespace PDI
#endif // PDI_ERROR_H_
