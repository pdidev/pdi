/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include "pdi/fmt.h"

template <>
struct fmt::formatter<std::exception_ptr>: formatter<std::string_view> {
	// parse is inherited from formatter<string_view>.

	format_context::iterator format(std::exception_ptr p, format_context& ctx) const
	{
		try {
			std::rethrow_exception(p);
		} catch (PDI::Error const & err) {
			return formatter<std::string_view>::format(err.full_msg(), ctx);
		} catch (std::exception const & err) {
			return formatter<std::string_view>::format(err.what(), ctx);
		} catch (...) {
			return formatter<std::string_view>::format("unknown error", ctx);
		}
	}
};

namespace PDI {

Error::~Error() noexcept = default;

namespace impl {

what_impl::what_impl(std::string what) noexcept
	: m_what{std::move(what)}
{
	// force inclusion of dtor
	fmt::format_error("");
}

const char* what_impl::what() const noexcept
{
	return m_what.c_str();
}

template <PDI_status_t STATUS>
Error_impl<STATUS>::Error_impl(std::string what) noexcept
	: what_impl(what)
{}

template <PDI_status_t STATUS>
std::string Error_impl<STATUS>::full_msg() const
{
	return fmt::format("{}: {}", PDI_STATUS_MSG[status()], what());
}

template <PDI_status_t STATUS>
[[noreturn]] void Error_impl<STATUS>::rethrow_with_context(std::string msg) const
{
	throw Error_impl("{}{}", msg, what());
}

template class PDI_EXPORT impl::Error_impl<PDI_ERR_VALUE>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_PLUGIN>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_IMPL>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_SYSTEM>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_STATE>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_PERMISSION>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_TYPE>;
template class PDI_EXPORT impl::Error_impl<PDI_ERR_INVALIDACTION>;

} // namespace impl

Spectree_error::Spectree_error(std::optional<Yaml_region> location, std::string what)
	: impl::what_impl(what)
	, m_location(location)
{}

Spectree_error::Spectree_error(PC_tree_t tree, std::string what)
	: Spectree_error(Yaml_region::make(tree), std::move(what))
{}

std::string Spectree_error::full_msg() const
{
	if (!!m_location) {
		auto& location = *m_location;
		return fmt::format(
			"{}: {}({}:{} -> {}:{}) {}",
			PDI_STATUS_MSG[status()],
			location.file(),
			location.start().line,
			location.start().column,
			location.end().line,
			location.end().column,
			what()
		);
	}
	return fmt::format("{}: {}", PDI_STATUS_MSG[status()], what());
}

[[noreturn]] void Spectree_error::rethrow_with_context(std::string msg) const
{
	throw Spectree_error(m_location, fmt::format("{}{}", msg, what()));
}

Multiple_errors::Multiple_errors(std::vector<std::exception_ptr> causes, std::string what) noexcept
	: impl::what_impl(std::move(what))
	, m_nested_ptrs(std::move(causes))
{}

std::string Multiple_errors::full_msg() const
{
	return fmt::format("{}: {}\n * {}", PDI_STATUS_MSG[status()], what(), fmt::join(m_nested_ptrs, "\n * "));
}

std::vector<std::exception_ptr> const & Multiple_errors::nested_ptrs() const
{
	return m_nested_ptrs;
}

[[noreturn]] void Multiple_errors::rethrow_with_context(std::string msg) const
{
	throw Multiple_errors(nested_ptrs(), "{}{}", msg, what());
}

[[noreturn]] void rethrow_with_simple_context(std::exception_ptr err, std::string msg)
{
	try {
		std::rethrow_exception(err);
	} catch (Error const & e) {
		e.rethrow_with_context(std::move(msg));
	} catch (...) {
		std::throw_with_nested(System_error(std::move(msg)));
	}
	std::abort(); // one of the above should have thrown, this is just to silence compiler warnings
}

void rethrow_with_simple_context(std::vector<std::exception_ptr> errors, std::string msg)
{
	switch (errors.size()) {
	case 0:
		return;
	case 1:
		rethrow_with_simple_context(errors[0], std::move(msg));
	default:
		throw Multiple_errors(errors, std::move(msg));
	}
}

} // namespace PDI
