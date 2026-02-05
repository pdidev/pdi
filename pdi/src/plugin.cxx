// SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "config.h"

#include <pdi/context.h>
#include <pdi/ref_any.h>

#include "pdi/plugin.h"

namespace PDI {

Plugin::Plugin(Context& ctx)
	: m_context{ctx}
{}

Plugin::~Plugin() noexcept(false) {}

Context& Plugin::context()
{
	return m_context;
}

unsigned long plugin_api_version(unsigned long expected_version)
{
	constexpr unsigned long MASK = (1 << 8) - 1;
	unsigned long expected_major = (expected_version >> 24) & MASK;
	unsigned long expected_minor = (expected_version >> 16) & MASK;
	unsigned long expected_patch = (expected_version >> 8) & MASK;

	if (expected_version && (expected_major != PLUGIN_API_VERSION_MAJOR || expected_minor > PLUGIN_API_VERSION_MINOR)) {
		throw Plugin_error{
			"Invalid plugin API version: {}.{}.{}, PDI provided version is {}.{}.{}",
			expected_major,
			expected_minor,
			expected_patch,
			PLUGIN_API_VERSION_MAJOR,
			PLUGIN_API_VERSION_MINOR,
			PLUGIN_API_VERSION_PATCH
		};
	}
	return PLUGIN_API_VERSION;
}

} // namespace PDI
