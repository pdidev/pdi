/*******************************************************************************
 * Copyright (C) 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
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

#include "pdi/context.h"
#include "pdi/logger.h"
#include "pdi/ref_any.h"

#include "pdi/plugin.h"

namespace PDI {

Plugin::Plugin(Logger& logger, Context& ctx)
	: m_context{ctx}
	, m_logger(logger)
{}

Plugin::~Plugin() noexcept(false) {}

Context& Plugin::context()
{
	return m_context;
}

Logger& Plugin::logger()
{
	return m_logger;
}

uint64_t plugin_api_version(uint64_t expected_version)
{
	constexpr uint64_t MASK = (1 << 16) - 1;
	uint64_t expected_major = (expected_version >> 48) & MASK;
	uint64_t expected_minor = (expected_version >> 32) & MASK;
	uint64_t expected_patch = (expected_version >> 16) & MASK;

	// before 1.0, every minor revision may break the API
	bool minor_compatible
		= PDI_PLUGIN_API_VERSION_MAJOR == 0 ? expected_minor == PDI_PLUGIN_API_VERSION_MINOR : expected_minor <= PDI_PLUGIN_API_VERSION_MINOR;
	if (expected_version && (expected_major != PDI_PLUGIN_API_VERSION_MAJOR || !minor_compatible)) {
		throw Plugin_error{
			"Invalid plugin API version: {}.{}.{}, PDI provided version is {}.{}.{}",
			expected_major,
			expected_minor,
			expected_patch,
			PDI_PLUGIN_API_VERSION_MAJOR,
			PDI_PLUGIN_API_VERSION_MINOR,
			PDI_PLUGIN_API_VERSION_PATCH
		};
	}
	return PDI_PLUGIN_API_VERSION;
}

} // namespace PDI
