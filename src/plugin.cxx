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

#include <pdi/context.h>
#include <pdi/ref_any.h>

#include "pdi/plugin.h"


namespace PDI {

Plugin::Plugin(Context& ctx):
	m_context{ctx}
{}

Plugin::~Plugin() noexcept(false)
{}

void Plugin::event(const char*)
{}

void Plugin::data(const char*, Ref)
{}

Context& Plugin::context()
{
	return m_context;
}

unsigned long plugin_api_version(unsigned long expected_version)
{
	constexpr unsigned long MASK = (1<<8) - 1;
	unsigned long expected_major = (expected_version>>24)&MASK;
	unsigned long expected_minor = (expected_version>>16)&MASK;
	unsigned long expected_patch = (expected_version>>8 )&MASK;
	
	if (
	    expected_version
	    && (
	        expected_major != PLUGIN_API_VERSION_MAJOR
	        || expected_minor > PLUGIN_API_VERSION_MINOR
	    )
	) {
		throw Error{
			PDI_ERR_PLUGIN,
			"Invalid plugin API version: %lu.%lu.%lu, PDI provided version is %lu.%lu.%lu",
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

}
