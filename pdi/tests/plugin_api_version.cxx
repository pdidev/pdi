/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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


#include <cstdint>
#include <type_traits>

#include <gtest/gtest.h>

#include <pdi/error.h>
#include <pdi/plugin.h>

// The version macros must be usable in a preprocessor #if, where a cast would not be accepted
#if PDI_PLUGIN_API_VERSION != PDI_COMPUTE_PLUGIN_API_VERSION(PDI_PLUGIN_API_VERSION_MAJOR, PDI_PLUGIN_API_VERSION_MINOR, PDI_PLUGIN_API_VERSION_PATCH)
#error PDI_PLUGIN_API_VERSION and PDI_COMPUTE_PLUGIN_API_VERSION disagree in the preprocessor
#endif

/* Name:                PdiPluginApiVersion.compute_version
 *
 * Tested macros:       PDI_COMPUTE_PLUGIN_API_VERSION, PDI_PLUGIN_API_VERSION
 * Tested functions:    PDI::plugin_api_version()
 *
 * Description:         Test that the plugin API version macros have the type of PDI::plugin_api_version(), the documented layout, and match what
 *                      it returns.
 */
TEST(PdiPluginApiVersion, compute_version)
{
	static_assert(std::is_same<decltype(PDI_PLUGIN_API_VERSION), uint64_t>::value, "PDI_PLUGIN_API_VERSION is not a uint64_t");
	static_assert(PDI_COMPUTE_PLUGIN_API_VERSION(1, 2, 3) == UINT64_C(0x0001000200030000), "PDI_COMPUTE_PLUGIN_API_VERSION has the wrong layout");
	EXPECT_EQ(PDI_PLUGIN_API_VERSION, PDI::plugin_api_version(PDI_PLUGIN_API_VERSION));
	EXPECT_EQ(PDI_PLUGIN_API_VERSION, PDI::plugin_api_version());
}

/* Name:                PdiPluginApiVersion.follows_pdi
 *
 * Tested macros:       PDI_PLUGIN_API_VERSION_MAJOR, PDI_PLUGIN_API_VERSION_MINOR, PDI_PLUGIN_API_VERSION_PATCH
 *
 * Description:         Test that the plugin API version is 0.(major * 128 + minor).patch of the version of PDI.
 */
TEST(PdiPluginApiVersion, follows_pdi)
{
	static_assert(PDI_PLUGIN_API_VERSION_MAJOR == 0, "the plugin API is not stable yet");
	static_assert(PDI_PLUGIN_API_VERSION_MINOR == PDI_VERSION_MAJOR * 128 + PDI_VERSION_MINOR, "the plugin API minor does not follow PDI");
	static_assert(PDI_PLUGIN_API_VERSION_PATCH == PDI_VERSION_PATCH, "the plugin API patch does not follow PDI");
	EXPECT_EQ(PDI_PLUGIN_API_VERSION, PDI_COMPUTE_PLUGIN_API_VERSION(0, PDI_VERSION_MAJOR * 128 + PDI_VERSION_MINOR, PDI_VERSION_PATCH));
}

/* Name:                PdiPluginApiVersion.other_minor_rejected
 *
 * Tested functions:    PDI::plugin_api_version()
 *
 * Description:         Test that while the plugin API major is 0, a plugin built for another minor is rejected, whether lower or greater, while
 *                      another patch is accepted.
 */
TEST(PdiPluginApiVersion, other_minor_rejected)
{
	EXPECT_NO_THROW(PDI::plugin_api_version(PDI_COMPUTE_PLUGIN_API_VERSION(0, PDI_PLUGIN_API_VERSION_MINOR, PDI_PLUGIN_API_VERSION_PATCH + 1)));
	EXPECT_THROW(PDI::plugin_api_version(PDI_COMPUTE_PLUGIN_API_VERSION(0, PDI_PLUGIN_API_VERSION_MINOR - 1, 0)), PDI::Plugin_error);
	EXPECT_THROW(PDI::plugin_api_version(PDI_COMPUTE_PLUGIN_API_VERSION(0, PDI_PLUGIN_API_VERSION_MINOR + 1, 0)), PDI::Plugin_error);
	EXPECT_THROW(PDI::plugin_api_version(PDI_COMPUTE_PLUGIN_API_VERSION(1, PDI_PLUGIN_API_VERSION_MINOR, 0)), PDI::Plugin_error);
}
