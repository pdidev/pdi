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


#include <type_traits>

#include <gtest/gtest.h>

#include <paraconf.h>
#include <pdi.h>

// The version macros must be usable in a preprocessor #if
#if PDI_VERSION < PDI_COMPUTE_VERSION(1, 0, 0) || PDI_VERSION >= PDI_COMPUTE_VERSION(PDI_VERSION_MAJOR + 1, 0, 0)
#error PDI_VERSION and PDI_COMPUTE_VERSION disagree in the preprocessor
#endif

/* Name:                PdiVersion.compute_version
 *
 * Tested macros:       PDI_COMPUTE_VERSION, PDI_VERSION
 *
 * Description:         Test that the PDI version macros are int, as PDI_VERSION has always been, with the documented layout, also with a value
 *                      computed at runtime.
 */
TEST(PdiVersion, compute_version)
{
	static_assert(std::is_same<decltype(PDI_VERSION), int>::value, "PDI_VERSION is not an int");
	static_assert(std::is_same<decltype(PDI_COMPUTE_VERSION(1, 2, 3)), int>::value, "PDI_COMPUTE_VERSION is not an int");
	static_assert(PDI_COMPUTE_VERSION(1, 2, 3) == 0x01020300, "PDI_COMPUTE_VERSION has the wrong layout");
	static_assert(PDI_COMPUTE_VERSION(127, 255, 255) == 0x7FFFFF00, "PDI_COMPUTE_VERSION overflows its components");
	static_assert(PDI_VERSION == PDI_COMPUTE_VERSION(PDI_VERSION_MAJOR, PDI_VERSION_MINOR, PDI_VERSION_PATCH), "PDI_VERSION is not computed");
	volatile int major = 2;
	EXPECT_EQ(PDI_COMPUTE_VERSION(major, 0, 0), 0x02000000);
}

/* Name:                PdiVersion.matches_the_library
 *
 * Tested functions:    PDI_version()
 *
 * Description:         Test that PDI_version() provides the version of the headers, and accepts the one PDI_COMPUTE_VERSION computes.
 */
TEST(PdiVersion, matches_the_library)
{
	PDI_init(PC_parse_string("{}"));
	unsigned long provided = 0;
	EXPECT_EQ(PDI_OK, PDI_version(&provided, PDI_COMPUTE_VERSION(PDI_VERSION_MAJOR, 0, 0)));
	EXPECT_EQ(PDI_VERSION, provided);
	PDI_finalize();
}
