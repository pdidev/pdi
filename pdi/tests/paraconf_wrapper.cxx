/*******************************************************************************
 * Copyright (C) 2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "pdi/error.h"
#include "pdi/paraconf_wrapper.h"

using namespace PDI;

TEST(ParaconfWrapper, ToString)
{
	PC_tree_t tree = PC_parse_string(R"==(
subtree1:
subtree2:
  - list
  - elem2
)==");
	EXPECT_THAT(to_string(Yaml_region::make(PC_get(tree, ".subtree1"))), testing::HasSubstr("(2:10 -> 2:10)"));
	EXPECT_THAT(to_string(*Yaml_region::make(PC_get(tree, ".subtree1"))), testing::HasSubstr("(2:10 -> 2:10)"));
	EXPECT_THAT(to_string(Yaml_region::make(PC_get(tree, ".subtree2"))), testing::HasSubstr("(4:3 -> 6:1)"));
	EXPECT_THAT(to_string(*Yaml_region::make(PC_get(tree, ".subtree2"))), testing::HasSubstr("(4:3 -> 6:1)"));
	PC_errhandler(PC_NULL_HANDLER);
	EXPECT_EQ(to_string(Yaml_region::make(PC_get(tree, ".invalid"))), "");
}

TEST(ParaconfWrapper, ToBool)
{
	PC_errhandler(PC_NULL_HANDLER);
	for (auto&& yaml: {"y", "Y", "yes", "Yes", "YES", "true", "True", "TRUE", "on", "On", "ON"}) {
		EXPECT_TRUE(to_bool(PC_parse_string(yaml))) << yaml;
		EXPECT_TRUE(to_bool(PC_parse_string(yaml), false)) << yaml;
	}
	for (auto&& yaml: {"n", "N", "no", "No", "NO", "false", "False", "FALSE", "off", "Off", "OFF"}) {
		EXPECT_FALSE(to_bool(PC_parse_string(yaml))) << yaml;
		EXPECT_FALSE(to_bool(PC_parse_string(yaml), true)) << yaml;
	}
	for (auto&& yaml: {"''", "maybe", "1", "yES", "nO", "tRUE", "oN", "oFF", "of", "onn", "yess", "[on]", "{on: on}"}) {
		EXPECT_THROW(to_bool(PC_parse_string(yaml)), Spectree_error) << yaml;
		EXPECT_TRUE(to_bool(PC_parse_string(yaml), true)) << yaml;
		EXPECT_FALSE(to_bool(PC_parse_string(yaml), false)) << yaml;
	}
	PC_tree_t missing = PC_get(PC_parse_string("{}"), ".missing");
	EXPECT_THROW(to_bool(missing), Spectree_error);
	EXPECT_TRUE(to_bool(missing, true));
	EXPECT_FALSE(to_bool(missing, false));
}
