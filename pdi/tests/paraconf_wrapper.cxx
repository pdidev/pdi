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

#include <gmock/gmock.h>
#include <gtest/gtest.h>

#include "pdi/paraconf_wrapper.h"

using namespace PDI;
using namespace std;

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
