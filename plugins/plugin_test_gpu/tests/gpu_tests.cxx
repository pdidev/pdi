/*******************************************************************************
 * Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <cstddef>
#include <cstdio>
#include <cstdlib>
#include <memory>

#include <gtest/gtest.h>

#include <pdi.h>

TEST(gpu_support, 00)
{
	const char* CONFIG_YAML
		= "pdi:                                    \n"
		  "  logging:                              \n"
		  "    pattern: \"[%T][%n] -- %^%l%$: %v\" \n"
		  "    level: debug                        \n"
		  "  data:                                 \n"
		  "    var11: int                          \n"
		  "    var22: int                          \n"
		  "    var13: int                          \n"
		  "    var23: int                          \n"
		  "    var12: int                          \n"
		  "    var21: int                          \n"
		  "                                        \n"
		  "  plugins:                              \n"
		  "    test_gpu:                           \n"
		  "      - var11: cpu                      \n"
		  "      - var22: gpu                      \n"
		  "      - var13: both                     \n"
		  "      - var23: both                     \n"
		  "      - var12: gpu                      \n"
		  "      - var21: cpu                      \n"
		//
		;

	// PC_tree_t conf = PC_parse_path("../debug.yml");
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* Testing */
	// Share a variable from CPU to CPU
	int var11 = 11;
	printf("Sharing var11. Value is %d\n", var11);
	PDI_expose("var11", &var11, PDI_INOUT);
	EXPECT_EQ(var11, 11);

	// Share a variable from GPU to GPU
	int var22 = 22;
	printf("Sharing var22. Value is %d\n", var22);
	PDI_expose("var22", &var22, PDI_GPU_INOUT);
	EXPECT_EQ(var22, 22);

	// Share a variable from CPU to BOTH
	int var13 = 13;
	printf("Sharing var13. Value is %d\n", var13);
	PDI_expose("var13", &var13, PDI_INOUT);
	EXPECT_EQ(var13, 13);

	// Share a variable from GPU to BOTH
	int var23 = 23;
	printf("Sharing var23. Value is %d\n", var23);
	PDI_expose("var23", &var23, PDI_INOUT);
	EXPECT_EQ(var23, 23);

	// Share a variable from CPU to GPU
	int var12 = 12;
	printf("Sharing var12. Value is %d\n", var12);
	PDI_expose("var12", &var12, PDI_GPU_INOUT);
	EXPECT_EQ(var12, 12);

	// Share a variable from GPU to CPU
	int var21 = 21;
	printf("Sharing var21. Value is %d\n", var21);
	PDI_expose("var21", &var21, PDI_GPU_INOUT);
	EXPECT_EQ(var21, 21);

	// printf("-- Requesting var from GPU\n");
	// PDI_expose("var", &var, PDI_GPU_IN);
	// printf("var : %d\n", var);

	PDI_finalize();
}

TEST(gpu_support_two_vars, 11_cpu_cpu)
{
	const char* CONFIG_YAML
		= "pdi:                                    \n"
		  "  data:                                 \n"
		  "    var11: int                          \n"
		  "    var11r: int                         \n"
		  "  plugins:                              \n"
		  "    test_gpu:                           \n"
		  "      - var11: cpu                      \n";

	// PC_tree_t conf = PC_parse_path("../debug.yml");
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* Testing */
	// Share a variable from CPU to CPU
	int var11 = 11;
	int var11r = 0;

	PDI_share("var11r", &var11r, PDI_IN);
	PDI_expose("var11", &var11, PDI_OUT);
	PDI_reclaim("var11r");

	EXPECT_EQ(var11, 11);
	EXPECT_EQ(var11r, 11);

	PDI_finalize();
}

TEST(gpu_support_two_vars, 12_cpu_gpu)
{
	const char* CONFIG_YAML
		= "pdi:                                    \n"
		  "  data:                                 \n"
		  "    var12: int                          \n"
		  "    var12r: int                         \n"
		  "  plugins:                              \n"
		  "    test_gpu:                           \n"
		  "      - var12: gpu                      \n";

	// PC_tree_t conf = PC_parse_path("../debug.yml");
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* Testing */
	// Share a variable from CPU to GPU
	int var12 = 12;
	int var12r = 0;

	PDI_share("var12r", &var12r, PDI_IN);
	PDI_expose("var12", &var12, PDI_OUT);
	PDI_reclaim("var12r");

	EXPECT_EQ(var12, 12);
	EXPECT_EQ(var12r, 12);

	PDI_finalize();
}

TEST(gpu_support_two_vars, 12_cpu_both)
{
	const char* CONFIG_YAML
		= "pdi:                                    \n"
		  "  data:                                 \n"
		  "    var13: int                          \n"
		  "    var13r: int                         \n"
		  "  plugins:                              \n"
		  "    test_gpu:                           \n"
		  "      - var13: both                     \n";

	// PC_tree_t conf = PC_parse_path("../debug.yml");
	PC_tree_t conf = PC_parse_string(CONFIG_YAML);
	PDI_init(PC_get(conf, ".pdi"));

	/* Testing */
	// Share a variable from CPU to BOTH
	int var13 = 13;
	int var13r = 0;

	PDI_share("var13r", &var13r, PDI_IN);
	PDI_expose("var13", &var13, PDI_OUT);
	PDI_reclaim("var13r");

	EXPECT_EQ(var13, 13);
	EXPECT_EQ(var13r, 13);

	PDI_finalize();
}
