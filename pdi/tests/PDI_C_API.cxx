/*******************************************************************************
 * Copyright (C) 2015-2022 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#include <string>

#include <gtest/gtest.h>

#include <paraconf.h>
#include <pdi.h>

class PdiCApiTest: public ::testing::Test
{
protected:
	void TearDown() override { PDI_finalize(); }
};

/* Name:                PdiCApiTest.InvalidMetadataRef
 *
 * Tested functions:    PDI_expose()
 *
 * Description:         Test that PDI_expose returns the expected error if the
 *                      exposed data type references an invalid metadata.
 */
TEST_F(PdiCApiTest, InvalidMetadataRef)
{
	static const char* CONFIG_YAML
		= "logging: trace          \n"
		  "data:                   \n"
		  "  invalid:              \n"
		  "    type: array         \n"
		  "    subtype: double\n"
		  "    size: $meta2        \n"
		  "plugins:                \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	PDI_errhandler(PDI_NULL_HANDLER);
	double invalid;
	PDI_status_t err = PDI_expose("invalid", &invalid, PDI_INOUT);
	std::string errmsg = PDI_errmsg();
	EXPECT_FALSE(errmsg.find("Cannot access a non shared value: `meta2'") == std::string::npos)
		<< "    " << errmsg << "\n"
		<< "  does not contain:\n"
		<< "    Cannot access a non shared value: `meta2'";
	EXPECT_EQ(err, PDI_ERR_VALUE);
}

/* Name:                PdiCApiTest.MetadataDensification
 *
 * Tested functions:    PDI_expose()
 *
 * Description:         Test that the content of metadata is densified as
 *                      expected after copy
 */
TEST_F(PdiCApiTest, MetadataDensification)
{
	static const char* CONFIG_YAML
		= "logging: trace         \n"
		  "metadata:              \n"
		  "  my_array:            \n"
		  "    size: [10, 10, 10] \n"
		  "    subsize: [3, 4, 5] \n"
		  "    start: [1, 2, 3]   \n"
		  "    type: array        \n"
		  "    subtype: int  \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	int sparse_array[1000]; //buffer: 10 x 10 x 10; data: 3 x 4 x 5; start: (1, 2, 3)
	int* dense_array; //buffer: 3 x 4 x 5

	for (int i = 0; i < 1000; i++) {
		sparse_array[i] = i;
	}

	// metadata expose creates a dense copy inside PDI
	PDI_expose("my_array", sparse_array, PDI_OUT);
	PDI_access("my_array", (void**)&dense_array, PDI_IN);

	for (int i = 0; i < 60; i++) {
		EXPECT_EQ(dense_array[i], (i / 20 + 1) * 100 + (i / 5 % 4 + 2) * 10 + i % 5 + 3);
	}
}

/* Name:                PdiCApiTest.PDI_expose
 *
 * Tested functions:    PDI_share(), PDI_share_const()
 *
 * Description:         Test PDI_share API call
 */
TEST_F(PdiCApiTest, PDI_share)
{
	static const char* CONFIG_YAML
		= "logging: trace         \n"
		  "metadata:              \n"
		  "  my_int: int \n";
		  "  my_const_int: int \n";

	PDI_init(PC_parse_string(CONFIG_YAML));

	int i=42;
	const int j=51;
	int* int_ptr;

	// share
	PDI_share("my_int", &i, PDI_OUT);
	EXPECT_EQ(i, 42);
	PDI_access("my_int", (void**)&int_ptr, PDI_IN);
	EXPECT_EQ(*int_ptr, 42);

	// share_const
	PDI_share_const("my_const_int", &j);	// no need for access as it is const data
	EXPECT_EQ(j, 51);
	PDI_access("my_const_int", (void**)&int_ptr, PDI_IN);
	EXPECT_EQ(*int_ptr, 51);
}
