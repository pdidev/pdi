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

#include <filesystem>
#include <iostream>
#include <numeric>
#include <ranges>

#include <pdi/testing.h>

class CheckMultiExpose: public ::PDI::PdiTest
{};

/*
 * Name:               CheckMultiExpose.AccesSecondData
 *
 * Description:        Acces to second data on the event "on_data" for the first data of a multi expose
 */

// Function to check the value inside the user_code function
void check_value(const char* var_name, int& var, const int expected_value)
{
	EXPECT_EQ(var, expected_value) << "Wrong value of " << var_name << ": " << var << " != " << expected_value;
}

// Define user_code function
extern "C" {

// Test access of the last argument var2
void test_access_var2(void)
{
	int* value;
	PDI_access("value", (void**)&value, PDI_IN); // Read something from input
	PDI_release("value");
	check_value("second", *value, 3);
}

} // end extern "C"

TEST_F(CheckMultiExpose, AccesSecondData)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  first: int
  second: int
plugins:
  user_code:
    on_data:
      first:
        test_access_var2: { value: $second }
)=="));

	const int var1 = -4;
	const int var2 = 3;

	PDI_multi_expose("my_test", "first", &var1, PDI_OUT, "second", &var2, PDI_OUT, NULL);
}

/*
 * Name:               CheckMultiExpose, DataWithSameName
 *
 * Description:        Verify the behavior of multi_expose when we shared two different
 *                     data in the same place in PDI store.
 */

// Define user_code function
extern "C" {

void add_2(void)
{
	int* value;
	PDI_access("value", (void**)&value, PDI_IN); // Read something from input
	PDI_release("value");
	*value = *value + 2;
}

} // end extern "C"

TEST_F(CheckMultiExpose, DataWithSameNameWithDifferentPointer)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  pdi_var1: int
plugins:
  user_code:
    on_data:
      pdi_var1:
        add_2: { value: $pdi_var1 }
)=="));

	int var1 = 3;
	int var2 = 11;

	PDI_multi_expose("my_test", "pdi_var1", &var1, PDI_OUT, "pdi_var1", &var2, PDI_OUT, NULL);

	EXPECT_EQ(var1, 3) << "Wrong value of var1"; // the reference of pdi_var1 in the store is &var2 => no change in the value
	EXPECT_EQ(var2, 15) << "Wrong value of var2"; // the add_2 function is called two times on reference &var2.
}

TEST_F(CheckMultiExpose, DataWithSameName)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  pdi_var1: int
plugins:
  user_code:
    on_data:
      pdi_var1:
        add_2: { value: $pdi_var1 }
)=="));

	int var1 = 3;
	int var2 = 11;

	PDI_multi_expose("my_test", "pdi_var1", &var1, PDI_OUT, "pdi_var1", &var1, PDI_OUT, NULL);

	EXPECT_EQ(var1, 7) << "Wrong value of var1"; // the reference of pdi_var1 in the store is &var2 => no change in the value
	EXPECT_EQ(var2, 11) << "Wrong value of var2"; // the add_2 function is called two times on reference &var2.
}

TEST_F(CheckMultiExpose, WriteFileName)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  pdi_var1: int
  iter: int
plugins:
  decl_hdf5:
    file: "myfile_iter${iter}.h5"
    write: [pdi_var1]
)=="));

	const int iter = 3;
	const int var1 = 11;

	ASSERT_FALSE(std::filesystem::exists("myfile_iter3.h5"));

	PDI_multi_expose("my_test", "pdi_var1", &var1, PDI_OUT, "iter", &iter, PDI_INOUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("myfile_iter3.h5"));
}

TEST_F(CheckMultiExpose, WriteArrayWithMetadataSize)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
metadata:
  Nsize: int
data:
  pdi_array: { type: array, size: ["$Nsize"], subtype: int }
plugins:
  trace: info
  decl_hdf5:
    file: "mydata.h5"
    write: [Nsize, pdi_array]
)=="));

	int Nsize= 3;
	auto array_data = make_a<std::array<int, 3>>();

	ASSERT_FALSE(std::filesystem::exists("mydata.h5"));

	PDI_multi_expose("my_test", "pdi_array", array_data.data(), PDI_OUT, "Nsize", &Nsize, PDI_INOUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("mydata.h5"));
}

TEST_F(CheckMultiExpose, WriteArrayWithDataSize)
{
	InitPdi(PC_parse_string(R"==(
logging: trace
data:
  pdi_array: { type: array, size: ["$Nsize"], subtype: int }
  Nsize: int
plugins:
  trace: info
  decl_hdf5:
    file: "mydata.h5"
    write: [Nsize, pdi_array]
)=="));

	int Nsize= 3;
	auto array_data = make_a<std::array<int, 3>>();

	ASSERT_FALSE(std::filesystem::exists("mydata.h5"));

	PDI_multi_expose("my_test", "pdi_array", array_data.data(), PDI_OUT, "Nsize", &Nsize, PDI_INOUT, NULL);

	EXPECT_TRUE(std::filesystem::exists("mydata.h5"));
}