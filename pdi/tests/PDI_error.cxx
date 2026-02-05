// SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
//
// SPDX-License-Identifier: BSD-3-Clause

#include <gtest/gtest.h>

#include <pdi/error.h>

using namespace PDI;

/*
 * Name:                ErrorTest.call_constructor_no_vargs
 *
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 *
 * Description:         Test calls Error constructor without vargs
 *                      and checks what() and status() values.
 *
 */
TEST(ErrorTest, call_constructor_no_vargs)
{
	Error error(PDI_OK, "No error.");
	ASSERT_STREQ("No error.", error.what());
	ASSERT_EQ(error.status(), PDI_OK);
}

/*
 * Name:                ErrorTest.call_constructor_vargs
 *
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 *
 * Description:         Test calls Error constructor with 2 vargs
 *                      and checks what() and status() values.
 *
 */
TEST(ErrorTest, call_constructor_vargs)
{
	Error error(PDI_UNAVAILABLE, "{} errors in {}?", 0, "ErrorTest");
	ASSERT_STREQ("0 errors in ErrorTest?", error.what());
	ASSERT_EQ(error.status(), PDI_UNAVAILABLE);
}

/*
 * Name:                ErrorTest.call_constructor_va_list
 *
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 *
 * Description:         Test calls Error constructor with va_list
 *                      (containing 2 strings) and checks what() and
 *                      status() values.
 *
 */
TEST(ErrorTest, call_constructor_va_list)
{
	Error error{PDI_OK, "Testing {} in {}", "what", "ErrorTest"};
	ASSERT_STREQ("Testing what in ErrorTest", error.what());
	ASSERT_EQ(error.status(), PDI_OK);
}

/*
 * Name:                ErrorTest.call_constructor_empty_va_list
 *
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 *
 * Description:         Test calls Error constructor with empty va_list
 *                      and checks what() and status() values.
 *
 */
TEST(ErrorTest, call_constructor_empty_va_list)
{
	Error error{PDI_OK, "This is some text."};
	ASSERT_STREQ("This is some text.", error.what());
	ASSERT_EQ(error.status(), PDI_OK);
}
