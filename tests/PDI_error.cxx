/*******************************************************************************
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#include <gtest/gtest.h>

#include <pdi/error.h>

using namespace PDI;

/*
 * Name:                ErrorTest.call_constructor_no_vargs
 *
 * Tested functions:    Error::Error()
 *                      Error::status()
 *                      Error::what()
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
 * Tested functions:    Error::Error()
 *                      Error::status()
 *                      Error::what()
 *
 * Description:         Test calls Error constructor with 2 vargs
 *                      and checks what() and status() values.
 *
 */
TEST(ErrorTest, call_constructor_vargs)
{
	Error error(PDI_UNAVAILABLE, "%d errors in %s?", 0,"ErrorTest");
	ASSERT_STREQ("0 errors in ErrorTest?", error.what());
	ASSERT_EQ(error.status(), PDI_UNAVAILABLE);
}

/*
 * Function created to support the va_list argument for Error::Error()
 */
Error va_function(const char* msg, ...)
{
	va_list ap;
	va_start(ap, msg);
	Error error(PDI_OK, msg, ap);
	va_end(ap);
	return error;
}

/*
 * Name:                ErrorTest.call_constructor_va_list
 *
 * Tested functions:    Error::Error()
 *                      Error::status()
 *                      Error::what()
 *
 * Description:         Test calls Error constructor with va_list
 *                      (containing 2 strings) and checks what() and
 *                      status() values.
 *
 */
TEST(ErrorTest, call_constructor_va_list)
{
	Error error {va_function("Testing %s in %s", "what", "ErrorTest")};
	ASSERT_STREQ("Testing what in ErrorTest", error.what());
	ASSERT_EQ(error.status(), PDI_OK);
}

/*
 * Name:                ErrorTest.call_constructor_empty_va_list
 *
 * Tested functions:    Error::Error()
 *                      Error::status()
 *                      Error::what()
 *
 * Description:         Test calls Error constructor with empty va_list
 *                      and checks what() and status() values.
 *
 */
TEST(ErrorTest, call_constructor_empty_va_list)
{
	Error error {va_function("This is some text.")};
	ASSERT_STREQ("This is some text.", error.what());
	ASSERT_EQ(error.status(), PDI_OK);
}
