#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <pdi.h>
#include <pdi/error.h>

/*
 * Name:                ErrorTest.call_constructor_no_vargs
 * 
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 * 
 * Description:         Test calls PDI::Error constructor without vargs
 *                      and checks what() and status() values.
 * 
 */
TEST(ErrorTest, call_constructor_no_vargs)
{
    PDI::Error error(PDI_OK, "No error.");
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
 * Description:         Test calls PDI::Error constructor with 2 vargs
 *                      and checks what() and status() values.
 * 
 */
TEST(ErrorTest, call_constructor_vargs)
{
    PDI::Error error(PDI_UNAVAILABLE, "%d errors in %s?", 0,"ErrorTest");
    ASSERT_STREQ("0 errors in ErrorTest?", error.what());
    ASSERT_EQ(error.status(), PDI_UNAVAILABLE);
}

/*
 * Function created to support the va_list argument for PDI::Error::Error()
 */
PDI::Error va_function(const char* msg, ...)
{
    va_list ap;
    va_start(ap, msg);
    PDI::Error error(PDI_OK, msg, ap);
    va_end(ap);
    return error;
}

/*
 * Name:                ErrorTest.call_constructor_va_list
 * 
 * Tested functions:    PDI::Error::Error()
 *                      PDI::Error::status()
 *                      PDI::Error::what()
 * 
 * Description:         Test calls PDI::Error constructor with va_list 
 *                      (containing 2 strings) and checks what() and 
 *                      status() values.
 * 
 */
TEST(ErrorTest, call_constructor_va_list)
{
    PDI::Error error {va_function("Testing %s in %s", "what", "ErrorTest")};
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
 * Description:         Test calls PDI::Error constructor with empty va_list 
 *                      and checks what() and status() values.
 * 
 */
TEST(ErrorTest, call_constructor_empty_va_list)
{
    PDI::Error error {va_function("This is some text.")};
    ASSERT_STREQ("This is some text.", error.what());
    ASSERT_EQ(error.status(), PDI_OK);
}
