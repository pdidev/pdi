#include <gtest/gtest.h>
#include <gmock/gmock.h>

#include <pdi.h>
#include <pdi/status.h>

TEST(ErrorTest, what)
{
    PDI::Error error(PDI_OK, "This is some error.");
    ASSERT_STREQ("This is some error.", error.what());
}
