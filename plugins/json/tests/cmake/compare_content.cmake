# SPDX-FileCopyrightText: 2023-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

if (NOT DEFINED TEST_NAME OR NOT DEFINED ACTUAL_OUTPUT_PATH OR NOT DEFINED EXPECTED_OUTPUT_PATH)
    message(FATAL_ERROR "Required variables not set. This script should be invoked using CTest.")
endif ()

file(REMOVE ${ACTUAL_OUTPUT_PATH})

# Execute test
execute_process(COMMAND "${CMAKE_BINARY_DIR}/${TEST_NAME}")

file(READ "${EXPECTED_OUTPUT_PATH}" EXPECTED_CONTENT)
file(READ "${ACTUAL_OUTPUT_PATH}" ACTUAL_CONTENT)

# Compare the actual and expected content
if (ACTUAL_CONTENT STREQUAL EXPECTED_CONTENT)
    message(STATUS "${TEST_NAME} : Produced and expected contents corresponds\nTest verify_content_${TEST_NAME} passed.")
else ()
    message(FATAL_ERROR "${TEST_NAME} : Produced and expected contents does not corresponds\nTest verify_content_${TEST_NAME} failed."
)
endif ()
