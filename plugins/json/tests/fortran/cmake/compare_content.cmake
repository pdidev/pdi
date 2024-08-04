#--------------------------------------------------------------------------------
# Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without modification,
# are permitted provided that the following conditions are met:
#
#  * Redistributions of source code must retain the above copyright notice, this
#    list of conditions and the following disclaimer.
#  * Redistributions in binary form must reproduce the above copyright notice,
#    this list of conditions and the following disclaimer in the documentation
#    and/or other materials provided with the distribution.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
# FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
# DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
# SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
# CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
# OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
#--------------------------------------------------------------------------------


if (NOT DEFINED TEST_NAME OR NOT DEFINED ACTUAL_OUTPUT_PATH OR NOT DEFINED EXPECTED_OUTPUT_PATH OR NOT DEFINED YAML_PATH)
    message(FATAL_ERROR "Required variables not set. This script should be invoked using CTest.")
endif ()

file(REMOVE ${ACTUAL_OUTPUT_PATH})

# Execute test
execute_process(COMMAND "${CMAKE_BINARY_DIR}/${TEST_NAME}" "${YAML_PATH}")

file(READ "${EXPECTED_OUTPUT_PATH}" EXPECTED_CONTENT)
file(READ "${ACTUAL_OUTPUT_PATH}" ACTUAL_CONTENT)

# Compare the actual and expected content
if (ACTUAL_CONTENT STREQUAL EXPECTED_CONTENT)
    message(STATUS "${TEST_NAME} : Produced and expected contents corresponds\nTest verify_content_${TEST_NAME} passed.")
else ()
    message(FATAL_ERROR "${TEST_NAME} : Produced and expected contents does not corresponds\nTest verify_content_${TEST_NAME} failed."
                    #    "Actual output:\n${ACTUAL_CONTENT}\n"
                    #    "Expected output:\n${EXPECTED_CONTENT}"
)
endif ()