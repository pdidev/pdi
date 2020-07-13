#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright
#   notice, this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright
#   notice, this list of conditions and the following disclaimer in the
#   documentation and/or other materials provided with the distribution.
# * Neither the name of CEA nor the names of its contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written permission.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
# THE SOFTWARE.
#=============================================================================

cmake_minimum_required(VERSION 3.10)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

add_executable(serialize_01_C serialize_test_01.c)
target_link_libraries(serialize_01_C PDI::PDI_C)
add_test(NAME serialize_01_C COMMAND "$<TARGET_FILE:serialize_01_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_01.yml")

add_executable(serialize_02_C serialize_test_02.c)
target_link_libraries(serialize_02_C PDI::PDI_C)
add_test(NAME serialize_02_C COMMAND "$<TARGET_FILE:serialize_02_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_02.yml")

add_executable(serialize_03_C serialize_test_03.c)
target_link_libraries(serialize_03_C PDI::PDI_C)
add_test(NAME serialize_03_C COMMAND "$<TARGET_FILE:serialize_03_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_03.yml")

add_executable(serialize_04_C serialize_test_04.c)
target_link_libraries(serialize_04_C PDI::PDI_C)
add_test(NAME serialize_04_C COMMAND "$<TARGET_FILE:serialize_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_04.yml")

add_executable(serialize_05_C serialize_test_05.c)
target_link_libraries(serialize_05_C PDI::PDI_C)
add_test(NAME serialize_05_C COMMAND "$<TARGET_FILE:serialize_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_05.yml")

add_executable(serialize_06_C serialize_test_06.c)
target_link_libraries(serialize_06_C PDI::PDI_C)
add_test(NAME serialize_06_C COMMAND "$<TARGET_FILE:serialize_06_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_06.yml")

add_executable(serialize_07_C serialize_test_07.c)
target_link_libraries(serialize_07_C PDI::PDI_C)
add_test(NAME serialize_07_C COMMAND "$<TARGET_FILE:serialize_07_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_07.yml")

add_executable(serialize_08_C serialize_test_08.c)
target_link_libraries(serialize_08_C PDI::PDI_C)
add_test(NAME serialize_08_C COMMAND "$<TARGET_FILE:serialize_08_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_08.yml")

add_executable(serialize_09_C serialize_test_09.c)
target_link_libraries(serialize_09_C PDI::PDI_C)
add_test(NAME serialize_09_C COMMAND "$<TARGET_FILE:serialize_09_C>" "${CMAKE_CURRENT_SOURCE_DIR}/serialize_test_09.yml")
