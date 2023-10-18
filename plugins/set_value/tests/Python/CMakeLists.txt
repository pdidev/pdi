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

cmake_minimum_required(VERSION 3.10...3.25)

find_package(Python3Path REQUIRED COMPONENTS Interpreter)
set(Python_ADDITIONAL_VERSIONS "${Python3_VERSION}" CACHE STRING "Python version found by FindPython3 for coherency")

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../../cmake/runtest-dir")

add_test(NAME set_value_01_py_share COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_01_share.py" "${CMAKE_CURRENT_SOURCE_DIR}/../set_value_test_01_share.yml")
add_test(NAME set_value_02_py_share COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_02_share.py" "${CMAKE_CURRENT_SOURCE_DIR}/../set_value_test_02_share.yml")
add_test(NAME set_value_03_py COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03.py" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03.yml")
add_test(NAME set_value_04_py COMMAND PDI::pdirun "${Python3_EXECUTABLE}" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.py" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.yml")
