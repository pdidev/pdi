#=============================================================================
# Copyright (C) 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# Copyright (C) 2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_set_value_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_set_value_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

add_executable(set_value_01_C_expose set_value_test_01_expose.c)
target_link_libraries(set_value_01_C_expose PDI::PDI_C)
add_test(NAME set_value_01_C_expose COMMAND "$<TARGET_FILE:set_value_01_C_expose>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_01_expose.yml")

add_executable(set_value_01_C_share set_value_test_01_share.c)
target_link_libraries(set_value_01_C_share PDI::PDI_C)
add_test(NAME set_value_01_C_share COMMAND "$<TARGET_FILE:set_value_01_C_share>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_01_share.yml")

add_executable(set_value_01_C_set set_value_test_01_set.c)
target_link_libraries(set_value_01_C_set PDI::PDI_C)
add_test(NAME set_value_01_C_set COMMAND "$<TARGET_FILE:set_value_01_C_set>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_01_set.yml")

add_executable(set_value_02_C_expose set_value_test_02_expose.c)
target_link_libraries(set_value_02_C_expose PDI::PDI_C)
add_test(NAME set_value_02_C_expose COMMAND "$<TARGET_FILE:set_value_02_C_expose>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_02_expose.yml")

add_executable(set_value_02_C_share set_value_test_02_share.c)
target_link_libraries(set_value_02_C_share PDI::PDI_C)
add_test(NAME set_value_02_C_share COMMAND "$<TARGET_FILE:set_value_02_C_share>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_02_share.yml")

add_executable(set_value_02_C_set set_value_test_02_set.c)
target_link_libraries(set_value_02_C_set PDI::PDI_C)
add_test(NAME set_value_02_C_set COMMAND "$<TARGET_FILE:set_value_02_C_set>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_02_set.yml")

add_executable(set_value_03_C_expose set_value_test_03_expose.c)
target_link_libraries(set_value_03_C_expose PDI::PDI_C)
add_test(NAME set_value_03_C_expose COMMAND "$<TARGET_FILE:set_value_03_C_expose>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03_expose.yml")

add_executable(set_value_03_C_share_set set_value_test_03_share_set.c)
target_link_libraries(set_value_03_C_share_set PDI::PDI_C)
add_test(NAME set_value_03_C_share_set COMMAND "$<TARGET_FILE:set_value_03_C_share_set>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_03_share_set.yml")

add_executable(set_value_04 set_value_test_04.c)
target_link_libraries(set_value_04 PDI::PDI_C)
add_test(NAME set_value_04 COMMAND "$<TARGET_FILE:set_value_04>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.yml")

if("${BUILD_PYTHON}")
	add_subdirectory(Python/)
endif()
