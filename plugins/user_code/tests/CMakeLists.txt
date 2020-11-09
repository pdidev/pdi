#=============================================================================
# Copyright (C) 2015-2020 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
# * Redistributions of source code must retain the above copyright notice,
#   this list of conditions and the following disclaimer.
# * Redistributions in binary form must reproduce the above copyright notice,
#   this list of conditions and the following disclaimer in the documentation
#   and/or other materials provided with the distribution.
# * Neither the names of CEA, nor the names of the contributors may be used to
#   endorse or promote products derived from this software without specific
#   prior written  permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
# AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
# IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
# ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
# LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
# CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
# SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
# INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
# CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
# ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
# POSSIBILITY OF SUCH DAMAGE.
#=============================================================================

cmake_minimum_required(VERSION 3.5)

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_user_code_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_user_code_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

add_executable(user_code_01_c user_code_test_01.c)
target_link_libraries(user_code_01_c PDI::PDI_C)
set_target_properties(user_code_01_c PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_01_c COMMAND user_code_01_c)

add_executable(user_code_02_c user_code_test_02.c)
target_link_libraries(user_code_02_c PDI::PDI_C)
set_target_properties(user_code_02_c PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_02_c COMMAND user_code_02_c)

add_executable(user_code_03_c user_code_test_03.c)
target_link_libraries(user_code_03_c PDI::PDI_C)
set_target_properties(user_code_03_c PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_03_c COMMAND user_code_03_c)

add_executable(user_code_04_c user_code_test_04.c)
target_link_libraries(user_code_04_c PDI::PDI_C)
set_target_properties(user_code_04_c PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_04_c COMMAND user_code_04_c)

add_executable(user_code_05_c user_code_test_05.c)
target_link_libraries(user_code_05_c PDI::PDI_C)
set_target_properties(user_code_05_c PROPERTIES ENABLE_EXPORTS TRUE)
add_test(NAME user_code_05_c COMMAND user_code_05_c)
