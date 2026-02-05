# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

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
