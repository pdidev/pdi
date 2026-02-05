# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_pycall_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_pycall_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

add_executable(pycall_01_c pycall_test_01.c)
target_link_libraries(pycall_01_c PDI::PDI_C)
add_test(NAME pycall_01_c COMMAND PDI::pdirun "$<TARGET_FILE:pycall_01_c>")

add_executable(pycall_02_c pycall_test_02.c)
target_link_libraries(pycall_02_c PDI::PDI_C)
add_test(NAME pycall_02_c COMMAND PDI::pdirun "$<TARGET_FILE:pycall_02_c>")
