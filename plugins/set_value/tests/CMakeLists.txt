# SPDX-FileCopyrightText: 2020 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
# SPDX-FileCopyrightText: 2020-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

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

add_executable(set_value_04_C set_value_test_04.c)
target_link_libraries(set_value_04_C PDI::PDI_C)
add_test(NAME set_value_04_C COMMAND "$<TARGET_FILE:set_value_04_C>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_04.yml")

add_executable(set_value_05_C set_value_test_05.c)
target_link_libraries(set_value_05_C PDI::PDI_C)
add_test(NAME set_value_05_C COMMAND "$<TARGET_FILE:set_value_05_C>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_05.yml")

add_executable(set_value_06_C set_value_test_06.c)
target_link_libraries(set_value_06_C PDI::PDI_C)
add_test(NAME set_value_06_C COMMAND "$<TARGET_FILE:set_value_06_C>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_06.yml")

add_executable(set_value_07_C set_value_test_07.c)
target_link_libraries(set_value_07_C PDI::PDI_C)
add_test(NAME set_value_07_C COMMAND "$<TARGET_FILE:set_value_07_C>" "${CMAKE_CURRENT_SOURCE_DIR}/set_value_test_07.yml")

if("${BUILD_PYTHON}")
	add_subdirectory(Python/)
endif()
