#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

# Add the plugin path to PDI_PLUGIN_PATH & add flowVR python path
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_flowvr_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_flowvr_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
set(PYTHONPATH \"\$ENV{PYTHONPATH}\")\n
if(\"x\${PYTHONPATH}x\" STREQUAL xx)\n
set(ENV{PYTHONPATH} \"${pdi_flowvr_plugin_SOURCE_DIR}/tools\")\n
else()\n
set(ENV{PYTHONPATH} \"${pdi_flowvr_plugin_SOURCE_DIR}/tools:\${PYTHONPATH}\")\n
endif()
"
)

add_subdirectory(01_Send_recv_msg/)
add_subdirectory(02_Stamps/)
add_subdirectory(03_Event_button/)
add_subdirectory(04_Event_mouse/)
add_subdirectory(05_Chunks/)
add_subdirectory(06_Abort_on_fin/)
add_subdirectory(07_Copy_send_recv/)
add_subdirectory(08_Wait_event/)

configure_file(run_flowvr_test.sh run_flowvr_test.sh @ONLY)
