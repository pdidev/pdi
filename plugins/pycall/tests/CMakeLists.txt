#=============================================================================
# Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

set(RUNTEST_DIR "${CMAKE_SOURCE_DIR}/cmake/runtest-dir")

# Add the plugin path to LD_LIBRARY_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(LD_LIBRARY_PATH \"\$ENV{LD_LIBRARY_PATH}\")\n
if(\"x\${LD_LIBRARY_PATH}x\" STREQUAL xx)\n
set(ENV{LD_LIBRARY_PATH} \"\$<TARGET_FILE_DIR:pdi_pycall_plugin>\")\n
else()\n
set(ENV{LD_LIBRARY_PATH} \"\$<TARGET_FILE_DIR:pdi_pycall_plugin>:\${LD_LIBRARY_PATH}\")\n
endif()
"
)

add_executable(pycall_01_c pycall_test_01.c)
target_link_libraries(pycall_01_c PDI::PDI_C)
add_test(NAME pycall_01_c COMMAND PDI::pdirun "$<TARGET_FILE:pycall_01_c>")

add_executable(pycall_02_c pycall_test_02.c)
target_link_libraries(pycall_02_c PDI::PDI_C)
add_test(NAME pycall_02_c COMMAND PDI::pdirun "$<TARGET_FILE:pycall_02_c>")
