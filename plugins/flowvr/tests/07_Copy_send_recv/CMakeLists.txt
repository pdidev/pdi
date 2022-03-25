#=============================================================================
# Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

add_executable(send_copy send_copy.cxx)
target_link_libraries(send_copy PDI::PDI_C)
target_compile_features(send_copy PUBLIC cxx_std_17)
configure_file(send_copy.yml send_copy.yml COPYONLY)


add_executable(recv_copy recv_copy.cxx)
target_link_libraries(recv_copy PDI::PDI_C)
target_compile_features(recv_copy PUBLIC cxx_std_17)
configure_file(recv_copy.yml recv_copy.yml COPYONLY)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/copy_send_recv.py
	 DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Test
add_test(NAME flowvr_07_Copy_send_recv COMMAND PDI::pdirun "../run_flowvr_test.sh" "copy_send_recv")
set_property(TEST flowvr_07_Copy_send_recv PROPERTY TIMEOUT 10)
set_property(TEST flowvr_07_Copy_send_recv PROPERTY RESOURCE_LOCK flowvrd)
set_property(TEST flowvr_07_Copy_send_recv PROPERTY PROCESSORS 3)
set_property(TEST flowvr_07_Copy_send_recv PROPERTY DISABLED TRUE)
