#=============================================================================
# Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

add_executable(send_msg_fin send_msg_fin.cxx)
target_link_libraries(send_msg_fin PDI::pdi)
configure_file(send_msg_fin.yml send_msg_fin.yml COPYONLY)
set_target_properties(send_msg_fin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE)


add_executable(recv_msg_fin recv_msg_fin.cxx)
target_link_libraries(recv_msg_fin PDI::pdi)
configure_file(recv_msg_fin.yml recv_msg_fin.yml COPYONLY)
set_target_properties(recv_msg_fin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/abort_on_fin.py ${CMAKE_CURRENT_SOURCE_DIR}/test.sh
	 DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Test
add_test(NAME flowvr_06_Abort_on_fin COMMAND "test.sh")
set_property(TEST flowvr_06_Abort_on_fin PROPERTY TIMEOUT 5)

