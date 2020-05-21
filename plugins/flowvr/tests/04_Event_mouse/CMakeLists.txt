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

add_executable(send_mouse send_mouse.cxx)
target_link_libraries(send_mouse PDI::PDI_C)
configure_file(send_mouse.yml send_mouse.yml COPYONLY)
set_target_properties(send_mouse PROPERTIES
		CXX_STANDARD 14
		CXX_STANDARD_REQUIRED TRUE)


add_executable(recv_mouse recv_mouse.cxx)
target_link_libraries(recv_mouse PDI::PDI_C)
configure_file(recv_mouse.yml recv_mouse.yml COPYONLY)
set_target_properties(recv_mouse PROPERTIES
		CXX_STANDARD 14
		CXX_STANDARD_REQUIRED TRUE)

file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/event_mouse.py ${CMAKE_CURRENT_SOURCE_DIR}/test.sh
	 DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

# Test
add_test(NAME flowvr_04_Event_mouse COMMAND "test.sh")
set_property(TEST flowvr_04_Event_mouse PROPERTY TIMEOUT 5)

