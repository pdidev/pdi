#=============================================================================
# Copyright (C) 2022 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

cmake_minimum_required(VERSION 3.5)

set(OpenGL_GL_PREFERENCE LEGACY)
find_package(OpenGL REQUIRED)
find_package(GLUT REQUIRED)

add_executable(compute compute.cxx)
target_link_libraries(compute PDI::PDI_C)
configure_file(compute.yml compute.yml COPYONLY)

add_executable(capture capture.cxx)
target_link_libraries(capture PDI::PDI_C OpenGL::GL OpenGL::GLU GLUT::GLUT)
configure_file(capture.yml capture.yml COPYONLY)

add_executable(visu visu.cxx)
target_link_libraries(visu PDI::PDI_C OpenGL::GL OpenGL::GLU GLUT::GLUT)
configure_file(visu.yml visu.yml COPYONLY)


file(COPY ${CMAKE_CURRENT_SOURCE_DIR}/primes.py
     DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
