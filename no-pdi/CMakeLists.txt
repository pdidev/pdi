#=============================================================================
# Copyright (C) 2025 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

# cmake_minimum_required(VERSION 3.16...3.29)
# project(PDI LANGUAGES C CXX)

# include("${CMAKE_CURRENT_SOURCE_DIR}/cmake/no-pdi.cmake")



cmake_minimum_required(VERSION 3.16...3.29)
project(PDI LANGUAGES C CXX)

# Set the CMAKE_PREFIX_PATH to include the directory containing PDIConfig.cmake
set(CMAKE_PREFIX_PATH "${CMAKE_CURRENT_SOURCE_DIR}/cmake")

# Find the PDI package
find_package(PDI REQUIRED)

# Add your executable or library target
add_executable(my_executable main.cpp)

# Link the PDI library to your target
if(PDI_FOUND)
    target_link_libraries(my_executable PDI::pdi)
endif()



# # cmake_minimum_required(VERSION 3.16...3.29)
# # project(PDI LANGUAGES C CXX)

# # # Specify a custom installation directory
# # set(CMAKE_INSTALL_PREFIX "${CMAKE_CURRENT_SOURCE_DIR}/cmake/install" CACHE STRING "Installation prefix directory")

# # # Create an interface library
# # add_library(PDI_C INTERFACE)

# # # Create aliases for the interface library
# # add_library(PDI::pdi ALIAS PDI_C)
# # add_library(PDI::PDI_C ALIAS PDI_C)

# # # Specify include directories using generator expressions
# # target_include_directories(PDI_C INTERFACE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>)

# # # Export the interface library
# # export(TARGETS PDI_C FILE PDITargets.cmake)

# # # Install the interface library and configuration files
# # install(TARGETS PDI_C
# #     EXPORT PDITargets
# #     LIBRARY DESTINATION lib
# #     ARCHIVE DESTINATION lib
# #     RUNTIME DESTINATION bin
# #     INCLUDES DESTINATION include
# # )

# # install(EXPORT PDITargets
# #     FILE PDITargets.cmake
# #     DESTINATION lib/cmake/PDI
# # )

# # install(FILES
# #     "${CMAKE_CURRENT_SOURCE_DIR}/cmake/PDIConfig.cmake"
# #     "${CMAKE_CURRENT_SOURCE_DIR}/cmake/PDIConfigVersion.cmake"
# #     DESTINATION lib/cmake/PDI
# # )

# # # # Ensure that the parent directories are created relative to the installation prefix
# # # install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/cmake/"
# # #     "${CMAKE_CURRENT_SOURCE_DIR}/cmake/"
# # #     DESTINATION lib/cmake/PDI
# # # )

# # # Ensure that the parent directories are created
# # file(MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/cmake/lib/cmake/PDI")
# # file(MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/cmake/lib")
# # file(MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/cmake/include")
# # file(MAKE_DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/cmake/bin")
