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
cmake_minimum_required(VERSION 3.10)

enable_language(C CXX)
find_package(PDI REQUIRED COMPONENTS python)

# Positive tests
add_test(NAME pos_val_pdi_config_test_01 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdi_config.py pos_val_pdi_config_test_01.yml)
configure_file(pos_val_pdi_config_test_01.yml pos_val_pdi_config_test_01.yml COPYONLY)

add_test(NAME pos_val_pdi_config_test_02 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdi_config.py pos_val_pdi_config_test_02.yml)
configure_file(pos_val_pdi_config_test_02.yml pos_val_pdi_config_test_02.yml COPYONLY)

add_test(NAME pos_val_pdi_config_test_03 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdi_config.py pos_val_pdi_config_test_03.yml)
configure_file(pos_val_pdi_config_test_03.yml pos_val_pdi_config_test_03.yml COPYONLY)

# Negative tests
add_test(NAME neg_val_pdi_config_test_01 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_01.yml)
configure_file(neg_val_pdi_config_test_01.yml neg_val_pdi_config_test_01.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_02 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_02.yml)
configure_file(neg_val_pdi_config_test_02.yml neg_val_pdi_config_test_02.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_03 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_03.yml)
configure_file(neg_val_pdi_config_test_03.yml neg_val_pdi_config_test_03.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_04 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_04.yml)
configure_file(neg_val_pdi_config_test_04.yml neg_val_pdi_config_test_04.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_05 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_05.yml)
configure_file(neg_val_pdi_config_test_05.yml neg_val_pdi_config_test_05.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_06 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_06.yml)
configure_file(neg_val_pdi_config_test_06.yml neg_val_pdi_config_test_06.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_07 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_07.yml)
configure_file(neg_val_pdi_config_test_07.yml neg_val_pdi_config_test_07.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_08 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_08.yml)
configure_file(neg_val_pdi_config_test_08.yml neg_val_pdi_config_test_08.yml COPYONLY)

add_test(NAME neg_val_pdi_config_test_09 COMMAND PDI::pdirun "${Python3_EXECUTABLE}" 
	${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdi_config.py neg_val_pdi_config_test_09.yml)
configure_file(neg_val_pdi_config_test_09.yml neg_val_pdi_config_test_09.yml COPYONLY)
