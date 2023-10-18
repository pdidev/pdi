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
cmake_minimum_required(VERSION 3.10...3.25)

# Positive tests
add_test(NAME pos_val_pdicfg_test_01 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdicfg_test_01.yml")
add_test(NAME pos_val_pdicfg_test_02 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdicfg_test_02.yml")
add_test(NAME pos_val_pdicfg_test_03 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/pos_val_pdicfg_test_03.yml")

# Negative tests
add_test(NAME neg_val_pdicfg_test_01 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_01.yml")
add_test(NAME neg_val_pdicfg_test_02 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_02.yml")
add_test(NAME neg_val_pdicfg_test_03 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_03.yml")
add_test(NAME neg_val_pdicfg_test_04 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_04.yml")
add_test(NAME neg_val_pdicfg_test_05 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_05.yml")
add_test(NAME neg_val_pdicfg_test_06 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_06.yml")
add_test(NAME neg_val_pdicfg_test_07 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_07.yml")
add_test(NAME neg_val_pdicfg_test_08 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_08.yml")
add_test(NAME neg_val_pdicfg_test_09 COMMAND pdicfg_validate "${CMAKE_CURRENT_SOURCE_DIR}/neg_val_pdicfg_test_09.yml")

set_tests_properties( 
	neg_val_pdicfg_test_01
	neg_val_pdicfg_test_02
	neg_val_pdicfg_test_03
	neg_val_pdicfg_test_04
	neg_val_pdicfg_test_05
	neg_val_pdicfg_test_06
	neg_val_pdicfg_test_07
	neg_val_pdicfg_test_08
	neg_val_pdicfg_test_09
	PROPERTIES
	WILL_FAIL TRUE)
