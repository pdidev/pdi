!*******************************************************************************
! Copyright (C) 2023 Commissariat a l'energie atomique et aux energies alternatives (CEA)
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! * Redistributions of source code must retain the above copyright
!   notice, this list of conditions and the following disclaimer.
! * Redistributions in binary form must reproduce the above copyright
!   notice, this list of conditions and the following disclaimer in the
!   documentation and/or other materials provided with the distribution.
! * Neither the name of CEA nor the names of its contributors may be used to
!   endorse or promote products derived from this software without specific 
!   prior written permission.
!
! THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
! IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
! FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
! AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
! LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
! OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
! THE SOFTWARE.
!******************************************************************************/

program main
    use PDI
    use paraconf
    implicit none

    ! character(4096) :: CONFIG_YAML
    ! integer, parameter :: str_len = len(CONFIG_YAML)
    character(len=512) :: strbuf
    type(PC_tree_t) :: conf
    ! Scalars
    ! uint8_t not directly available, using the smallest integer kind
    integer(1) :: var_uint8_t
    integer(2) :: var_uint16_t
    integer(4) :: var_uint32_t
    integer(8) :: var_uint64_t
    integer(1) :: var_int8_t
    integer(2) :: var_int16_t
    integer(4) :: var_int32_t
    integer(8) :: var_int64_t
    real :: var_float
    real(8) :: var_double
    character(1) :: var_char


    ! Define the CONFIG_YAML string
    ! CONFIG_YAML = &
    !     "pdi:" // &
    !     "  data:" // &
    !     "    var_uint8_t: uint8_t" // &
    !     "    var_uint16_t: uint16_t" // &
    !     "    var_uint32_t: uint32_t" // &
    !     "    var_uint64_t: uint64_t" // &
    !     "    var_int8_t: int8_t" // &
    !     "    var_int16_t: int16_t" // &
    !     "    var_int32_t: int32_t" // &
    !     "    var_int64_t: int64_t" // &
    !     "    var_float: float" // &
    !     "    var_double: double" // &
    !     "    var_char: char" // &
    !     "" // &
    !     "  plugins:" // &
    !     "    json:" // &
    !     "      - file : json_01_check_datatypes.json" // &
    !     "        write : [var_uint8_t, var_uint16_t, var_uint32_t, var_uint64_t]" //&
    !     "        write : [var_int8_t, var_int16_t, var_int32_t, var_int64_t, var_float, var_double, var_char]"

    ! Parse the YAML string
    ! call PC_parse_string(CONFIG_YAML, conf)


    if (command_argument_count() < 1) then
        call get_command_argument(0, strbuf)
        print '("Usage: ",A," <config_file>")', trim(strbuf)
        stop
    endif
  
    call get_command_argument(1, strbuf)
    call PC_parse_path(strbuf, conf)

    ! Initialize PDI
    call PDI_init(PC_get(conf, ".pdi"))

    ! Initialize scalar variables
    var_uint8_t = 1
    var_uint16_t = 2
    var_uint32_t = 3
    var_uint64_t = 4
    var_int8_t = 5
    var_int16_t = 6
    var_int32_t = 7
    var_int64_t = 8
    var_float = 9.0
    var_double = 10.0
    var_char = 'a'

    ! Expose scalar variables
    call PDI_expose("var_uint8_t", var_uint8_t, PDI_OUT)
    call PDI_expose("var_uint16_t", var_uint16_t, PDI_OUT)
    call PDI_expose("var_uint32_t", var_uint32_t, PDI_OUT)
    call PDI_expose("var_uint64_t", var_uint64_t, PDI_OUT)
    call PDI_expose("var_int8_t", var_int8_t, PDI_OUT)
    call PDI_expose("var_int16_t", var_int16_t, PDI_OUT)
    call PDI_expose("var_int32_t", var_int32_t, PDI_OUT)
    call PDI_expose("var_int64_t", var_int64_t, PDI_OUT)
    call PDI_expose("var_float", var_float, PDI_OUT)
    call PDI_expose("var_double", var_double, PDI_OUT)
    call PDI_expose("var_char", var_char, PDI_OUT)

    ! Finalize PDI
    call PDI_finalize()

end program main