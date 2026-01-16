!******************************************************************************
! Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies 
! alternatives (CEA)
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
!******************************************************************************

!> Status of function execution
enum, bind(C)
  enumerator :: PC_OK = 0            ! No error
  enumerator :: PC_INVALID_PARAMETER ! A parameter value is invalid
  enumerator :: PC_INVALID_NODE_TYPE ! Unexpected type found for a node
  enumerator :: PC_NODE_NOT_FOUND    ! The requested node doesn exist in the tree
  enumerator :: PC_INVALID_FORMAT    ! The provided input is invalid
  enumerator :: PC_SYSTEM_ERROR      ! An error occured with the system
end enum


integer, parameter :: PC_ERRMSG_MAXLENGTH = 1024


!> Error handlers
!! \{
type(PC_errhandler_t) :: PC_ASSERT_HANDLER
common /PC_ASSERT_HANDLER/ PC_ASSERT_HANDLER
bind(C, name = "PC_ASSERT_HANDLER") :: /PC_ASSERT_HANDLER/
type(PC_errhandler_t) ::   PC_NULL_HANDLER
common /PC_NULL_HANDLER/ PC_NULL_HANDLER
bind(C, name = "PC_NULL_HANDLER") :: /PC_NULL_HANDLER/
!! \}
