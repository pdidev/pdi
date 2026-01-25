! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

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
