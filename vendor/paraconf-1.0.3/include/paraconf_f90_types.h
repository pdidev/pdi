! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

type, bind(C) :: PC_tree_t
  integer(C_int) :: status
  type(C_ptr) :: document
  type(C_ptr) :: node
end type PC_tree_t


type, bind(C) :: PC_errhandler_t
  type(C_funptr) :: func
  type(C_ptr) :: context
end type PC_errhandler_t
