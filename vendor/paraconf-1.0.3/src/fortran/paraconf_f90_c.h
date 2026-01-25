! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

interface

  function PC_errmsg_C() &
    bind(C, name="PC_errmsg")
    use ISO_C_binding
    implicit none
    type(C_ptr) :: PC_errmsg_C
  end function PC_errmsg_C

  function PC_errhandler_C(handler) &
    bind(C, name="PC_errhandler")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_errhandler_t), value :: handler
    type(PC_errhandler_t) :: PC_errhandler_C
  end function PC_errhandler_C

  function PC_parse_path_C(path) &
    bind(C, name="PC_parse_path")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(C_ptr), value :: path
    type(PC_tree_t) :: PC_parse_path_C
  end function PC_parse_path_C

  function PC_get_C(tree, index_fmt) &
    bind(C, name="PC_sget")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: index_fmt
    type(PC_tree_t) :: PC_get_C
  end function PC_get_C

  function PC_len_C(tree, value) &
    bind(C, name="PC_len")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: value
    integer(C_int) :: PC_len_C
  end function PC_len_C

  function PC_int_C(tree, value) &
    bind(C, name="PC_int")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: value
    integer(C_int) :: PC_int_C
  end function PC_int_C

  function PC_double_C(tree, value) &
    bind(C, name="PC_double")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: value
    integer(C_int) :: PC_double_C
  end function PC_double_C

  function PC_string_C(tree, value) &
    bind(C, name="PC_string")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: value
    integer(C_int) :: PC_string_C
  end function PC_string_C

  function PC_bool_C(tree, value) &
    bind(C, name="PC_bool")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), value :: tree
    type(C_ptr), value :: value
    integer(C_int) :: PC_bool_C
  end function PC_bool_C

  function PC_tree_destroy_C(tree) &
    bind(C, name="PC_tree_destroy")
    use ISO_C_binding
    implicit none
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree
    integer(C_int) :: PC_tree_destroy_C
  end function PC_tree_destroy_C

  subroutine free_C(ptr) &
    bind(C, name="free")
    use ISO_C_binding
    implicit none
    type(C_ptr), value :: ptr
  end subroutine free_C

end interface
