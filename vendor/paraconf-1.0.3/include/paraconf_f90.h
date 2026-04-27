! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

include 'paraconf_f90_types.h'


interface

  function PC_status(tree)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree
    integer :: PC_status
  end function PC_status
  
  
  subroutine PC_errmsg(errmsg)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    character(*), intent(OUT) :: errmsg
  end subroutine PC_errmsg
  
  
  subroutine PC_errhandler(new_handler, old_handler)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_errhandler_t), intent(IN) :: new_handler
    type(PC_errhandler_t), intent(OUT), optional :: old_handler
  end subroutine PC_errhandler
  
  
  subroutine PC_parse_path(path, tree)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    character(len = *), intent(IN) :: path
    type(PC_tree_t), intent(OUT) :: tree
  end subroutine PC_parse_path
  
  
  subroutine PC_len(tree_in, value, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree_in
    integer, intent(OUT), target :: value
    integer, intent(OUT), optional :: status
  end subroutine PC_len
  
  
  type(PC_tree_t) function PC_get(tree, index_fmt)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree
    character(len = *), intent(IN) :: index_fmt
!     type(*), optional, intent(IN) :: arguments(:)
  end function PC_get
  
  
  subroutine PC_int(tree_in, value, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree_in
    integer, intent(OUT) :: value
    integer, intent(OUT), optional :: status
  end subroutine PC_int
  
  
  subroutine PC_double(tree_in, value, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree_in
    real(8), intent(OUT) :: value
    integer, intent(OUT), optional :: status
  end subroutine PC_double
  
  
  subroutine PC_string(tree_in, value, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree_in
    character(len = *), intent(OUT) :: value
    integer, intent(OUT), optional :: status
  end subroutine PC_string
  
  
  subroutine PC_log(tree_in, value, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(IN) :: tree_in
    logical, intent(OUT) :: value
    integer, intent(OUT), optional :: status
  end subroutine PC_log
  
  
  subroutine PC_tree_destroy(tree_in, status)
    use ISO_C_binding
    include 'paraconf_f90_types.h'
    type(PC_tree_t), intent(INOUT), target :: tree_in
    integer, intent(OUT), optional :: status
  end subroutine PC_tree_destroy

end interface


include 'paraconf_f90_consts.h'
