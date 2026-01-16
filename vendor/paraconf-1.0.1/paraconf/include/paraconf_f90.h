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
