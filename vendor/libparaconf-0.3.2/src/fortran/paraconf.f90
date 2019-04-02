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

module paraconf

  use ISO_C_binding

  implicit none

  include 'paraconff.h'

end module paraconf


integer function PC_status(tree)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree

  PC_status = int(tree%status)

end function PC_status


subroutine PC_errmsg(errmsg)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  character(*), intent(OUT) :: errmsg
  character(kind = C_char), pointer, dimension(:) :: errmsg_array
  integer :: errmsg_length
  integer :: I

  errmsg_length = len(errmsg)
  errmsg = ""
  call C_F_pointer(PC_errmsg_C(), errmsg_array, [errmsg_length])
  if (associated(errmsg_array)) then

    do I = 1, errmsg_length
      if (errmsg_array(i) ==  C_NULL_CHAR) exit
      errmsg(i:i+1) = errmsg_array(i)
    end do

    ! remove new line character at the end of the string
    if (errmsg(I-1:I) ==  achar(10)) then
      errmsg(I-1:I) = ""
    end if
  end if

end subroutine PC_errmsg


subroutine PC_errhandler(new_handler, old_handler)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_errhandler_t), intent(IN) :: new_handler
  type(PC_errhandler_t), intent(OUT), optional :: old_handler

  type(PC_errhandler_t) :: tmp_handler

  if (present(old_handler)) then
    old_handler = PC_errhandler_C(new_handler)
  else
    tmp_handler = PC_errhandler_C(new_handler)
  end if

end subroutine PC_errhandler


subroutine PC_parse_path(path, tree)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  character(len = *), intent(IN) :: path
  type(PC_tree_t), intent(OUT) :: tree

  integer :: i
  character(C_char), target :: C_path(len_trim(path)+1)

  do i = 1, len_trim(path)
      C_path(i) = path(i:i)
  end do
  C_path(len_trim(path)+1) = C_NULL_CHAR

  tree = PC_parse_path_C(c_loc(C_path))

end subroutine PC_parse_path


subroutine PC_len(tree_in, value, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree_in
  integer, intent(OUT), target :: value
  integer, intent(OUT), optional :: status

  integer :: tmp


  if(present(status)) then
    status = int(PC_len_C(tree_in, c_loc(value)))
  else
    tmp = int(PC_len_C(tree_in, c_loc(value)))
  end if

end subroutine PC_len


type(PC_tree_t) function PC_get(tree, index_fmt)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree
  character(len = *), intent(IN) :: index_fmt
!   type(*), optional, intent(IN) :: arguments(:)

  integer :: i
  character(C_char), target :: C_index_fmt(len_trim(index_fmt)+1)

  do i = 1, len_trim(index_fmt)
      C_index_fmt(i) = index_fmt(i:i)
  end do
  C_index_fmt(len_trim(index_fmt)+1) = C_NULL_CHAR

  PC_get = PC_get_C(tree, c_loc(C_index_fmt))

end function PC_get


subroutine PC_int(tree_in, value, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree_in
  integer, intent(OUT) :: value
  integer, intent(OUT), optional :: status

  integer :: tmp
  integer(C_long), target :: longvalue

  if(present(status)) then
    status = int(PC_int_C(tree_in, c_loc(longvalue)))
  else
    tmp = int(PC_int_C(tree_in, c_loc(longvalue)))
  end if

  value = int(longvalue)

end subroutine PC_int


subroutine PC_double(tree_in, value, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree_in
  real(8), intent(OUT) :: value
  integer, intent(OUT), optional :: status

  integer :: tmp
  real(C_double), target :: doublevalue

  if(present(status)) then
    status = int(PC_double_C(tree_in, c_loc(doublevalue)))
  else
    tmp = int(PC_double_C(tree_in, c_loc(doublevalue)))
  end if

  value = real(doublevalue, 8)

end subroutine PC_double


subroutine PC_string(tree_in, value, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_consts.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree_in
  character(len = *), intent(OUT) :: value
  integer, intent(OUT), optional :: status

  integer :: i, tmp
  integer, dimension(1), target :: tab_lengh
  type(C_ptr), target :: C_pointer
  CHARACTER, dimension(:), pointer :: F_pointer

  value = ""
  tmp = int(PC_string_C(tree_in, c_loc(C_pointer)))
  if (present(status)) status = tmp

  if (tmp ==  PC_OK) then

    tmp = int(PC_len_C(tree_in, C_loc(tab_lengh(1))))

    call C_F_pointer(C_pointer, F_pointer, tab_lengh)
    do i = 1, tab_lengh(1)
      value(i:i) = F_pointer(i)
    end do

    do i = tab_lengh(1)+1, len(value)
      value(i:i) = ' '
    end do

    call free_C(C_pointer)
  end if
end subroutine PC_string


subroutine PC_log(tree_in, value, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(IN) :: tree_in
  logical, intent(OUT) :: value
  integer, intent(OUT), optional :: status

  integer :: tmp
  integer, target :: ilog

  ilog = 0
  if(present(status)) then
    status = int(PC_bool_C(tree_in, c_loc(ilog)))
  else
    tmp = int(PC_bool_C(tree_in, c_loc(ilog)))
  end if

  if (ilog ==  0) then
    value = .false.
  else
    value = .true.
  end if

end subroutine PC_log


subroutine PC_tree_destroy(tree_in, status)

  use ISO_C_binding

  implicit none

  include 'paraconff_types.h'
  include 'paraconff_c.h'

  type(PC_tree_t), intent(INOUT), target :: tree_in
  integer, intent(OUT), optional :: status

  integer :: tmp

  if(present(status)) then
    status = int(PC_tree_destroy_C(tree_in))
  else
    tmp = int(PC_tree_destroy_C(tree_in))
  end if

end subroutine PC_tree_destroy
