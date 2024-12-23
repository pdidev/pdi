!*******************************************************************************
! Copyright (C) 2021 Centre national de la recherche scientifique (CNRS)
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
! ******************************************************************************/

subroutine assert_share_success_
  use pdi
  implicit none

  integer, pointer               :: p_val
  character(LEN=32)              :: msg
  character, pointer, contiguous :: p_msg(:)
  integer                        :: msg_ranks(1)
  integer                        :: i
  call pdi_access("value", p_val, pdi_in)
  ! assert equality
  if (p_val .ne. 42) then
     write(*,*) 'failed sharing async int value : PDI value &
          different than fortran value'
     call exit(1)
  endif
  call pdi_release("value")  
  
  msg_ranks(1) = 32
  call PDI_access("message", p_msg, pdi_in, msg_ranks)
  !copy char array into scalar string
  do i = 1, 32
     msg(i:i) = p_msg(i)
  end do
  ! assert equality
  if (trim(msg) .ne. "Watermelon is the tastiest fruit") then
     write(*,*) 'failed sharing asynchronous char array : PDI &
          value different than fortran value'     
     call exit(1)
  endif  
  call pdi_release("message")
  
  return
end subroutine assert_share_success_


program async
  use paraconf
  use pdi
  implicit none
  
  character(len=512)      :: strbuf
  type(pc_tree_t),target  :: conf
  integer                 :: my_value
  character(LEN=32)       :: secret_message
  integer, target         :: my_array(10,2)
  integer, pointer        :: p_array(:) ! without the contiguous attribute 
  integer, pointer, contiguous :: pdi_array(:)
  integer                      :: i
  integer                      :: size(1)
 
  
  call get_command_argument(1, strbuf)
  call pc_parse_path(strbuf, conf)
  call pdi_init(conf) 

  ! test asynchronous PDI_share
  call pdi_share("value", 42, pdi_out)      
  call pdi_share("message", "Watermelon is the tastiest fruit" , pdi_out)

  call assert_share_success_()

  call pdi_reclaim("message")
  call pdi_reclaim("value")

  ! test asynchronous PDI_expose
  
  my_array(:,1) =(/ (i, i=1,10)  /)
  my_array(:,2) =(/ (i, i=11,20)  /)

  p_array => my_array(:,2)
  
  call pdi_transaction_begin("copy_to_metadata")
  call pdi_expose("intarray", p_array , PDI_OUT)
  call pdi_transaction_end()

  size=(/10/)
  call pdi_access("intarray", pdi_array, pdi_in, size)
  ! assert equality
  if (sum(abs(p_array-pdi_array)) .ne. 0) then
     write(*,*) 'failed exposing int array : PDI array &
          different than exposed array'
     call exit(1)
  endif
  call pdi_release("intarray")  
  
  call pdi_finalize() 
end program async
