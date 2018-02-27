!*******************************************************************************
! Copyright (c) 2015, Corentin Roussel - CEA (corentin.roussel@cea.fr)
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
#include "pdi.F90"

program test2
  use pdi

  implicit none

  include 'mpif.h'

  integer, pointer :: pmeta0,pmeta1,pmeta2,pmeta3,pmeta4
  integer, target :: meta0,meta1,meta2,meta3,meta4
  integer :: i, ierr,  main_comm
  integer,dimension(:), allocatable :: buf
  integer :: nbuf=1000
  double precision,target :: test_var=0.0
  double precision,pointer ::pt
  character(len=512) :: strbuf
  logical :: file_exist
  type(PC_tree_t) :: conf

  call MPI_init(ierr)

  meta0=4
  meta1=6
  meta2=2
  meta3=3
  meta4=4

  pmeta0=>meta0
  pmeta1=>meta1
  pmeta2=>meta2
  pmeta3=>meta3
  pmeta4=>meta4

  if (command_argument_count() /= 1) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  endif

  call get_command_argument(1, strbuf)
  call PC_parse_path(strbuf, conf)
  main_comm = MPI_COMM_WORLD
  call PDI_init(conf, main_comm)

  call PDI_transaction_begin("testing")
  call PDI_expose("meta0",pmeta0, PDI_OUT)
  call PDI_expose("meta1",pmeta1, PDI_OUT)
  allocate(buf(nbuf)) !! an useless buffer
  call PDI_expose("meta2",pmeta2, PDI_OUT)
  buf(:)=0
  call PDI_expose("meta3",pmeta3, PDI_OUT)
  do i=1,nbuf-1
    buf(i)=buf(i+1)+1
  enddo
  call PDI_expose("meta4",pmeta4, PDI_OUT)
  test_var=0
  pt=>test_var
  call PDI_expose("test_var",pt, PDI_OUT)
  deallocate(buf)
  test_var=1
  call PDI_transaction_end()
  call PDI_finalize()

  inquire(file="test_01_variable_6.sion", exist=file_exist) ! values(1)=6
  if( file_exist ) then
    print*, "File found."
  else
    print*, "File not found"
    call MPI_abort(MPI_COMM_WORLD, -1, ierr)
  endif ! file doesn't exist

  inquire(file="test_01_event_6.sion", exist=file_exist) ! values(1)=6
  if( file_exist ) then
    print*, "File found."
  else
    print*, "File not found"
    call MPI_abort(MPI_COMM_WORLD, -1, ierr)
  endif ! file doesn't exist

  call MPI_Finalize(ierr)

endprogram
