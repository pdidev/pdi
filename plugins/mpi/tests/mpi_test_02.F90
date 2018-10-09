!*******************************************************************************
! Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

program test2
  use pdi

  implicit none

  include 'mpif.h'

  integer, pointer :: comm_ptr
  integer :: main_comm, ierr
  character(len=512) :: strbuf
  type(PC_tree_t) :: conf

  call MPI_init(ierr)

  if (command_argument_count() /= 1) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  endif
  
  call get_command_argument(1, strbuf)
  call PC_parse_path(strbuf, conf)
  main_comm = MPI_COMM_WORLD
  call PDI_init(conf, main_comm)
  
  call PDI_access("MPI_COMM_WORLD_F", comm_ptr, PDI_IN);
  if ( comm_ptr .ne. MPI_COMM_WORLD ) then
    write(0,*) "`MPI_COMM_WORLD_F' != MPI_COMM_WORLD"
    call MPI_abort(MPI_COMM_WORLD, -1, ierr)
  endif
  call PDI_release("MPI_COMM_WORLD_F")

  call PDI_access("MPI_COMM_SELF_F", comm_ptr, PDI_IN);
  if ( comm_ptr .ne. MPI_COMM_SELF ) then
    write(0,*) "`MPI_COMM_SELF_F' != MPI_COMM_SELF"
    call MPI_abort(MPI_COMM_SELF, -1, ierr)
  endif
  call PDI_release("MPI_COMM_SELF_F")
  
  call PDI_access("MPI_COMM_NULL_F", comm_ptr, PDI_IN);
  if ( comm_ptr .ne. MPI_COMM_NULL ) then
    write(0,*) "`MPI_COMM_NULL_F' != MPI_COMM_NULL"
    call MPI_abort(MPI_COMM_NULL, -1, ierr)
  endif
  call PDI_release("MPI_COMM_NULL_F")
  
  call PDI_finalize()
  call MPI_Finalize(ierr)

endprogram  
