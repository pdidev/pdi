!*******************************************************************************
! Copyright (C) 2015-2018 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

  character(len=512) :: strbuf
  integer :: i, j, ierr,  main_comm
  integer,target :: imx=10, jmx=5, rank, nj, ni
  double precision, pointer, dimension(:,:) :: reals, cp_reals
  integer, pointer, dimension(:,:) :: values, cp_values

  integer, pointer :: iptr
  type(PC_tree_t) :: conf

  integer :: icst=-1
  double precision :: rcst=-1.0D0

  call MPI_init(ierr)

  if (command_argument_count() < 1) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  end if

  call get_command_argument(1, strbuf)
  call PC_parse_path(strbuf, conf)
  main_comm = MPI_COMM_WORLD
  call PDI_init(conf, main_comm)
  call MPI_Comm_rank(main_comm, rank, ierr)

  nj=jmx
  ni=imx

  allocate(values(ni,nj),reals(ni,nj),cp_values(ni,nj),cp_reals(ni,nj))

  do j=1,nj
    do i=1,ni
       values(i,j) = i
       reals(i,j)  = i*1.1
       cp_values(i,j) = icst
       cp_reals(i,j)  = rcst
    enddo
  enddo

  ! Set size for PDI
  iptr => ni; call PDI_expose("ni", iptr, PDI_OUT)
  iptr => nj; call PDI_expose("nj", iptr, PDI_OUT)

  ! Test that expose works
  call PDI_transaction_begin("write_data");
  call PDI_expose("reals",reals , PDI_OUT)     ! output real
  call PDI_expose("values",values , PDI_OUT) ! output integers
  call PDI_transaction_end();

  ! Exchange should also work
  call PDI_transaction_begin("read_data");
  call PDI_expose("reals" ,cp_reals, PDI_INOUT)     ! input real
  call PDI_expose("values" ,cp_values, PDI_INOUT) ! input integers
  call PDI_transaction_end();

  do j=1,nj
    do i=1,ni
       if ( (values(i,j) .ne.  cp_values(i,j)) .or. (reals(i,j) .ne. cp_reals(i,j))) then
          write(0,*) "integer (export) / integer(imported) ::", values(i,j), cp_values(i,j)
          write(0,*) "reals   (export) / reals (imported) ::", reals(i,j), cp_reals(i,j)
          call MPI_abort(MPI_COMM_WORLD, -1, ierr)
       endif
    enddo
  enddo

  call PDI_finalize()
  call MPI_Finalize(ierr)

endprogram
