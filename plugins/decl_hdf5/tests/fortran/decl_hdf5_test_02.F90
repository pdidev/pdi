!*******************************************************************************
! Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

  use MPI
  use paraconf
  use PDI

  implicit none
  
  character(len=512) :: strbuf
  integer :: i, j, ierr,  main_comm 
  integer,target :: imx=10, jmx=5, input, rank, nj, ni, nig, njg
  double precision, pointer, dimension(:,:) :: reals, cp_reals
  integer, pointer, dimension(:,:) :: values, cp_values

  integer, pointer :: iptr
  type(PC_tree_t) :: conf
  logical :: have_ghost

  integer :: icst=-1
  double precision :: rcst=-1.0D0

  call MPI_init(ierr)

  if (command_argument_count() < 1) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  elseif(command_argument_count() == 2) then
    have_ghost=.true.
    nig=2;
    njg=1;
  else
    have_ghost=.false.
    nig=0;
    njg=0;
  endif
  
  call get_command_argument(1, strbuf)
  call PC_parse_path(strbuf, conf)
  main_comm = MPI_COMM_WORLD
  call PDI_init(conf)
  call MPI_Comm_rank(main_comm, rank, ierr)

  nj=jmx
  ni=imx

  allocate(values(ni,nj),reals(ni,nj),cp_values(ni,nj),cp_reals(ni,nj))

  do j=1,nj
    do i=1,ni
       values(i,j) = i+10*j
       reals(i,j)  = i*1+j*.1
       cp_values(i,j) = icst
       cp_reals(i,j)  = rcst
    enddo
  enddo

  input=0
  iptr => rank ; call PDI_expose("rank", iptr, PDI_OUT)
  iptr => input; call PDI_expose("input", iptr, PDI_OUT)
  ! Set size for PDI
  iptr => nig; call PDI_expose("ni_ghost", iptr, PDI_OUT)
  iptr => njg; call PDI_expose("nj_ghost", iptr, PDI_OUT)
  iptr => ni; call PDI_expose("ni", iptr, PDI_OUT)
  iptr => nj; call PDI_expose("nj", iptr, PDI_OUT)
  
  ! Test that export/exchange works
  iptr => input; call PDI_expose("input", iptr, PDI_OUT)  ! update metadata => HDF5 now export only
  call PDI_expose("reals",reals , PDI_OUT)     ! output real
  call PDI_expose("values",values , PDI_INOUT) ! output integers
  
  input=1
  ! Import should also work
  iptr => input ; call PDI_expose("input", iptr, PDI_OUT) ! update metadata => HDF5 now import only
  call PDI_expose("reals" ,cp_reals, PDI_IN)     ! input real 
  call PDI_expose("values" ,cp_values, PDI_INOUT) ! input integers
  
  if(.not.(have_ghost)) then
    ! So the data should be the same everywhere
    do j=1,nj
      do i=1,ni
         if ( (values(i,j) .ne.  cp_values(i,j)) .or. (reals(i,j) .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "integer (export) / integer(imported) ::", values(i,j), cp_values(i,j)
            write(0,*) i, j, "reals   (export) / reals (imported) ::", reals(i,j), cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
    enddo
  else
    do j=1+njg,nj-njg ! Should be the same inside 
      do i=1+nig,ni-nig
         if ( (values(i,j) .ne.  cp_values(i,j)) .or. (reals(i,j) .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "integer (export) / integer(imported) ::", values(i,j), cp_values(i,j)
            write(0,*) i, j, "reals   (export) / reals (imported) ::", reals(i,j), cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
    enddo
    do j=1,njg ! and should be icst/rscst outside 
      do i=1,nig
         if ( (icst .ne.  cp_values(i,j)) .or. (rcst .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "Ghost: integer (export) / integer(imported) ::",icst, cp_values(i,j)
            write(0,*) i, j, "Ghost: reals   (export) / reals (imported) ::",rcst, cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
      do i=ni-nig+1,ni
         if ( (icst .ne.  cp_values(i,j)) .or. (rcst .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "Ghost: integer (export) / integer(imported) ::",icst, cp_values(i,j)
            write(0,*) i, j, "Ghost: reals   (export) / reals (imported) ::",rcst, cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
    enddo
    do j=nj-njg+1,nj
      do i=1,nig
         if ( (icst .ne.  cp_values(i,j)) .or. (rcst .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "Ghost: integer (export) / integer(imported) ::",icst, cp_values(i,j)
            write(0,*) i, j, "Ghost: reals   (export) / reals (imported) ::",rcst, cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
      do i=ni-nig+1,ni
         if ( (icst .ne.  cp_values(i,j)) .or. (rcst .ne. cp_reals(i,j))) then 
            write(0,*) i, j, "Ghost: integer (export) / integer(imported) ::",icst, cp_values(i,j)
            write(0,*) i, j, "Ghost: reals   (export) / reals (imported) ::",rcst, cp_reals(i,j)
            call MPI_abort(MPI_COMM_WORLD, -1, ierr)
         endif
      enddo
    enddo
  endif

  call PDI_finalize()
  call MPI_Finalize(ierr)

endprogram  
