! SPDX-FileCopyrightText: 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
!
! SPDX-License-Identifier: BSD-3-Clause

program test2

  use MPI
  use PDI

  implicit none


  integer, pointer :: comm_ptr
  integer :: ierr
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
  call PDI_init(conf)
  
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
