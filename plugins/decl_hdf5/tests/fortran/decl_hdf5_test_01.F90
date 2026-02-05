! SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
!
! SPDX-License-Identifier: BSD-3-Clause

program test1

  use paraconf
  use PDI

  implicit none

  integer, pointer :: pmeta0,pmeta1,pmeta2,pmeta3,pmeta4
  integer, target :: meta0,meta1,meta2,meta3,meta4
  integer :: i
  integer,dimension(:), allocatable :: buf
  integer :: nbuf=1000
  double precision,target :: test_var=0.0
  double precision,pointer ::pt
  character(len=512) :: strbuf
  logical :: file_exist
  type(PC_tree_t) :: conf

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
  call PDI_init(conf)
  
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
  
  inquire(file="6.h5", exist=file_exist) ! values(1)=6
  if( file_exist ) then
    print*, "File found."
  else
    print*, "File not found"
    stop
  endif ! file doesn't exist

endprogram  
