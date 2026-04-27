! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

PROGRAM example
  USE ISO_C_binding
  USE paraconf
  
  IMPLICIT NONE
  
!   INCLUDE 'paraconf_f90.h'
  
  TYPE(PC_tree_t) :: conf, some_key
  INTEGER :: a_int, a_list_len, a_map_len, ii
  REAL(8) :: a_float
  CHARACTER(30) :: a_string
  LOGICAL :: a_log
  CHARACTER(LEN=PC_ERRMSG_MAXLENGTH) :: errmsg
  TYPE(PC_errhandler_t) :: errh
  CHARACTER(LEN=4096) :: infile

  if (command_argument_count() /= 1) then
    print *, "Error: expected 1 argument!"
    error stop
  endif

  call get_command_argument(1,infile)   !first, read in the two values
  
  call PC_parse_path(infile,conf)

  call PC_int(PC_get(conf,".a_int"), a_int)
  call PC_double(PC_get(conf,".a_float"), a_float)
  call PC_string(PC_get(conf,".a_string"), a_string)
  call PC_log(PC_get(conf, ".a_yes"), a_log)
  print '("a_int=",I5," a_float=",F10.1," a_string=",A30," a_yes=",L)', &
      a_int, a_float, a_string, a_log

  print '("a_list=[")'
  call PC_len(PC_get(conf,".a_list"), a_list_len)
  do ii = 0, a_list_len-1
!     call PC_int(PC_get(PC_get(conf,".a_int"), ii), a_int)
    print '("  ", I5)', a_int
  enddo
  print '("]")'

  print '("a_map={")'
  call PC_len(PC_get(conf,".a_map"), a_map_len)
  do ii = 0, a_map_len-1
!     call PC_int(PC_key(PC_get(conf,".a_int"), ii), a_string)
!     call PC_int(PC_get(PC_get(conf,".a_int"), ii), a_int)
    print '("  ", A30,"=> ", I5)', a_string, a_int
  enddo
  print '("}")'

  call PC_errhandler(PC_NULL_HANDLER, errh)
    some_key = PC_get(conf, ".some_key");
  call PC_errhandler(errh)
  if ( PC_status(some_key) > 0 ) then
    print '("config contains `some_key`")'
  else
    print '("config does not contain `some_key`")'
  endif
  
  call PC_tree_destroy(conf)

END PROGRAM example
