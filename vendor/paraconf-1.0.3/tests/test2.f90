! Copyright (C) The Paraconf development team, see COPYRIGHT.md file at the
!               root of the project or at https://github.com/pdidev/paraconf
! 
! SPDX-License-Identifier: MIT

program example
  use paraconf

  type(pc_tree_t) :: tree1
  integer :: a_int
  character(20) :: a_string
  real(8) :: a_float
  logical :: a_log
  integer :: ierr
  character(len=pc_errmsg_maxlength) :: errmsg
  character(len=4096) :: infile

  if (command_argument_count() /= 1) then
    print *, "Error: expected 1 argument!"
    error stop
  endif

  call get_command_argument(1,infile)   !first, read in the two values
  
  call PC_parse_path(infile, tree1)

  call PC_int(PC_get(tree1,".a_int"), a_int)
  if ( a_int /= 100 ) then
    print *, "error with a_int, ", a_int
    error stop
  endif

  call PC_double(PC_get(tree1,".a_float"), a_float)
  if ( abs(a_float-100.1d0) > 1.0e-10 ) then
    print *, "error with a_float, ", a_float
    error stop
  endif

  call PC_string(PC_get(tree1,".a_string"), a_string)
  if ( a_string /= "this is a string" ) then
    print *, "error with a_string, ", a_string
    error stop
  endif

  call PC_len(PC_get(tree1, ".a_list"), a_int)
  if ( a_int /= 2 ) then
    print *, "error with a_list len, ", a_int
    error stop
  endif

  call PC_int(PC_get(tree1, ".a_list[0]"), a_int)
  if ( a_int /= 10 ) then
    print *, "error with a_list[0], ", a_int
    error stop
  endif

  call PC_len(PC_get(tree1, ".a_map"), a_int)
  if ( a_int /= 2 ) then
    print *, "error with a_map len, ", a_int
    error stop
  endif

  call PC_string(PC_get(tree1,".a_map{0}"), a_string)
  if ( a_string /= "first" ) then
    print *, "error with a_map{0}, ", a_string
    error stop
  endif

  call PC_int(PC_get(tree1, ".a_map<0>"), a_int)
  if ( a_int /= 20 ) then
    print *, "error with a_map<0>, ", a_int
    error stop
  endif

  call PC_int(PC_get(tree1, ".another_list[1]"), a_int)
  if ( a_int /= 31 ) then
    print *, "error with another_list[1], ", a_int
    error stop
  endif

  call PC_int(PC_get(tree1, ".another_map.second"), a_int)
  if ( a_int /= 41 ) then
    print *, "error with another_map.second, ", a_int
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_true"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_true, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_True"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_True, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_TRUE"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_TRUE, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_yes"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_yes, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_Yes"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_Yes, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_YES"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_YES, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_false"), a_log)
  if ( a_log ) then
    print *, "error with a_false, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_False"), a_log)
  if ( a_log ) then
    print *, "error with a_False, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_FALSE"), a_log)
  if ( a_log ) then
    print *, "error with a_FALSE, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_no"), a_log)
  if ( a_log ) then
    print *, "error with a_no, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_No"), a_log)
  if ( a_log ) then
    print *, "error with a_No, ", a_log
    error stop
  endif

  call PC_log(PC_get(tree1, ".a_NO"), a_log)
  if ( a_log ) then
    print *, "error with a_NO, ", a_log
    error stop
  endif


  ! Test status values
  
  ! First we need to pass the NULL_HANDLER
  ! The default handler is the PC_ASSERT_HANDLER which aborts on error
  call PC_errhandler(PC_NULL_HANDLER)

  call PC_string(PC_get(tree1,".a_string"), a_string, ierr)
  if (ierr /= PC_OK) then
    print *, "error with ierr==PC_OK, got ", ierr
    error stop
  endif

  call PC_int(PC_get(tree1,".a_string"), a_int, ierr)
  if (ierr /= PC_INVALID_NODE_TYPE) then
    print *, "error with ierr==PC_INVALID_NODE_TYPE, got ", ierr
    error stop
  endif
  
  ierr = PC_status(PC_get(tree1,".invalid_node"))
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    error stop
  endif

  ierr = PC_status(PC_get(tree1,".invalid_node"))
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    error stop
  endif
  call PC_errmsg(errmsg)
  if (trim(errmsg) /= ("Key `invalid_node' not found in (ROOT)")) then
    print *, "error with error message, got `", trim(errmsg),"'"
    error stop
  endif

  call PC_string(PC_get(tree1,".invalid_node"), a_string, ierr)
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    error stop
  endif
  call PC_errmsg(errmsg)
  if (trim(errmsg) /= ("Key `invalid_node' not found in (ROOT)")) then
    print *, "error with error message, got `", trim(errmsg),"'"
    error stop
  endif



  
  call PC_tree_destroy(tree1)

end program example
