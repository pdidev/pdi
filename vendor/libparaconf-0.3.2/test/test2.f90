PROGRAM example
  USE paraconf

  TYPE(PC_tree_t) :: tree1
  INTEGER :: a_int
  CHARACTER(20) :: a_string
  REAL(8) :: a_float
  LOGICAL :: a_log
  INTEGER :: ierr
  CHARACTER(LEN=PC_ERRMSG_MAXLENGTH) :: errmsg

  call PC_parse_path("test2.yml",tree1)

  call PC_int(PC_get(tree1,".a_int"), a_int)
  if ( a_int /= 100 ) then
    print *, "error with a_int, ", a_int
    stop 1
  endif

  call PC_double(PC_get(tree1,".a_float"), a_float)
  if ( abs(a_float-100.1d0) > 1.0e-10 ) then
    print *, "error with a_float, ", a_float
    stop 1
  endif

  call PC_string(PC_get(tree1,".a_string"), a_string)
  if ( a_string /= "this is a string" ) then
    print *, "error with a_string, ", a_string
    stop 1
  endif

  call PC_len(PC_get(tree1, ".a_list"), a_int)
  if ( a_int /= 2 ) then
    print *, "error with a_list len, ", a_int
    stop 1
  endif

  call PC_int(PC_get(tree1, ".a_list[0]"), a_int)
  if ( a_int /= 10 ) then
    print *, "error with a_list[0], ", a_int
    stop 1
  endif

  call PC_len(PC_get(tree1, ".a_map"), a_int)
  if ( a_int /= 2 ) then
    print *, "error with a_map len, ", a_int
    stop 1
  endif

  call PC_string(PC_get(tree1,".a_map{0}"), a_string)
  if ( a_string /= "first" ) then
    print *, "error with a_map{0}, ", a_string
    stop 1
  endif

  call PC_int(PC_get(tree1, ".a_map<0>"), a_int)
  if ( a_int /= 20 ) then
    print *, "error with a_map<0>, ", a_int
    stop 1
  endif

  call PC_int(PC_get(tree1, ".another_list[1]"), a_int)
  if ( a_int /= 31 ) then
    print *, "error with another_list[1], ", a_int
    stop 1
  endif

  call PC_int(PC_get(tree1, ".another_map.second"), a_int)
  if ( a_int /= 41 ) then
    print *, "error with another_map.second, ", a_int
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_true"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_true, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_True"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_True, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_TRUE"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_TRUE, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_yes"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_yes, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_Yes"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_Yes, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_YES"), a_log)
  if ( .not. a_log ) then
    print *, "error with a_YES, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_false"), a_log)
  if ( a_log ) then
    print *, "error with a_false, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_False"), a_log)
  if ( a_log ) then
    print *, "error with a_False, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_FALSE"), a_log)
  if ( a_log ) then
    print *, "error with a_FALSE, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_no"), a_log)
  if ( a_log ) then
    print *, "error with a_no, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_No"), a_log)
  if ( a_log ) then
    print *, "error with a_No, ", a_log
    stop 1
  endif

  call PC_log(PC_get(tree1, ".a_NO"), a_log)
  if ( a_log ) then
    print *, "error with a_NO, ", a_log
    stop 1
  endif


  ! Test status values
  
  ! First we need to pass the NULL_HANDLER
  ! The default handler is the PC_ASSERT_HANDLER which aborts on error
  call PC_errhandler(PC_NULL_HANDLER)

  print *, "reading a string as a string should be ok"
  call PC_string(PC_get(tree1,".a_string"), a_string, ierr)
  if (ierr /= PC_OK) then
    print *, "error with ierr==PC_OK, got ", ierr
    stop 1
  endif

  print *, "reading a string as an int should not be ok"
  call PC_int(PC_get(tree1,".a_string"), a_int, ierr)
  if (ierr /= PC_INVALID_NODE_TYPE) then
    print *, "error with ierr==PC_INVALID_NODE_TYPE, got ", ierr
    stop 1
  endif
  
  print *, "trying to access an invalid node should not be ok"
  call PC_status(PC_get(tree1,".invalid_node"), ierr)
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    stop 1
  endif

  print *, "trying to access an invalid node should print an error message"
  call PC_status(PC_get(tree1,".invalid_node"), ierr)
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    stop 1
  endif
  call PC_errmsg(errmsg)
  if (trim(errmsg) /= ("Key `invalid_node' not found in (ROOT)")) then
    print *, "error with error message, got `", trim(errmsg),"'"
    stop 1
  endif

  print *, "trying to access an invalid node as a string should print an error message"
  call PC_string(PC_get(tree1,".invalid_node"), a_string, ierr)
  if (ierr /= PC_NODE_NOT_FOUND) then
    print *, "error with ierr==PC_NODE_NOT_FOUND, got ", ierr
    stop 1
  endif
  call PC_errmsg(errmsg)
  if (trim(errmsg) /= ("Key `invalid_node' not found in (ROOT)")) then
    print *, "error with error message, got `", trim(errmsg),"'"
    stop 1
  endif



  
  call PC_tree_destroy(tree1)

END PROGRAM example
