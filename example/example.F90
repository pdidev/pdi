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

program PDI_example_f90

  use MPI
  use paraconf
  use PDI

  implicit none
  

  character(len=512) :: strbuf
  type(PC_tree_t),target :: conf
  integer,target :: err, main_comm, psize_1d, pcoord_1d, dsize(2), psize(2)
  integer,target :: cart_comm, pcoord(2), next_reduce, ii, rem_iter
  integer,pointer :: iptr, iaptr(:)
  logical,target :: cart_period(2), keep_running
  real(8),target :: duration, start, local_time, global_time
  real(8),pointer :: cur(:,:), next(:,:), tmp(:,:)
  
  call MPI_init(err)

  if ( command_argument_count() /= 1 ) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  endif
  
  call get_command_argument(1, strbuf)
  call PC_parse_path(strbuf, conf)

  main_comm = MPI_COMM_WORLD
  call PDI_init(PC_get(conf, ".pdi"))

  call MPI_Comm_size(main_comm, psize_1d, err)
  call MPI_Comm_rank(main_comm, pcoord_1d, err)

  call PC_int(PC_get(conf,".datasize[0]"), dsize(1))
  call PC_int(PC_get(conf,".datasize[1]"), dsize(2))
  
  call PC_int(PC_get(conf,".parallelism.height"), psize(1))
  call PC_int(PC_get(conf,".parallelism.width"), psize(2))
  
  call PC_double(PC_get(conf,".duration"), duration)

  ! get local & add ghosts to sizes
  if ( mod(dsize(1), psize(1)) /= 0 ) then
    print *, "Invalid height, pheight: ", dsize(1), psize(1)
    stop
  endif
  if ( mod(dsize(2), psize(2)) /= 0 ) then
    print *, "Invalid width, pwidth: ", dsize(2), psize(2)
    stop
  endif
  dsize = dsize/psize + 2

  if ( psize(2)*psize(1) /= psize_1d ) then
    print *, "Invalid pwidth, pheight, size: ", psize(2), psize(1), psize_1d
    stop
  endif

  cart_period = (/ .FALSE., .FALSE. /)
  call MPI_Cart_create(main_comm, 2, psize, cart_period, .TRUE., &
      cart_comm, err)
  call MPI_Cart_coords(cart_comm, pcoord_1d, 2, pcoord, err)
  
  ! passing target argument to pointer dummy argument is F2008, not well supported
  iaptr => dsize;  call PDI_expose("dsize",  iaptr, PDI_OUT)
  iaptr => psize;  call PDI_expose("psize", iaptr, PDI_OUT)
  iaptr => pcoord; call PDI_expose("pcoord", iaptr, PDI_OUT)

  allocate( cur(dsize(2), dsize(1)) )
  allocate( next(dsize(2), dsize(1)) )

  call init(pcoord, cur)

  call PDI_event("main_loop")
  start = MPI_Wtime()
  next_reduce = 0
  keep_running = .TRUE.
  ii = 0
  do while( keep_running )
    call PDI_transaction_begin("newiter")
    iptr => ii; call PDI_expose("iter", iptr, PDI_INOUT)
    call PDI_expose("main_field", cur, PDI_INOUT)
    call PDI_transaction_end()
    
    call iter(cur, next)
    call exchange(cart_comm, next)
    tmp => cur; cur => next; next => tmp
    
    if ( ii >= next_reduce ) then
      local_time = MPI_Wtime()-start
      call MPI_Allreduce(local_time, global_time, 1, MPI_DOUBLE, MPI_MAX, &
          main_comm, err)
      if ( global_time >= duration ) then
        if (0==pcoord_1d) then
          print '("iter=",I7,"; time=",F7.3,"; STOP!!!")', ii, global_time
        endif
        keep_running = .FALSE.
      else
        rem_iter = .9 * (duration-global_time) * (ii+1) / (global_time+0.1)
        if ( rem_iter < 1 ) rem_iter = 1
        next_reduce = ii + rem_iter
        if (0==pcoord_1d) then
          print '("iter=",I7,"; time=",F7.3,"; next_reduce=",I7)', ii, &
              global_time, next_reduce
        endif
      endif
    endif
    ii = ii+1
  enddo
  
  call PDI_event("finalization")
  iptr => ii
  call PDI_expose("iter", iptr, PDI_OUT)
  call PDI_expose("main_field", cur, PDI_OUT)

  call PDI_finalize()

  call PC_tree_destroy(conf)

  deallocate(cur)
  deallocate(next)


  call MPI_finalize(err)

contains

  subroutine init( pcoord, dat )
    integer, intent(IN) :: pcoord(2)
    real(8), intent(INOUT) :: dat(:,:)

    integer :: yy, xx

    do xx = lbound(dat, 1), ubound(dat, 1)
      do yy = lbound(dat, 2), ubound(dat, 2)
        dat(xx,yy) = 0
      enddo
    enddo
    if ( pcoord(1) == 0 ) then
      do yy = lbound(dat, 2), ubound(dat, 2)
        dat(lbound(dat, 1),yy) = 1000000
      enddo
    endif

  endsubroutine

  subroutine iter( cur, next )
    real(8), intent(INOUT) :: cur(:,:)
    real(8), intent(INOUT) :: next(:,:)

    integer :: xx, yy

    do xx = lbound(cur, 1), ubound(cur, 1)
      next(xx, lbound(next, 2)) = cur(xx, lbound(next, 2))
    enddo
    do yy = lbound(cur, 2)+1, ubound(cur, 2)-1
      next(lbound(next, 1), yy) = cur(lbound(next, 1), yy)
      do xx = lbound(cur, 1)+1, ubound(cur, 1)-1
        next(xx,yy) = &
              (cur(xx,yy)   *.5) &
            + (cur(xx-1,yy) *.125) &
            + (cur(xx+1,yy) *.125) &
            + (cur(xx,yy-1) *.125) &
            + (cur(xx,yy+1) *.125)
      enddo
      next(ubound(next, 1), yy) = cur(ubound(next, 1), yy)
    enddo
    do xx = lbound(cur, 1), ubound(cur, 1)
      next(xx, ubound(next, 2)) = cur(xx, ubound(next, 2))
    enddo

  endsubroutine

  subroutine exchange( cart_com, cur )
    integer :: cart_com
    real(8), intent(INOUT) :: cur(:,:)

    integer :: err, rank_source, rank_dest, xlb, ylb, xub, yub
    integer :: status(MPI_STATUS_SIZE)
    integer, save :: column, row
    logical, save :: initialized = .FALSE.

    if ( initialized .eqv. .FALSE. ) then
      call MPI_Type_vector(dsize(1)-2, 1, dsize(2), MPI_DOUBLE, column, err)
      call MPI_Type_commit(column, err)
      call MPI_Type_contiguous(dsize(2)-2, MPI_DOUBLE, row, err)
      call MPI_Type_commit(row, err)
      initialized = .TRUE.
    endif

    xlb = lbound(cur, 1)
    ylb = lbound(cur, 2)
    xub = ubound(cur, 1)
    yub = ubound(cur, 2)

    ! send to the right
    call MPI_Cart_shift(cart_com, 1, 1, rank_source, rank_dest, err)
    call MPI_Sendrecv( &
        ! send column before ghost
        cur(xub-1, ylb+1), 1, column, rank_dest,   100, &
        ! receive 1st column (ghost)
        cur(xlb,   ylb+1), 1, column, rank_source, 100, &
        cart_com, status, err)

    ! send to the left
    call MPI_Cart_shift(cart_com, 1, -1, rank_source, rank_dest, err)
    call MPI_Sendrecv( &
        ! send column after ghost
        cur(xlb+1, ylb+1), 1, column, rank_dest,   100, &
        ! receive last column (ghost)
        cur(xub,   ylb+1), 1, column, rank_source, 100, &
        cart_com, status, err)

    ! send down
    call MPI_Cart_shift(cart_com, 0, 1, rank_source, rank_dest, err)
    call MPI_Sendrecv( &
        ! send row before ghost
        cur(xlb+1, yub-1), 1, row, rank_dest,   100, &
        ! receive 1st row (ghost)
        cur(xlb+1, ylb  ), 1, row, rank_source, 100, &
        cart_com, status, err)

    ! send up
    call MPI_Cart_shift(cart_com, 0, -1, rank_source, rank_dest, err)
    call MPI_Sendrecv( &
        ! send column after ghost
        cur(xlb+1, ylb+1), 1, row, rank_dest,   100, &
        ! receive last column (ghost)
        cur(xlb+1, yub  ), 1, row, rank_source, 100, &
        cart_com, status, err)
  endsubroutine

endprogram
