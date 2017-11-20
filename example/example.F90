
#include "PDI.F90"

program PDI_example_f90

  use pdi

  implicit none
  
  include 'mpif.h'

  integer,target :: status, next_reduce, width, height, pheight, pwidth, main_comm, ii
  integer,target :: size, rank, cart_dims(2), cart_comm, cart_coord(2), rem_iter, err
  integer,pointer :: iptr, iaptr(:)
  logical,target :: cart_period(2), keep_running
  type(PC_tree_t),target :: conf
  real(8),pointer :: cur(:,:), next(:,:), tmp(:,:)
  real(8),target :: local_time, global_time, duration, start
  character(len=512) :: strbuf
  
  call MPI_init(status)

  if ( command_argument_count() /= 1 ) then
    call get_command_argument(0, strbuf)
    print '("Usage: ",A," <config_file>")', trim(strbuf)
    stop
  endif
  
  call get_command_argument(1, strbuf)
  
  call PC_parse_path(strbuf, conf)

  call PC_int(PC_get(conf,".datasize[0]"), width)
  call PC_int(PC_get(conf,".datasize[1]"), height)
  call PC_int(PC_get(conf,".parallelism.height"), pheight)
  call PC_int(PC_get(conf,".parallelism.width"), pwidth)
  call PC_double(PC_get(conf,".duration"), duration)

  main_comm = MPI_COMM_WORLD
  call PDI_init(PC_get(conf, ".pdi"), main_comm)

  ! get local & add ghosts to sizes
  if ( mod(width, pwidth) /= 0 ) then
    print *, "Invalid width, pwidth: ", width, pwidth
    stop
  endif
  width  = width /pwidth  + 2
  if ( mod(height, pheight) /= 0 ) then
    print *, "Invalid height, pheight: ", height, pheight
    stop
  endif
  height = height/pheight + 2

  call MPI_Comm_size(main_comm, size, status)
  call MPI_Comm_rank(main_comm, rank, status)

  if ( pwidth*pheight /= size ) then
    print *, "Invalid pwidth, pheight, size: ", pwidth, pheight, size
    stop
  endif

  cart_dims = (/ pwidth, pheight /)
  cart_period = (/ .FALSE., .FALSE. /)
  call MPI_Cart_create(main_comm, 2, cart_dims, cart_period, .TRUE., &
      cart_comm, status)
  call MPI_Cart_coords(cart_comm, rank, 2, cart_coord, status)
  
  ! passing target argument to pointer dummy argument is F2008, not well supported
  iaptr => cart_coord
  call PDI_expose("coord", iaptr)
  iptr => width
  call PDI_expose("width", iptr)
  iptr => height
  call PDI_expose("height", iptr)
  iptr => pwidth
  call PDI_expose("pwidth", iptr)
  iptr => pheight
  call PDI_expose("pheight", iptr)

  allocate( cur(width, height) )
  allocate( next(width, height) )

  call PDI_import("main_field", cur, status)
  if ( status>0 ) then
    call init(cur, width, height, cart_coord(1), cart_coord(2))
  endif

  call PDI_event("main_loop")
  start = MPI_Wtime()
  next_reduce = 0
  keep_running = .TRUE.
  ii = 0
  do while( keep_running )
    call PDI_transaction_begin("newiter")
    iptr => ii
    call PDI_expose("iter", iptr)
    call PDI_expose("main_field", cur)
    call PDI_transaction_end()
    
    call iter(cur, next)
    call exchange(cart_comm, next)
    tmp => cur; cur => next; next => tmp
    
    if ( ii >= next_reduce ) then
      local_time = MPI_Wtime()-start
      call MPI_Allreduce(local_time, global_time, 1, MPI_DOUBLE, MPI_MAX, main_comm, err)
      if ( global_time >= duration ) keep_running = .FALSE.
      rem_iter = .8 * (duration-global_time) * (ii+1) / global_time + 1
      if ( rem_iter < 1 ) rem_iter = 1
      next_reduce = ii + rem_iter
      print '("iter=",I7,"; time=",F7.3,"; next_reduce=",I7)', ii, global_time, next_reduce
    endif
    ii = ii+1
  enddo
  
  call PDI_event("finalization")
  iptr => ii
  call PDI_expose("iter", iptr)
  call PDI_expose("main_field", cur)

  call PDI_finalize()

  call PC_tree_destroy(conf)

  deallocate(cur)
  deallocate(next)


  call MPI_finalize(status)

contains

  subroutine init( dat, width, height, px, py )
    real(8), intent(INOUT) :: dat(:,:)
    integer, intent(IN) :: width, height, px, py

    integer :: yy, xx

    do xx = lbound(dat, 1), ubound(dat, 1)
      do yy = lbound(dat, 2), ubound(dat, 2)
        dat(xx,yy) = 0
      enddo
    enddo
    if ( px == 0 ) then
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

    integer :: mpi_err, rank_source, rank_dest, xlb, ylb, xub, yub
    integer :: status(MPI_STATUS_SIZE)
    integer, save :: column, row
    logical, save :: initialized = .FALSE.

    if ( initialized .eqv. .FALSE. ) then
      call MPI_Type_vector(height-2, 1, width, MPI_DOUBLE, column, mpi_err)
      call MPI_Type_commit(column, mpi_err)
      call MPI_Type_contiguous(width-2, MPI_DOUBLE, row, mpi_err)
      call MPI_Type_commit(row, mpi_err)
      initialized = .TRUE.
    endif

    xlb = lbound(cur, 1)
    ylb = lbound(cur, 2)
    xub = ubound(cur, 1)
    yub = ubound(cur, 2)

    ! send to the right
    call MPI_Cart_shift(cart_com, 0, 1, rank_source, rank_dest, mpi_err)
    call MPI_Sendrecv( &
        ! send column before ghost
        cur(xub-1, ylb+1), 1, column, rank_dest,   100, &
        ! receive 1st column (ghost)
        cur(xlb,   ylb+1), 1, column, rank_source, 100, &
        cart_com, status, mpi_err)

    ! send to the left
    call MPI_Cart_shift(cart_com, 0, -1, rank_source, rank_dest, mpi_err)
    call MPI_Sendrecv( &
        ! send column after ghost
        cur(xlb+1, ylb+1), 1, column, rank_dest,   100, &
        ! receive last column (ghost)
        cur(xub,   ylb+1), 1, column, rank_source, 100, &
        cart_com, status, mpi_err)

    ! send down
    call MPI_Cart_shift(cart_com, 1, 1, rank_source, rank_dest, mpi_err)
    call MPI_Sendrecv( &
        ! send row before ghost
        cur(xlb+1, yub-1), 1, row, rank_dest,   100, &
        ! receive 1st row (ghost)
        cur(xlb+1, ylb  ), 1, row, rank_source, 100, &
        cart_com, status, mpi_err)

    ! send up
    call MPI_Cart_shift(cart_com, 1, -1, rank_source, rank_dest, mpi_err)
    call MPI_Sendrecv( &
        ! send column after ghost
        cur(xlb+1, ylb+1), 1, row, rank_dest,   100, &
        ! receive last column (ghost)
        cur(xlb+1, yub  ), 1, row, rank_source, 100, &
        cart_com, status, mpi_err)
  endsubroutine

endprogram
