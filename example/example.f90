

program example

  use pdi
  use mpi

  implicit none

  integer :: status, nb_iter, width, height, pheight, pwidth, main_comm, iter
  integer :: size, rank, cart_dims(2), cart_comm, cart_coord(2)
  logical :: cart_period(2)
  type(PC_tree_t_f) :: conf, treetmp
  real(8), pointer :: cur(:,:), next(:,:), tmp(:,:)

  call MPI_init(status)

  call PC_parse_path("example.yml", conf)

  call PC_get(conf,".iter", treetmp)
  call PC_int(treetmp, nb_iter)
  call PC_get(conf,".datasize[0]", treetmp)
  call PC_int(treetmp, width)
  call PC_get(conf,".datasize[1]", treetmp)
  call PC_int(treetmp, height)
  call PC_get(conf,".parallelism.height", treetmp)
  call PC_int(treetmp, pheight)
  call PC_get(conf,".parallelism.width", treetmp)
  call PC_int(treetmp, pwidth)
  
  main_comm = MPI_COMM_WORLD
  call PC_get(conf, ".pdi", treetmp)
  call PDI_init(treetmp, main_comm, status)
  
  ! get local & add ghosts to sizes
  width  = width /pwidth  + 2;
  height = height/pheight + 2;
  
  call PDI_expose("iter", iter)

  call MPI_Comm_size(main_comm, size, status)
  call MPI_Comm_size(main_comm, rank, status)

  cart_dims = (/ pwidth, pheight /)
  cart_period = (/ .FALSE., .FALSE. /)
  call MPI_Cart_create(main_comm, 2, cart_dims, cart_period, .TRUE., &
      cart_comm, status)
  call MPI_Cart_coords(cart_comm, rank, 2, cart_coord, status);

  call PDI_expose("coord", cart_coord, status)
  call PDI_expose("width", width, status)
  call PDI_expose("height", height, status)
  call PDI_expose("pwidth", pwidth, status)
  call PDI_expose("pheight", pheight, status)
  call PDI_expose("nb_iter", nb_iter, status)

  allocate( cur(width, height) )
  allocate( next(width, height) )
  
  if ( PDI_import("main_field", cur, status) ) then
    call init(cur, width, height, cart_coord(1), cart_coord(2));
  endif

  call PDI_event("main_loop", status);
  do iter = 1, nb_iter
    call PDI_expose("iter", iter, status);
    call PDI_expose("main_field", cur, status);
!     call iter(cur, next, width, height);
!     call exchange(cart_comm, next, width, height);
    tmp => cur
    cur => next
    next => tmp
  enddo
  call PDI_event("finalization", status);
  call PDI_expose("iter", iter, status);
  call PDI_expose("main_field", cur, status);

  call PDI_finalize()
  
  call PC_tree_destroy(conf);
  
  deallocate(cur);
  deallocate(next);


  call MPI_finalize(status)

contains

  subroutine init(dat, width, height, px, py)
    
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
        dat(lbound(dat, 1),yy) = 1000000;
      enddo
    endif
    
  endsubroutine

endprogram example
