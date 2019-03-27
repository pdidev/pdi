program mandelbrot_mpi

!  use mpi
  use sion_f90_mpi

  implicit none

include 'mpif.h'

  complex*16 :: z, coord
  real*8  :: xmin, ymin                ! coordinate of lower left corner
  real*8  :: xmax, ymax                ! coordinate of upper right corner
  integer :: iiter, niter              ! maximum iterations per coordinate
  real    :: abslimit                  ! limit for distance of coords
  integer :: xmaxpoints, ymaxpoints    ! resolution in x and y 
  character*6, allocatable :: image(:,:)
  real*8  :: xstepsize, ystepsize
  integer :: x,y

  integer :: colorlimit, colors
  character*6, allocatable :: colormap(:)
  character*15 :: filename, newfname
  character*2  :: filemode,ppmtype
  integer*8    :: chunksize, size, nelem, nwrote
  integer      :: flcomm
  integer      :: nfiles, fsblksize, sid, nbytes, npoints

  integer :: ierror, myrank, nranks
  integer :: myymaxpoints

! MPI Initialization
  call MPI_INIT(ierror)
  call MPI_COMM_SIZE(MPI_COMM_WORLD,nranks,ierror)
  call MPI_COMM_RANK(MPI_COMM_WORLD,myrank,ierror)
  if (myrank == 0) then
    write(*,*)
    write(*,'(A)') 'on MPI rank 0:  ==============================='
    write(*,'(A)') 'on MPI rank 0:  Calculations of mandelbrot set.'
    write(*,'(A)') 'on MPI rank 0:  ==============================='
    write(*,'(A,I4,A)') 'on MPI rank 0:  Running with ', nranks,' ranks.'
    write(*,*)
  endif
 
! Initialization of values
! Define color map
  colorlimit  = 1
  colors      = 6
  allocate(colormap(0:colors-1))
  colormap(0) = " 1 1 1"
  colormap(1) = " 0 0 0"
  colormap(2) = " 0 0 1"
  colormap(3) = " 1 0 1"
  colormap(4) = " 0 1 1"
  
  ppmtype = 'P3'

! Define parameters for iteration
  abslimit   =   2
  niter      = 1000
  xmaxpoints = 800
  ymaxpoints = 800
  xmin       =  -2.0
  ymin       =  -2.0
  xmax       =   2.0
  ymax       =   2.0

! Decompose lines
  myymaxpoints = ymaxpoints/nranks
  if (mod(ymaxpoints,nranks) /= 0) then
    if (myrank == nranks-1) then
      myymaxpoints = myymaxpoints + mod(ymaxpoints,nranks)
    endif
  endif
  allocate(image(xmaxpoints,myymaxpoints))

! Define output
  filename="mandelbrot.sion"

! Calculate step size
  xstepsize  = (xmax-xmin)/dble(xmaxpoints)
  ystepsize  = (ymax-ymin)/dble(ymaxpoints)

! Iterate over all coordinates
  do y = 1, myymaxpoints
    do x = 1, xmaxpoints
      coord = cmplx(xmin+dble(x-1)*xstepsize, ymin+(dble(ymaxpoints/nranks*myrank)+dble(y-1))*ystepsize, 8)
      z     = 0
      iiter = 0
      do while ((abs(z)<abslimit) .and. (iiter <= niter))
        z     = z**2 + coord
        iiter = iiter + 1 
      enddo 
      image(x,y) = colormap(mod(iiter,colors-1))
    enddo
  enddo

!  image(:,:) = colormap(myrank)

  call MPI_Barrier(MPI_COMM_WORLD,ierror)

! Open file for writing image data
  nfiles    = 1
  npoints   = myymaxpoints*xmaxpoints
  nbytes    = 6
  chunksize = npoints*nbytes
! ppm header data on rank 0
  if (myrank == 0) then
    chunksize = chunksize + 15
  endif
  fsblksize = 1000
  filemode  = 'wb'
  flcomm    = MPI_COMM_WORLD
  call fsion_paropen_mpi(trim(filename),filemode,nfiles,MPI_COMM_WORLD,flcomm,chunksize,&
&                        fsblksize,myrank,newfname,sid)

! Write image data
  if (myrank == 0) then
    size=2;nelem=1;call fsion_write(ppmtype,size,nelem,sid,nwrote)
    size=4;nelem=1;call fsion_write(xmaxpoints,size,nelem,sid,nwrote)
    size=4;nelem=1;call fsion_write(ymaxpoints,size,nelem,sid,nwrote)
    size=4;nelem=1;call fsion_write(colorlimit,size,nelem,sid,nwrote)
    size=nbytes;nelem=npoints;call fsion_write(image(1,1),size,nelem,sid,nwrote)
  else
    size=nbytes;nelem=npoints; call fsion_write(image(1,1),size,nelem,sid,nwrote)
  endif
  call fsion_parclose_mpi(sid, ierror)

! Finalize
  deallocate(image,colormap)
  call MPI_FINALIZE(ierror)

end program mandelbrot_mpi
