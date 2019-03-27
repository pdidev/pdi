program siontest

  use sion_f90_mpi

  implicit none

  include 'mpif.h'

  integer        :: ierr, my_rank, nranks, fsblksize, sid
  integer*8      :: chuncksize, bwrite, bwrote
  character(250) :: filename, nfilename

  integer :: lComm, gComm

  double precision, dimension(2048)::data

  call MPI_Init(ierr)

  call MPI_COMM_size(MPI_COMM_WORLD, nranks, ierr)
  call MPI_COMM_RANK(MPI_COMM_WORLD, my_rank, ierr)

  write(*,*) "mpi config: ", nranks, my_rank

  filename = "parfile_fortran.sion"

  chuncksize = 2048 * 8_8
  fsblksize  = -1

  data = 14.4

  lComm = MPI_COMM_WORLD
  gComm = MPI_COMM_WORLD

  call fsion_paropen_mpi(trim(filename), "bw", 1, gComm, lComm, chuncksize,&
       & fsblksize, my_rank, nfilename, sid)

  bwrite = chuncksize
  call fsion_write(data(1), 1_8, bwrite, sid, bwrote)

  write(*,*) "sionlib bwrote: ", bwrote


  call fsion_parclose_mpi(sid, ierr)

  call MPI_Finalize(ierr)

end program siontest
