!/****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2019                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!****************************************************************************/
      program main
      implicit none
      include 'mpif.h'

      integer :: ierr, msize, rank
      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, msize, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)

! step 1
      call run_test(rank)

      call MPI_Finalize(ierr)
!     stop
      end program main

      subroutine run_test ( rank )
      implicit none
      integer rank

      include 'mpif.h'

      integer omp_get_thread_num,omp_get_num_threads
      integer :: ierr, i, tid, nthreads
      character(len=255) :: filename = 'testB.out'
      character(len=255) :: newfname
      integer :: nfiles
      integer :: gComm,lComm,sid,globalrank
      integer :: fsblksize
      integer*8 :: chunksize, size, nelem, nwrote, nread
      real*8 :: sum,buffer(1000)

!$omp parallel private(globalrank,tid,sid,buffer,sum,newfname)

      gcomm=MPI_COMM_WORLD
      globalrank=rank
      fsblksize=1000000
      chunksize=2000000
      nfiles=1
      call fsion_paropen_ompi(trim(filename),'bw',nfiles, gComm,lComm,
     $     chunksize,fsblksize,globalrank,newfname,sid)
      tid = omp_get_thread_num()
      nthreads = omp_get_num_threads()

      buffer=1111.011

      size=8
      nelem=1000
      call fsion_write(buffer,size,nelem,sid,nwrote)

!$omp master
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I6,A,I6)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': nthreads=',nthreads
         write (6,'(A,I6,A,I6,A,I6)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': wrote=',nwrote
      end if
!$omp end master

      call fsion_parclose_ompi(sid,ierr)


      globalrank=-1
      fsblksize=-1
      chunksize=-1
      lcomm=-1


      call fsion_paropen_ompi(trim(filename),'br',nfiles, gComm,lComm,
     $     chunksize,fsblksize,globalrank,newfname,sid)

      buffer=0
      sum=0
      size=8
      nelem=1000
      call fsion_read(buffer,size,nelem,sid,nread)
!$omp master
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I6,A,I6)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': read= ',nread
         do i=0,1000
            sum=sum+buffer(i)
         end do
         write (6,'(A,I6,A,I6,A,F16.5)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': sum= ',sum
      end if
!$omp end master
      call fsion_parclose_ompi(sid,ierr)
!$omp master
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I6,A,I8)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': nfiles=',nfiles
         write (6,'(A,I6,A,I6,A,I8)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': chunksize=',chunksize
         write (6,'(A,I6,A,I6,A,I8)') 'on MPI rank ',rank,
     $        ', OMP thread ',tid,': fsblksize=',fsblksize
      end if
!$omp end master

!$omp end parallel

      end
