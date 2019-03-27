!/****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2018                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!****************************************************************************/
      program main
      implicit none

      include 'mpif.h'
      integer*8 ierr
      integer msize, rank, i
      character(len=255) :: filename = 'test_sionfile.sion'
      character(len=255) :: newfname
      integer :: nfiles
      integer :: gComm,lComm,sid,globalrank
      integer :: fsblksize
      integer*8 :: chunksize
      real*8 :: sum,buffer(1000)
      INTEGER*8          SIZE, NELEM 

      call MPI_INIT(ierr)
      call MPI_COMM_SIZE(MPI_COMM_WORLD, msize, ierr)
      call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierr)



! step 1
      gcomm=MPI_COMM_WORLD
      globalrank=rank
      fsblksize=1000000
      chunksize=2000000
      nfiles=1
      call fsion_paropen_mpi(trim(filename),'bw',nfiles, gComm,lComm,
     $                      chunksize,fsblksize,globalrank,newfname,sid)

      buffer=1.0
      
      size=8
      nelem=1000
      call fsion_write(buffer,size,nelem,sid,ierr)
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I6)') 'on rank ',rank,': wrote=',ierr
         sum=0.0
         do i=1,1000
            sum=sum+buffer(i)
         end do
         write (6,'(A,I6,A,F16.5)') 'on rank ',rank,': sum= ',sum
      end if

      call fsion_parclose_mpi(sid,ierr)


      globalrank=-1
      fsblksize=-1
      chunksize=-1
      lcomm=-1


      call fsion_paropen_mpi(trim(filename),'br',nfiles, gComm,lComm,
     $     chunksize,fsblksize,globalrank,newfname,sid)

      buffer=0
      size=8
      nelem=1000
      call fsion_read(buffer,size,nelem,sid,ierr)
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I6)') 'on rank ',rank,': read= ',ierr
         sum=0.0
         do i=1,1000
            sum=sum+buffer(i)
         end do
         write (6,'(A,I6,A,F16.5)') 'on rank ',rank,': sum= ',sum
      end if

      call fsion_parclose_mpi(sid,ierr)
      if(rank .eq. 0) then
         write (6,'(A,I6,A,I8)') 'on rank ',rank,': nfiles=',nfiles
         write (6,'(A,I6,A,I8)') 'on rank ',rank,': chunksize=',
     $        chunksize
         write (6,'(A,I6,A,I8)') 'on rank ',rank,': fsblksize=',
     $        fsblksize
      end if
      call MPI_Finalize(ierr)
!     stop
      end program main
