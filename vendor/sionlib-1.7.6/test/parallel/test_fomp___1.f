      program test_fomp___1
      implicit none

      include "sion_f77.h"

      integer*8 :: chunksize = 100
      integer*4 :: fsblksize = -1
      integer :: globalrank, sid, ierr
      character(len=1) :: file_mode = "w"
      character(len=1000) :: newfname

!$omp parallel default(none)
!$omp&  private(globalrank, newfname, sid, ierr)
!$omp&  firstprivate(file_mode, chunksize, fsblksize)
      call fsion_paropen_omp("test.dat", file_mode, chunksize,
     &  fsblksize, globalrank, newfname, sid)
      if (sid /= -1) call fsion_parclose_omp(sid, ierr)

!$omp critical
      print "(A,I1,A)", "on OMP thread ", globalrank, ": END of TEST"
!$omp end critical
!$omp end parallel
      end program
