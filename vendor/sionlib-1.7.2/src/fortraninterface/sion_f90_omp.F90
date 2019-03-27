!*****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2018                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!*****************************************************************************

!*****************************************************************************
!** Module file of Fortran 90 OpenMP interface to SIONlib                   **
!*****************************************************************************
!*
!* @file sion_f90.f90
!*
!* @brief Fortran90 interface
!*
!* @author Florian Janetzko
!* @date 05.07.2013
!*
module sion_f90_omp
  use sion_f90

  implicit none

  
!***********************************************
!* Fortran interface subroutines and functions *
!***********************************************
contains
! Subroutines (without overloading)
  subroutine fsion_paropen_omp(fname,file_mode,chunksizes,fsblksize,globalranks,newfname,sid)

    implicit none 

    character(len=*), intent(in)    :: fname
    character(len=*), intent(inout) :: file_mode
    integer*8, intent(inout)        :: chunksizes
    integer*4, intent(inout)        :: fsblksize
    integer, intent(inout)          :: globalranks
    character(len=*), intent(out)   :: newfname
    integer, intent(out)            :: sid

    call fsion_paropen_omp_c(fname,file_mode,chunksizes,fsblksize,globalranks,newfname,sid)
  end subroutine fsion_paropen_omp

  subroutine fsion_parclose_omp(sid,ierr)

    implicit none

    integer, intent(in)             :: sid
    integer, intent(out)            :: ierr

    call fsion_parclose_omp_c(sid,ierr)
  end subroutine fsion_parclose_omp

end module sion_f90_omp
