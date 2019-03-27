!*****************************************************************************
!**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
!*****************************************************************************
!**  Copyright (c) 2008-2018                                                **
!**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
!**                                                                         **
!**  See the file COPYRIGHT in the package base directory for details       **
!*****************************************************************************

!*****************************************************************************
!** Module file of Fortran 90 MPI interface to SIONlib                      **
!*****************************************************************************
!*
!* @file sion_f90.f90
!*
!* @brief Fortran90 interface
!*
!* @author Florian Janetzko
!* @date 05.07.2013
!* @date 05.06.2014 interface for collective routines added
!*
module sion_f90_mpi
  use sion_f90

  implicit none

!************************************************************
!* Explicit interfaces to C void functions with overloading *
!************************************************************
  interface fsion_coll_fwrite_mpi
   module procedure fsion_coll_fwrite_mpi_integer
   module procedure fsion_coll_fwrite_mpi_integer8
   module procedure fsion_coll_fwrite_mpi_real
   module procedure fsion_coll_fwrite_mpi_double_precision
   module procedure fsion_coll_fwrite_mpi_complex
   module procedure fsion_coll_fwrite_mpi_logical
   module procedure fsion_coll_fwrite_mpi_character
  end interface fsion_coll_fwrite_mpi

  interface fsion_coll_fread_mpi
    module procedure fsion_coll_fread_mpi_integer
    module procedure fsion_coll_fread_mpi_integer8
    module procedure fsion_coll_fread_mpi_real
    module procedure fsion_coll_fread_mpi_double_precision
    module procedure fsion_coll_fread_mpi_complex
    module procedure fsion_coll_fread_mpi_logical
    module procedure fsion_coll_fread_mpi_character
  end interface fsion_coll_fread_mpi

!***********************************************
!* Fortran interface subroutines and functions *
!***********************************************
contains
! Subroutines (without overloading)
  subroutine fsion_paropen_mpi(fname,file_mode,nfiles,fgcomm,flcomm,chunksizes,fsblksize,&
 &                             globalrank,newfname,sid)

    implicit none 

    character(len=*), intent(in)    :: fname
    character(len=*), intent(in)    :: file_mode
    integer, intent(in)             :: nfiles
    integer, intent(in)             :: fgcomm
    integer, intent(inout)          :: flcomm
    integer*8, intent(inout)        :: chunksizes
    integer*4, intent(inout)        :: fsblksize
    integer, intent(in)             :: globalrank
    character(len=*), intent(out)   :: newfname
    integer, intent(out)            :: sid

    call fsion_paropen_mpi_c(fname,file_mode,nfiles,fgcomm,flcomm,chunksizes,fsblksize,&
&                            globalrank,newfname,sid)
  end subroutine fsion_paropen_mpi

  subroutine fsion_parclose_mpi(sid,ierr)

    implicit none

    integer, intent(in)             :: sid
    integer, intent(out)            :: ierr

    call fsion_parclose_mpi_c(sid,ierr)
  end subroutine fsion_parclose_mpi

! Subroutines (with overloading)
! fsion_coll_fwrite_mpi
  subroutine fsion_coll_fwrite_mpi_integer(data,size,nitems,sid,rc)
    
    implicit none

    integer, intent(in)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_integer
  subroutine fsion_coll_fwrite_mpi_integer8(data,size,nitems,sid,rc)
    
    implicit none

    integer*8, intent(in)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_integer8
  subroutine fsion_coll_fwrite_mpi_real(data,size,nitems,sid,rc)

    implicit none

    real, intent(in)       :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_real
  subroutine fsion_coll_fwrite_mpi_double_precision(data,size,nitems,sid,rc)

    implicit none

    double precision, intent(in)    :: data
    integer*8, intent(in)           :: size
    integer*8, intent(in)           :: nitems
    integer, intent(in)             :: sid
    integer*8, intent(out)          :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_double_precision
  subroutine fsion_coll_fwrite_mpi_complex(data,size,nitems,sid,rc)

    implicit none

    complex, intent(in)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_complex
  subroutine fsion_coll_fwrite_mpi_logical(data,size,nitems,sid,rc)

    implicit none

    logical, intent(in)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_logical
  subroutine fsion_coll_fwrite_mpi_character(data,size,nitems,sid,rc)

    implicit none

    character, intent(in)  :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fwrite_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fwrite_mpi_character

! fsion_coll_fread_mpi
  subroutine fsion_coll_fread_mpi_integer(data,size,nitems,sid,rc)

    implicit none

    integer, intent(out)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc 

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_integer
  subroutine fsion_coll_fread_mpi_integer8(data,size,nitems,sid,rc)

    implicit none

    integer*8, intent(out)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc 

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_integer8
  subroutine fsion_coll_fread_mpi_real(data,size,nitems,sid,rc)

    implicit none

    real, intent(out)       :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_real
  subroutine fsion_coll_fread_mpi_double_precision(data,size,nitems,sid,rc)

    implicit none

    double precision, intent(out)  :: data
    integer*8, intent(in)         :: size
    integer*8, intent(in)         :: nitems
    integer, intent(in)           :: sid
    integer*8, intent(out)        :: rc

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_double_precision
  subroutine fsion_coll_fread_mpi_complex(data,size,nitems,sid,rc)

    implicit none

    complex, intent(out)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_complex
  subroutine fsion_coll_fread_mpi_logical(data,size,nitems,sid,rc)

    implicit none

    logical, intent(out)    :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_logical
  subroutine fsion_coll_fread_mpi_character(data,size,nitems,sid,rc)

    implicit none

    character, intent(out)  :: data
    integer*8, intent(in)  :: size
    integer*8, intent(in)  :: nitems
    integer, intent(in)    :: sid
    integer*8, intent(out) :: rc

    call fsion_coll_fread_mpi_c(data,size,nitems,sid,rc)

  end subroutine fsion_coll_fread_mpi_character
end module sion_f90_mpi
