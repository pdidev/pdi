************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                  **
************************************************************************
**  Copyright (c) 2008-2018                                           **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre          **
**                                                                    **
**  See the file COPYRIGHT in the package base directory for details  **
************************************************************************

**************************************************************
** Fortran 77 OpenMP interface to SIONlib                   **
**************************************************************
*
* @file sion_f77.f
*
* @brief Fortran77 interface
*
* @author Florian Janetzko
* @date 29.05.2013
*

***********************************************
* Fortran interface subroutines and functions *
***********************************************
* Subroutines (without overloading)
      SUBROUTINE FSION_PAROPEN_OMP(FNAME,FILE_MODE,
     &                             CHUNKSIZE,FSBLKSIZE,GLOBALRANK,
     &                             NEWFN,SID)
      
        IMPLICIT NONE 

        INCLUDE 'sion_f77.h'
      
        CHARACTER(LEN=*) FNAME
        CHARACTER(LEN=*) FILE_MODE
        CHARACTER(LEN=*) NEWFN
        INTEGER*8        CHUNKSIZE
        INTEGER*4        FSBLKSIZE
        INTEGER          GLOBALRANK
        INTEGER          SID
      
        CALL FSION_PAROPEN_OMP_C(FNAME,FILE_MODE,
     &                           CHUNKSIZE,FSBLKSIZE,GLOBALRANK,
     &                           NEWFN,SID)
      END 


      SUBROUTINE FSION_PARCLOSE_OMP(SID,IERR)
      
        IMPLICIT NONE 

        INCLUDE 'sion_f77.h'
      
        INTEGER          SID
        INTEGER          IERR
      
        CALL FSION_PARCLOSE_OMP_C(SID,IERR)
      END 
