************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                  **
************************************************************************
**  Copyright (c) 2008-2018                                           **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre          **
**                                                                    **
**  See the file COPYRIGHT in the package base directory for details  **
************************************************************************

**************************************************************
** Fortran 77 MPI interface to SIONlib                      **
**************************************************************
*
* @file sion_f77.f
*
* @brief Fortran77 interface
*
* @author Florian Janetzko
* @date 29.05.2013
* @date 05.06.2014 wrapper for collectives added
*

***********************************************
* Fortran interface subroutines and functions *
***********************************************
* Subroutines (without overloading)
      SUBROUTINE FSION_PAROPEN_MPI(FNAME,FILE_MODE,NFILES,
     &                             GCOMM,LCOMM,
     &                             CHUNKSIZE,FSBLKSIZE,GLOBALRANK,
     &                             NEWFN,SID)
      
        IMPLICIT NONE 

        INCLUDE 'sion_f77.h'
      
        CHARACTER(LEN=*) FNAME
        CHARACTER(LEN=*) FILE_MODE
        CHARACTER(LEN=*) NEWFN
        INTEGER          GCOMM
        INTEGER          LCOMM
        INTEGER          NFILES
        INTEGER*8        CHUNKSIZE
        INTEGER*4        FSBLKSIZE
        INTEGER          GLOBALRANK
        INTEGER          SID
      
        CALL FSION_PAROPEN_MPI_C(FNAME,FILE_MODE,NFILES,GCOMM,LCOMM,
     &                           CHUNKSIZE,FSBLKSIZE,GLOBALRANK,
     &                           NEWFN,SID)
      END 


      SUBROUTINE FSION_PARCLOSE_MPI(SID,IERR)
      
        IMPLICIT NONE 

        INCLUDE 'sion_f77.h'
      
        INTEGER          SID
        INTEGER          IERR
      
        CALL FSION_PARCLOSE_MPI_C(SID,IERR)
      END 

* Subroutines (with overloading)
* Datatype INTEGER
      SUBROUTINE FSION_COLL_FWRITE_MPI_INTEGER(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        INTEGER   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_INTEGER(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        INTEGER   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

* Datatype CHARACTER
      SUBROUTINE FSION_COLL_FWRITE_MPI_CHARACTER(DATA,SIZE,NITEMS,SID,
     &                                           RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        CHARACTER DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_CHARACTER(DATA,SIZE,NITEMS,
     &                                          SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        CHARACTER DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

* Datatype REAL
      SUBROUTINE FSION_COLL_FWRITE_MPI_REAL(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        REAL      DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_REAL(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        REAL      DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

* Datatype DOUBLE PRECISION
      SUBROUTINE FSION_COLL_FWRITE_MPI_DOUBLE(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        DOUBLE PRECISION DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_DOUBLE(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        DOUBLE PRECISION DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

* Datatype COMPLEX
      SUBROUTINE FSION_COLL_FWRITE_MPI_COMPLEX(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        COMPLEX   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_COMPLEX(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        COMPLEX   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

* Datatype LOGICAL
      SUBROUTINE FSION_COLL_FWRITE_MPI_LOGICAL(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        LOGICAL   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FWRITE_MPI(DATA,SIZE,NITEMS,SID,RC)
      END

      SUBROUTINE FSION_COLL_FREAD_MPI_LOGICAL(DATA,SIZE,NITEMS,SID,RC)

        IMPLICIT NONE

        INCLUDE 'sion_f77.h'

        LOGICAL   DATA
        INTEGER*8 SIZE
        INTEGER*8 NITEMS
        INTEGER   SID
        INTEGER*8 RC

        CALL FSION_COLL_FREAD_MPI(DATA,SIZE,NITEMS,SID,RC)
      END
