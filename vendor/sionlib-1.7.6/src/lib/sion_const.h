/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

/*!
 * \file
 *
 * SIONlib constants
 */

#ifndef SION_SION_CONST_H
#define SION_SION_CONST_H

/* return codes for functions signaling success */
#define SION_SUCCESS 1
#define SION_NOT_SUCCESS 0

/* return codes for functions signaling success (used in for callback functions) */
#define SION_STD_SUCCESS 0
#define SION_STD_NOT_SUCCESS 1

/* return codes for functions returning identifier (sid)  */
#define SION_SMALLEST_VALID_ID 0
#define SION_ID_UNDEF -1
#define SION_ID_NOT_VALID -1
#define SION_UNKNOWN -1
#define SION_AUTOMATIC -1

/* return codes for functions returning size information */
#define SION_SIZE_NOT_VALID -1

/* return codes for functions wrapping fwrite/fread */
#define SION_ANSI_SIZE_NOT_VALID 0


/* SION version information */
#define SION_MAIN_VERSION 1
#define SION_SUB_VERSION 7
#define SION_VERSION_PATCHLEVEL 6

/* file format version history:
   3 -> startpointer are calculated by chunksize 
        and fsblksize, --> alignment 
   4 -> chunksize will be expanded for aligning
        up to sionlib version 1.4 
   5 -> able to store key value data */
#define SION_FILEFORMAT_VERSION 5

#ifndef GIT_REV
/* include Makefile.gitrev in Makefile and add
     CFLAGS += -DGIT_REV='"$(GIT_REV)"'
   to enable GIT_REV */
#define GIT_REV "Unknown"
#endif
#define SION_SVN_VERSION GIT_REV
#define SION_GIT_VERSION GIT_REV

#define SION_LVERSION_PREFIX ""

/* SION constants */
#define SION_CURRENT_RANK        -101  /*!< Alias for the current rank */
#define SION_CURRENT_BLK         -102  /*!< Alias for the current block */
#define SION_CURRENT_CHUNK       -102  /*!< Alias for the current block */
#define SION_CURRENT_BLOCK       -102  /*!< Alias for the current block */
#define SION_CURRENT_POS         -103  /*!< Alias for the current position in the current block */
#define SION_ABSOLUTE_POS        -104  /*!< Alias for the flag to specify in sion_seek an absolute position in bytestream */
#define SION_END_POS             -105  /*!< Flag in sion_seek for a position relative to end */

/* for sion_dup */
#define SION_DUP_ALL             -201  /*!< dup meta data for all ranks&keys */
#define SION_DUP_RANK            -202  /*!< dup meta data only for one rank and all keys */
#define SION_DUP_RANK_KEY        -203  /*!< dup meta data only for one rank and one key */

/* for key value */
#define SION_KEYVAL_NONE            50          /*!< no Key-Value Pairs in Chunks */
#define SION_KEYVAL_INLINE          51          /*!< use inline records to store key-value */
#define SION_KEYVAL_META            52          /*!< use meta data block to store keys/len */
#define SION_KEYVAL_HASH            53          /*!< use hash data structure to store key-value */
#define SION_KEYVAL_UNKNOWN         54          /*!< type UNKNOWN */
#define SION_KEYVAL_NOTSET          55          /*!< no Key-Value Pairs in Chunks */

/* for callback routines of generic interface */
#define _SION_INT32     10
#define _SION_INT64     11
#define _SION_CHAR      12


/* role of task, for sion_get_io_info  */
#define SION_ROLE_NONE        0
#define SION_ROLE_COLLECTOR   1
#define SION_ROLE_SENDER      2
#define SION_ROLE_WRITER      4
#define SION_ROLE_NOWRITER    8
#define SION_ROLE_READER      16
#define SION_ROLE_NOREADER    32
#define SION_ROLE_COLLECTOR_READER   64

/* flag for for sion_get_io_info_spec  */
#define SION_GET_IO_INFO_FLAG_NONE        0

#endif
