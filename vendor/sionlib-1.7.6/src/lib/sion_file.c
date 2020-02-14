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
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <time.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <errno.h>

#if defined(_SION_SIONFWD)
#include <sionfwd/client.h>
#endif

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_file.h"

/*!\brief Create and open a new file for writing
 *
 * @param  fname               filename to use
 * @param  flags               flags
 * @param  addflags            optional additional flags
 *
 * @return file handle, or `NULL` on error
 */
_sion_fileptr   *_sion_file_open(const char *fname, unsigned int flags, unsigned int addflags)  {

  if(flags & SION_FILE_FLAG_ANSI) {
    _sion_fileptr *sion_fileptr = _sion_file_alloc_and_init_sion_fileptr();
    sion_fileptr->flags |= SION_FILE_FLAG_ANSI;

    if(flags & SION_FILE_FLAG_WRITE) {
      sion_fileptr->flags |= SION_FILE_FLAG_WRITE;

      if(flags & SION_FILE_FLAG_CREATE) {
	sion_fileptr->flags |= SION_FILE_FLAG_CREATE;
	sion_fileptr->fileptr=_sion_file_open_ansi_write_create(fname,addflags);
      } else {
	sion_fileptr->fileptr=_sion_file_open_ansi_write_existing(fname,addflags);
      }
    } else {
      sion_fileptr->flags |= SION_FILE_FLAG_READ;
      sion_fileptr->fileptr=_sion_file_open_ansi_read(fname,addflags);
    }

    /* could not open ANSI file */
    if(!sion_fileptr->fileptr) {
      free(sion_fileptr);
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"error: could not open file (ANSI), %s %d %d, aborting ...\n", fname, flags, addflags);
      return(NULL);
    }

    return(sion_fileptr);
  } else if (flags & SION_FILE_FLAG_POSIX) {
    _sion_fileptr *sion_fileptr = _sion_file_alloc_and_init_sion_fileptr();
    sion_fileptr->flags |= SION_FILE_FLAG_POSIX;

    if(flags & SION_FILE_FLAG_WRITE) {
      sion_fileptr->flags |= SION_FILE_FLAG_WRITE;
      if(flags & SION_FILE_FLAG_CREATE) {
        sion_fileptr->flags |= SION_FILE_FLAG_CREATE;
        sion_fileptr->fd=_sion_file_open_posix_write_create(fname,addflags);
    } else {
      sion_fileptr->fd=_sion_file_open_posix_write_existing(fname,addflags);
      }
    } else {
      sion_fileptr->flags |= SION_FILE_FLAG_READ;
      sion_fileptr->fd=_sion_file_open_posix_read(fname,addflags);
    }

    /* could not open POSIX file */
    if(sion_fileptr->fd<0) {
      free(sion_fileptr);
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"error: could not open file (POSIX), %s %d %d, aborting ...\n", fname, flags, addflags);
      return(NULL);
    }

    return(sion_fileptr);
#if defined(_SION_SIONFWD)
  } else if (flags & SION_FILE_FLAG_SIONFWD) {
    _sion_fileptr *sion_fileptr = _sion_file_alloc_and_init_sion_fileptr();
    sion_fileptr->flags |= SION_FILE_FLAG_SIONFWD;

    if(flags & SION_FILE_FLAG_WRITE) {
      sion_fileptr->flags |= SION_FILE_FLAG_WRITE;
      if(flags & SION_FILE_FLAG_CREATE) {
        sion_fileptr->flags |= SION_FILE_FLAG_CREATE;
        sion_fileptr->fd=_sion_file_open_sionfwd_write_create(fname,addflags);
      } else {
        sion_fileptr->fd=_sion_file_open_sionfwd_write_existing(fname,addflags);
      }
    } else {
      sion_fileptr->flags |= SION_FILE_FLAG_READ;
      sion_fileptr->fd=_sion_file_open_sionfwd_read(fname,addflags);
    }

    /* could not open SIONFWD file */
    if(sion_fileptr->fd == -1) {
      free(sion_fileptr);
      _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"error: could not open file (SIONFWD), %s %d %d, aborting ...\n", fname, flags, addflags);
      return(NULL);
    }

    return(sion_fileptr);
#endif
  } else {
    /* unknown mode? */
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_WARN,"internal error: unknown open type, %s %d %d, aborting ...\n", fname, flags, addflags);
    return(NULL);
  }
}

#define DFUNCTION "_sion_file_close"
/*!\brief Close file and destroys fileptr structure
 *
 * @param[in,out]  sion_fileptr  file handle
 *
 * @return `SION_SUCCESS` if success
 */
int _sion_file_close( _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_NOT_SUCCESS;

  int rc;
  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI) {
      _sion_file_close_ansi(sion_fileptr->second_fileptr);
      sion_fileptr->second_fileptr=NULL;
    }
    rc=_sion_file_close_ansi(sion_fileptr->fileptr);
    sion_fileptr->fileptr=NULL;
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    rc=_sion_file_close_posix(sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    rc=_sion_file_close_sionfwd(sion_fileptr->fd);
#endif
  } else {
    rc = SION_NOT_SUCCESS;
  }

  DPRINTFP((32, DFUNCTION, -1, "free now fileptr=%x %d rc=%d\n",sion_fileptr,sion_fileptr->flags,rc));
  free(sion_fileptr);

  return (rc);
}
#undef DFUNCTION

/*!\brief Write data to file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to write
 * @param  sion_fileptr         file handle
 *
 * @return returns `bytes` if the requested amount could be written, otherwise `-1`
 */
sion_int64 _sion_file_write(const void *data, sion_int64 bytes, _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_SIZE_NOT_VALID;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)
      return _sion_file_write_ansi(data,bytes,sion_fileptr->second_fileptr);
    else
      return _sion_file_write_ansi(data,bytes,sion_fileptr->fileptr);
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_write_posix(data,bytes,sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_write_sionfwd(data,bytes,sion_fileptr->fd,&sion_fileptr->position);
#endif
  } else {
    return _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_file_write: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Read data from file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to write
 * @param  sion_fileptr         file handle
 *
 * @return returns `bytes` if the requested amount could be read,
 *         on EOF, returns `n` with `0 <= n < bytes`,
 *         otherwise `-1`
 */
sion_int64 _sion_file_read(void *data, sion_int64 bytes, _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_SIZE_NOT_VALID;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)
      return _sion_file_read_ansi(data,bytes,sion_fileptr->second_fileptr);
    else
      return _sion_file_read_ansi(data,bytes,sion_fileptr->fileptr);
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_read_posix(data,bytes,sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_read_sionfwd(data,bytes,sion_fileptr->fd,&sion_fileptr->position);
#endif
  } else {
    return _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_file_read: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Get optional file system block size for a file
 *
 * @param  sion_fileptr  fileptr
 *
 * @return blocksize or `-1` if not defined
 */
long _sion_file_get_opt_blksize( _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_SIZE_NOT_VALID;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    return _sion_file_get_opt_blksize_ansi(sion_fileptr->fileptr);
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_get_opt_blksize_posix(sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_get_opt_blksize_sionfwd(sion_fileptr->fd);
#endif
  } else {
    return _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_file_get_opt_blksize: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Check if file exists  (LARGE_FILE support on BlueGene)
 *
 * @param  *fname               filename to use
 *
 * @return `0` or `1`
 */
int _sion_file_stat_file(const char *fname) {
  return _sion_file_stat_file2(fname, SION_FILE_FLAG_POSIX);
}

/*!\brief Check if file exists with appropriate low-level API
 *
 * @param  *fname               filename to use
 * @param   apiflag             indicates the low-level API to use (POSIX, ANSI, etc.)
 *
 * @return `1` if the file exists, otherwise `0`
 */
int _sion_file_stat_file2(const char *fname, unsigned int apiflag) {
  // _sion_file_stat_file above is incomplete, because it does not
  // allow selecting an appropriate low-level API like all other functions
  // in this file.
  // Unfortunately its signature is declared in the public sion_common.h
  // and thus cannot be changed without breaking backward compatibility.
  // Thus, this new function is introduced and used internally in its place.
  if ((apiflag & SION_FILE_FLAG_ANSI) || (apiflag & SION_FILE_FLAG_POSIX)) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_AIX) ||  defined(_SION_BGP) || defined(_SION_BGQ)
    struct stat sbuf;
    return 0 == stat(fname, &sbuf);
#else
#error "No platform selected."
#endif
#if defined(_SION_SIONFWD)
  } else if(apiflag & SION_FILE_FLAG_SIONFWD) {
    return sionfwd_stat_file(fname);
#endif
  } else {
    return _sion_errorprint(0,_SION_ERROR_RETURN,"_sion_file_stat_file2: cannot find valid api flag (flags=%d)\n",apiflag);
  }
}

/*!\brief Set new position in file
 *
 * @param  sion_fileptr         fileptr
 * @param  startpointer         new position
 *
 * @return new position in file or `-1` on failure
 */
sion_int64 _sion_file_set_position( _sion_fileptr *sion_fileptr, sion_int64 startpointer ) {
  if (NULL == sion_fileptr) return SION_SIZE_NOT_VALID;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
     return _sion_file_set_position_ansi(sion_fileptr->second_fileptr, startpointer);
    } else {
      return _sion_file_set_position_ansi(sion_fileptr->fileptr, startpointer);
    }
  } else  if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_set_position_posix(sion_fileptr->fd, startpointer);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    sion_fileptr->position = startpointer;
    return sion_fileptr->position;
#endif
  } else {
    return _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_file_set_position: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Get new position in file
 *
 * @param  sion_fileptr  fileptr
 *
 * @return position in file or `-1` on failure
 */
sion_int64 _sion_file_get_position( _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_SIZE_NOT_VALID;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return _sion_file_get_position_ansi(sion_fileptr->second_fileptr);
    } else {
      return _sion_file_get_position_ansi(sion_fileptr->fileptr);
    }
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_get_position_posix(sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return sion_fileptr->position;
#endif
  } else {
    return _sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_file_get_position: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Flush data to file
 *
 * @param  sion_fileptr  fileptr
 *
 * @return  `SION_SUCCESS` if ok
 */
int _sion_file_flush( _sion_fileptr *sion_fileptr ) {
  if (NULL == sion_fileptr) return SION_NOT_SUCCESS;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return _sion_file_flush_ansi(sion_fileptr->second_fileptr);
    } else {
      return _sion_file_flush_ansi(sion_fileptr->fileptr);
    }
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_flush_posix(sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_flush_sionfwd(sion_fileptr->fd);
#endif
  } else {
    return _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_file_flush: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Purge data to file
 *
 * @param  sion_fileptr  fileptr
 *
 * @return  `SION_SUCCESS` if ok
 */
int _sion_file_purge( _sion_fileptr *sion_fileptr ) {
  if( sion_fileptr == NULL ) return SION_NOT_SUCCESS;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return _sion_file_purge_ansi(sion_fileptr->second_fileptr);
    } else {
      return _sion_file_purge_ansi(sion_fileptr->fileptr);
    }
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_purge_posix(sion_fileptr->fd);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_purge_sionfwd(sion_fileptr->fd);
#endif
  } else {
    return _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_file_purge: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Set buffer of fp.
 *
 * @param  *sion_fileptr        file handle
 * @param  *buffer              pointer buffer
 * @param  *buffer_size         buffer size
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_set_buffer(_sion_fileptr *sion_fileptr, char *buffer, sion_int32 buffer_size) {
  if( sion_fileptr == NULL ) return SION_NOT_SUCCESS;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return _sion_file_set_buffer_ansi(sion_fileptr->second_fileptr, buffer, buffer_size);
    } else {
      return _sion_file_set_buffer_ansi(sion_fileptr->fileptr, buffer, buffer_size);
    }
  } else if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return _sion_file_set_buffer_posix(sion_fileptr->fd, buffer, buffer_size);
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return _sion_file_set_buffer_sionfwd(sion_fileptr->fd, buffer, buffer_size);
#endif
  } else {
    return _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"_sion_file_set_buffer: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/*!\brief Set second fileptr for file if opened with ANSI.
 *
 * @param  sion_fileptr         file handle
 * @param  fileptr              second file pointer
 *
 * @return `SION_SUCCESS` if success
 */
int _sion_file_set_second_fileptr( _sion_fileptr *sion_fileptr, FILE* fileptr) {
  if( sion_fileptr == NULL ) return SION_NOT_SUCCESS;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: second fileptr already set, aborting ...\n"));
    } else {
      sion_fileptr->flags |= SION_FILE_FLAG_SCNDANSI;
      sion_fileptr->second_fileptr=fileptr;
      return SION_SUCCESS;
    }
  } else {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: second fileptr could not be set for non-ANSI file, aborting ...\n"));
  }
}

/*!\brief Unset second fileptr for file if opened with ANSI
 *
 * @param  sion_fileptr         file handle
 *
 * @return `SION_SUCCESS` if success
 */
int _sion_file_unset_second_fileptr( _sion_fileptr *sion_fileptr) {
  if( sion_fileptr == NULL ) return SION_NOT_SUCCESS;

  if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      sion_fileptr->flags &= ~SION_FILE_FLAG_SCNDANSI;
      sion_fileptr->second_fileptr=NULL;
      return SION_SUCCESS;
    } else {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: second fileptr was not set, aborting ...\n"));
    }
  } else {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"internal error: second fileptr could not be unset for non-ANSI file, aborting ...\n"));
  }
}

/*!\brief Utility function: Get POSIX fp
 *
 * @param  sion_fileptr         file handle
 *
 * @return a POSIX file descriptor or `SION_ID_UNDEF` on failure
 */
int _sion_file_get_fd( _sion_fileptr *sion_fileptr) {
  if( sion_fileptr == NULL ) return SION_ID_UNDEF;

  if(sion_fileptr->flags & SION_FILE_FLAG_POSIX) {
    return sion_fileptr->fd;
  } else if(sion_fileptr->flags & SION_FILE_FLAG_ANSI) {
    if(sion_fileptr->flags & SION_FILE_FLAG_SCNDANSI)  {
      return fileno(sion_fileptr->second_fileptr);
    } else {
      return fileno(sion_fileptr->fileptr);
    }
#if defined(_SION_SIONFWD)
  } else if(sion_fileptr->flags & SION_FILE_FLAG_SIONFWD) {
    return SION_ID_UNDEF;
#endif
  } else {
    return _sion_errorprint(SION_ID_UNDEF,_SION_ERROR_RETURN,"_sion_file_get_fd: cannot find valid file flag (flags=%d)\n",sion_fileptr->flags);
  }
}

/* ********************************************************************************************** */
/* *** INTERFACE Functions                                                                        */
/* ********************************************************************************************** */

/*!\brief ANSI: Create and open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `NULL`
 */
FILE  *_sion_file_open_ansi_write_create(const char *fname, unsigned int addflags) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  return fopen(fname, "w");
#elif defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  int fd = open64(fname, O_CREAT | O_RDWR, 0664);
  return fdopen(fd, "w");
#else
#error "No platform selected."
  return NULL;
#endif
}

/*!\brief ANSI: Open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `NULL`
 */
FILE  *_sion_file_open_ansi_write_existing(const char *fname, unsigned int addflags) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  /* changed from w to r+ to prevent time truncation of empty existing file */
  return fopen(fname, "r+");
#elif defined(_SION_AIX)
  int fd = open64(fname, O_CREAT | O_RDWR, 0664);
  return fdopen(fd, "r+");
#elif defined(_SION_BGP) || defined(_SION_BGQ)
  int fd = open64(fname, O_RDWR, 0664);
  return fdopen(fd, "r+");
#else
#error "No platform selected."
  return NULL;
#endif
}

/*!\brief ANSI: Open a file for reading
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `NULL`
 */
FILE *_sion_file_open_ansi_read(const char *fname, unsigned int addflags) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  return fopen(fname, "r");
#elif defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  int fd = open64(fname, O_RDONLY, 0664);
  return fdopen(fd, "r");
#else
#error "No platform selected."
  return NULL;
#endif
}

/*!\brief ANSI: Close a file
 *
 * @param  *fileptr             ANSI file pointer
 *
 * @return `SION_SUCCESS` or `SION_NOT_SUCCESS`
 */
int _sion_file_close_ansi(FILE *fileptr) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  return (0 == fclose(fileptr)) ? SION_SUCCESS : SION_NOT_SUCCESS;
#else
#error "No platform selected."
  return SION_NOT_SUCCESS;
#endif
}

/*!\brief ANSI: Get optional file system block size for a file
 *
 * @param  fileptr  fileptr
 *
 * @return  blocksize or `-1` if not defined
 */
long _sion_file_get_opt_blksize_ansi( FILE *fileptr ) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  int fd = fileno(fileptr);
  struct stat sbuf;
  if (fstat(fd, &sbuf) == 0) {
    return sbuf.st_blksize;
  } else {
    return SION_SIZE_NOT_VALID;
  }
#else
#error "No platform selected."
  return SION_SIZE_NOT_VALID;
#endif
}

/*!\brief ANSI: Set the start position for the current task
 *
 * @param  *fileptr                     file handle
 * @param  startpointer         the position for the current task
 *
 * @return  new position in file, `-1` on failure
 */
sion_int64 _sion_file_set_position_ansi(FILE *fileptr, sion_int64 startpointer) {
  sion_int64 newpos;

  DPRINTFP((32, "_sion_set_position_ansi", -1, "enter (to %lld) (to %ld)\n", (long long) startpointer, (long) startpointer));

#if defined(_SION_LINUX) || defined(_SION_BGP) || defined(_SION_BGQ)
  off_t offset = (off_t) startpointer;
  if (offset != startpointer) {
    DPRINTFP((32, "_sion_set_position_ansi", -1, "_sion_set_position_ansi: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_set_position_ansi: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
  }
  int fd = fileno(fileptr);
  off_t result;
  newpos = result = lseek(fd, offset, SEEK_SET);
#if defined(_SION_LINUX)
  DPRINTFP((1024, "_sion_set_position_ansi", -1, "set position=%lld  (LINUX)\n", (long long) result));
#endif
#elif defined(_SION_DARWIN)
  long offset = (long) startpointer;
  if (offset != startpointer) {
    DPRINTFP((32, "_sion_set_position_ansi", -1, "_sion_set_position_ansi: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_set_position_ansi: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
  }
  off_t result;
  newpos = result = fseek(fileptr, offset, SEEK_SET);
  DPRINTFP((1024, "_sion_set_position_ansi", -1, "set position=%lld  (LINUX)\n", (long long) result));
#elif defined(_SION_AIX)
  off64_t   offset = (off_t) startpointer;
  off64_t   result;
  if (offset != startpointer) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_set_position_ansi: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
  }
  result = fseeko64(fileptr, offset, SEEK_SET);
  newpos = result = ftello64(fileptr);
#else
#error "No platform selected."
  newpos = SION_SIZE_NOT_VALID;
#endif
  DPRINTFP((32, "_sion_set_position_ansi", -1, "leave (%lld --> %lld)\n",(long long) startpointer, (long long) newpos));

  return newpos;
}

/*!\brief ANSI: Get the current position in file
 *
 * @param  *fileptr                     file handle
 *
 * @retval position in file, `-1` on failure
 */
sion_int64 _sion_file_get_position_ansi(FILE *fileptr) {
  sion_int64 result;

#if defined(_SION_LINUX)
    off_t     resulto;
    int       fd = fileno(fileptr);
    resulto = lseek(fd,0,SEEK_CUR);
    result = (sion_int64) resulto;
    DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (LINUX)\n", (long) result));
#elif defined(_SION_DARWIN)
    off_t     resulto;
    resulto = ftello(fileptr);
    result = (sion_int64) resulto;
    DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (DARWIN)\n", (long) result));
#elif defined(_SION_AIX)
    result = ftell(fileptr);
    DPRINTFP((4096, "_sion_get_position", -1, "got position=%lld (AIX)\n", result));
#elif defined(_SION_BGP)
    off_t     resulto;
    int       fd = fileno(fileptr);
    resulto = lseek(fd, 0, SEEK_CUR);
    result = (sion_int64) resulto;
    DPRINTFP((4096, "_sion_get_position", -1, "got position=%lld (BGP)\n", result));
#elif defined(_SION_BGQ)
    off_t     resulto;
    int       fd = fileno(fileptr);
    resulto = lseek(fd, 0, SEEK_CUR);
    result = (sion_int64) resulto;
    DPRINTFP((4096, "_sion_get_position", -1, "got position=%lld (BGQ)\n", result));
#else
#error "No platform selected."
  result = SION_SIZE_NOT_VALID;
#endif

  return (result);
}

/*!\brief ANSI: Flush the data to the disk
 *
 * @param  *fileptr             file handle
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_flush_ansi(FILE *fileptr) {
  return (0 == fflush(fileptr)) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief ANSI: Purge the data to the disk
 *
 * @param  *fileptr             file handle
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_purge_ansi(FILE *fileptr) {
  return _sion_file_flush_ansi(fileptr);
}

/*!\brief ANSI: set buffer of fp
 *
 * @param  *fileptr             file handle
 * @param  *buffer              pointer buffer
 * @param  *buffer_size         buffer size
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_set_buffer_ansi(FILE *fileptr, char *buffer, sion_int32 buffer_size) {
  DPRINTFP((32, "_sion_file_set_buffer", -1, "set buffer of fileptr\n"));
  return (0 == setvbuf(fileptr, buffer, _IOFBF, (size_t) buffer_size)) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief ANSI: Write data to file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to write
 * @param  fileptr              file handle
 *
 * @return returns `bytes` if the requested amount could be written, otherwise `-1`
 */
sion_int64 _sion_file_write_ansi(const void *data, sion_int64 bytes, FILE *fileptr ) {
  return (bytes == fwrite(data, 1, bytes, fileptr)) ? bytes : -1;
}

/*!\brief ANSI: Read data from file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to read
 * @param  fileptr              file handle
 *
 * @return returns `bytes` if the requested amount could be read,
 *         on EOF, returns `n` with `0 <= n < bytes`,
 *         otherwise `-1`
 */
sion_int64 _sion_file_read_ansi(void *data, sion_int64 bytes, FILE *fileptr ) {
  if (fread(data, 1, bytes, fileptr) < bytes) {
    if (feof(fileptr)) {
      return bytes;
    } else {
      return -1;
    }
  } else {
    return bytes;
  }
}

/*!\brief POSIX: Create and open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int _sion_file_open_posix_write_create(const char *fname, unsigned int addflags) {
  int fd;

#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  do {
    fd = open(fname, O_CREAT | O_RDWR, 0664);
  } while (-1 == fd && EINTR == errno);
#elif defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  do {
    fd = open64(fname, O_CREAT | O_RDWR, 0664);
  } while (-1 == fd && EINTR == errno);
#else
#error "No platform selected."
  fd = -1;
#endif

  return fd;
}

/*!\brief POSIX: Open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int  _sion_file_open_posix_write_existing(const char *fname, unsigned int addflags) {
  int fd;

#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  do {
    fd = open(fname, O_RDWR, 0664);
  } while (-1 == fd && EINTR == errno);
#elif defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  do {
    fd = open64(fname, O_RDWR, 0664);
  } while (-1 == fd && EINTR == errno);
#else
#error "No platform selected."
  fd = -1;
#endif

  return fd;
}

/*!\brief POSIX: Open a file for reading
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int _sion_file_open_posix_read(const char *fname, unsigned int addflags) {
  int fd;

#if defined(_SION_LINUX) || defined(_SION_DARWIN)
  do {
    fd = open(fname, O_RDONLY, 0664);
  } while (-1 == fd && EINTR == errno);
#elif defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  do {
    fd = open64(fname, O_RDONLY, 0664);
  } while (-1 == fd && EINTR == errno);
#else
#error "No platform selected."
  fd = -1;
#endif

  return (fd);
}

/*!\brief POSIX: Close a file
 *
 * @param  fd                   POSIX file pointer
 *
 * @return `SION_SUCCESS` or `SION_NOT_SUCCESS`
 */
int _sion_file_close_posix(int fd) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  return (0 == close(fd)) ? SION_SUCCESS : SION_NOT_SUCCESS;
#else
#error "No platform selected."
  return SION_NOT_SUCCESS;
#endif
}

/*!\brief POSIX: Get optional file system block size for a file
 *
 * @param  fd  file descriptor
 *
 * @return  blocksize or `-1` if not defined
 */
long _sion_file_get_opt_blksize_posix( int fd ) {
#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_AIX) || defined(_SION_BGP) || defined(_SION_BGQ)
  struct stat sbuf;
  if (fstat(fd, &sbuf) == 0) {
    return sbuf.st_blksize;
  } else {
    return SION_SIZE_NOT_VALID;
  }
#else
#error "No platform selected."
  return SION_SIZE_NOT_VALID;
#endif
}

/*!\brief POSIX: Set the start position for the current task
 *
 * @param  fd                   file descriptor
 * @param  startpointer         the position for the current task
 *
 * @return  new position in file, `-1` on failure
 */
sion_int64 _sion_file_set_position_posix(int fd, sion_int64 startpointer) {
  sion_int64  newpos = SION_SIZE_NOT_VALID;

  DPRINTFP((32, "_sion_set_position_posix", -1, "enter (to %lld)\n", (long long) startpointer));

#if defined(_SION_LINUX) || defined(_SION_DARWIN) || defined(_SION_BGP) || defined(_SION_BGQ)
    off_t offset = (off_t) startpointer;
    if (offset != startpointer) {
      return (_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_set_position_posix: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
        offset, sizeof(offset), startpointer, sizeof(startpointer)));
    }
    off_t result;
    newpos = result = lseek(fd, offset, SEEK_SET);
#if defined(_SION_LINUX) || defined(_SION_DARWIN)
    DPRINTFP((4096, "_sion_set_position_posix", -1, "set position=%lld  (LINUX)\n", (long long) result));
#endif
#elif defined(_SION_AIX)
  off64_t   offset = (off_t) startpointer;
  off64_t   result;
  if (offset != startpointer) {
    return(_sion_errorprint(SION_SIZE_NOT_VALID,_SION_ERROR_RETURN,"_sion_set_position_posix: cannot set position to %ld (%zu),%lld (%zu)  offset conversion error\n",
      offset, sizeof(offset), startpointer, sizeof(startpointer)));
  }
  newpos = result = lseek(fd, offset, SEEK_SET);
#else
#error "No platform selected."
  newpos = SION_SIZE_NOT_VALID;
#endif
  DPRINTFP((32, "_sion_set_position_posix", -1, "leave (to %lld)\n",(long long) startpointer));

  return newpos;
}

/*!\brief POSIX: Get the current position in file
 *
 * @param  fd                           file handle
 *
 * @retval position in file or `-1` on failure
 */
sion_int64 _sion_file_get_position_posix(int fd) {
  sion_int64 result=SION_SIZE_NOT_VALID;

  off_t     resulto;
  resulto = lseek(fd,0,SEEK_CUR);
  result = (sion_int64) resulto;

#if defined(_SION_LINUX)
  DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (LINUX)\n", (long) result));
#elif defined(_SION_DARWIN)
  DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (DARWIN)\n", (long) result));
#elif defined(_SION_AIX)
  DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (AIX)\n", (long) result));
#elif defined(_SION_BGP)
  DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (BGP)\n", (long) result));
#elif defined(_SION_BGQ)
  DPRINTFP((4096, "_sion_get_position", -1, "get position=%ld  (BGQ)\n", (long) result));
#endif

  return (result);
}

/*!\brief POSIX: Flush the data to the disk
 *
 * @param  fd  file descriptor
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_flush_posix(int fd) {
  int frc;

#if defined(_SION_BGQ)
  /* BGQ 20.09.13: bad fsync performance since V1R2M1, try without */
  frc = 0;
#else
  do {
    frc = fsync(fd);
  } while (frc != 0 && EINTR == errno);
#endif

  return (0 == frc) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief POSIX: Purge the data to the disk
 *
 * Purge is only relevant for ANSI, using fsync instead
 *
 * @param  fd  file descriptor
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_purge_posix(int fd) {
  int frc;

#if defined(_SION_BGQ)
  /* BGQ 20.09.13: bad fsync performance since V1R2M1, try without */
  frc = 1
#else
  do {
    frc = fsync(fd);
  } while (frc != 0  && EINTR == errno);
#endif

  return (0 == frc) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief POSIX: set buffer of fd
 *
 * @param  fd                   file descriptor
 * @param  *buffer              pointer buffer
 * @param  *buffer_size         buffer size
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_set_buffer_posix(int fd, char *buffer, sion_int32 buffer_size) {
  DPRINTFP((32, "_sion_file_set_buffer", -1, "set buffer of fileptr\n"));
  return SION_SUCCESS;
}

/*!\brief POSIX: Write data to file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to write
 * @param  fd                   file handle
 *
 * @return returns `bytes` if the requested amount could be written, otherwise `-1`
 */
sion_int64 _sion_file_write_posix(const void *data, sion_int64 bytes, int fd ) {
  ssize_t n = 0, k;
  while (1) {
    k = write(fd, data, bytes);
    if (k == -1) {
      if (errno != EINTR) {
        // I/O error, return -1
        return -1;
      } // else, interrupted before starting, retry
    } else if (k == bytes) {
      // requested amount has been written
      return n + k;
    } else {
      // k < bytes, presumably interrupted or no space left, retry
      // - retry after interruption should continue
      // - retry with no space left should lead to error
      bytes -= k;
      n += k;
      data = (char*)data + k;
    }
  }
}

/*!\brief POSIX: Read data from file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to read
 * @param  fd                   file handle
 *
 * @return returns `bytes` if the requested amount could be read,
 *         on EOF, returns `n` with `0 <= n < bytes`,
 *         otherwise `-1`
 */
sion_int64 _sion_file_read_posix(void *data, sion_int64 bytes, int fd ) {
  ssize_t n = 0, k;
  while (1) {
    k = read(fd, data, bytes);
    if (k == -1) {
      if (errno != EINTR) {
        // I/O error, return -1
        return -1;
      } // else, interrupted before starting, retry
    } else if (k == 0) {
      // presumably EOF, return number of bytes read up to here
      return n;
    } else if (k == bytes) {
      // requested amount has been read
      return n + k;
    } else {
      // k < bytes, presumably interrupted or EOF, retry
      // - retry after interruption should continue
      // - retry at EOF should lead to k == 0 next
      bytes -= k;
      n += k;
      data = (char*)data + k;
    }
  }
}

#if defined(_SION_SIONFWD)
/*!\brief SIONfwd: Create and open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int _sion_file_open_sionfwd_write_create(const char *fname, unsigned int addflags) {
  return sionfwd_open(fname, SIONFWD_WRITE | SIONFWD_CREATE);
}

/*!\brief SIONfwd: Open a new file for writing
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int _sion_file_open_sionfwd_write_existing(const char *fname, unsigned int addflags) {
  return sionfwd_open(fname, SIONFWD_WRITE);
}

/*!\brief SIONfwd: Open a file for reading
 *
 * @param  *fname               filename to use
 * @param  addflags             optional additional flags
 *
 * @return file handle or `-1` on failure
 */
int _sion_file_open_sionfwd_read(const char *fname, unsigned int addflags) {
  return sionfwd_open(fname, SIONFWD_READ);
}

/*!\brief SIONfwd: Close a file
 *
 * @param  fd                   POSIX file pointer
 *
 * @return `SION_SUCCESS` or `SION_NOT_SUCCESS`
 */
int _sion_file_close_sionfwd(int fd) {
  return (0 == sionfwd_close(fd)) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief SIONfwd: Get optional file system block size for a file
 *
 * @param  fd  file descriptor
 *
 * @return  blocksize or `-1` if not defined
 */
long _sion_file_get_opt_blksize_sionfwd(int fd) {
  return sionfwd_stat_blksize(fd);
}

/*!\brief SIONfwd: Flush the data to the disk
 *
 * @param  fd  file descriptor
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_flush_sionfwd(int fd) {
  return (sionfwd_flush(fd) == 0) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief SIONfwd: Purge the data to the disk
 *
 * Purge is only relevant for ANSI, using fsync instead
 *
 * @param  fd  file descriptor
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_purge_sionfwd(int fd) {
  return (sionfwd_flush(fd) == 0) ? SION_SUCCESS : SION_NOT_SUCCESS;
}

/*!\brief SIONfwd: set buffer of fd
 *
 * @param  fd                   file descriptor
 * @param  *buffer              pointer buffer
 * @param  *buffer_size         buffer size
 *
 * @retval `SION_SUCCESS` if OK
 */
int _sion_file_set_buffer_sionfwd(int fd, char *buffer, sion_int32 buffer_size) {
  DPRINTFP((32, "_sion_file_set_buffer", -1, "set buffer of fileptr\n"));
  return SION_SUCCESS;
}

/*!\brief SIONfwd: Write data to file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to write
 * @param  fd                   file handle
 * @param  position             current position in the file, will be updated
 *
 * @return returns `bytes` if the requested amount could be written, otherwise `-1`
 */
sion_int64 _sion_file_write_sionfwd(const void *data, sion_int64 bytes, int fd, sion_int64 *position) {
  sion_int64 written = sionfwd_pwrite(fd, data, bytes, *position);
  if (written != -1) { *position += written; }
  return written;
}

/*!\brief SIONfwd: Read data from file
 *
 * @param  data                 pointer to data
 * @param  bytes                number of bytes to read
 * @param  fd                   file handle
 * @param  position             current position in the file, will be updated
 *
 * @return returns `bytes` if the requested amount could be read,
 *         on EOF, returns `n` with `0 <= n < bytes`,
 *         otherwise `-1`
 */
sion_int64 _sion_file_read_sionfwd(void *data, sion_int64 bytes, int fd, sion_int64 *position) {
  sion_int64 bread = sionfwd_pread(fd, data, bytes, *position);
  if (bread != -1) { *position += bread; }
  return bread;
}
#endif

/*!\brief Create and return _sion_fileptr
 *
 * @return _sion_fileptr
 */
_sion_fileptr *_sion_file_alloc_and_init_sion_fileptr(void) {
  _sion_fileptr *sion_fileptr;

  sion_fileptr = (_sion_fileptr *) malloc(sizeof(_sion_fileptr));
  if (sion_fileptr == NULL) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"cannot allocate _sion_fileptr structure of size %lu (_sion_fileptr), aborting ...\n",
		     (unsigned long) sizeof(_sion_fileptr));
    return(NULL);
  }
  sion_fileptr->fileptr        = NULL;
  sion_fileptr->second_fileptr = NULL;
  sion_fileptr->fd             = -1;
  sion_fileptr->flags          = 0;
  sion_fileptr->position       = 0;

  return (sion_fileptr);
}

#define STR_PRT(X) case X: return # X
/*!
 * @return String representation of flag
 */
char* _sion_fileptrflags_to_str (unsigned int flag) {
  switch (flag) {
    STR_PRT(SION_FILE_FLAG_ANSI);
    STR_PRT(SION_FILE_FLAG_SCNDANSI);
    STR_PRT(SION_FILE_FLAG_POSIX);
    STR_PRT(SION_FILE_FLAG_CREATE);
    STR_PRT(SION_FILE_FLAG_WRITE);
    STR_PRT(SION_FILE_FLAG_READ);
    STR_PRT(SION_FILE_FLAG_SIONFWD);
  }
  return "";
}

/*!
 * @return File pointer description
 */
char* _sion_get_fileptr_desc(_sion_fileptr *sion_fileptr) {
  int flags;
  if(!sion_fileptr) return("<undefined>");
  flags = sion_fileptr->flags;

  if(flags & SION_FILE_FLAG_ANSI) {
    if(flags & SION_FILE_FLAG_WRITE) {
      if(flags & SION_FILE_FLAG_CREATE) {
	return("<ANSI,WRITE,CREATE>");
      } else {
	return("<ANSI,WRITE>");
      }
    } else {
	return("<ANSI,READ>");
    }
  } else {
    if (flags & SION_FILE_FLAG_POSIX) {
      if(flags & SION_FILE_FLAG_WRITE) {
	if(flags & SION_FILE_FLAG_CREATE) {
	  return("<POSIX,WRITE,CREATE>");
      } else {
	  return("<POSIX,WRITE>");
	}
      } else {
	  return("<POSIX,READ>");
      }
    } else {
      if (flags & SION_FILE_FLAG_SIONFWD) {
        if(flags & SION_FILE_FLAG_WRITE) {
          if(flags & SION_FILE_FLAG_CREATE) {
            return("<SIONFWD,WRITE,CREATE>");
        } else {
            return("<SIONFWD,WRITE>");
          }
        } else {
            return("<SIONFWD,READ>");
        }
      }
    }
  }
  return("<unknown>");
}
