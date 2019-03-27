/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
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

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_tools.h"

/*!\brief Return endianness
 *
 * @return 1-> big endian
 *         0 ->little endian
 */
int sion_get_endianness(void)
{

  /* Are we little or big endian?  From Harbison&Steele.  */
  union
  {
    long      l;
    char      c[sizeof(long)];
  } u;

  u.l = 1;
  return (u.c[sizeof(long) - 1] == 1);

}

/*!\brief Return version numbers
 *
 *  return version numbers
 */
int sion_get_version(int *main_version,int *sub_version,int *patch_level,int *fileformat_version) 
{
  *main_version       = SION_MAIN_VERSION;
  *sub_version        = SION_SUB_VERSION;
  *patch_level        = SION_VERSION_PATCHLEVEL;
  *fileformat_version = SION_FILEFORMAT_VERSION;
  return(SION_SUCCESS);
}

FILE     *sion_get_fp(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_get_fp: invalid sion_filedesc sid=%d ...\n",sid);
    return (NULL);
  }

  if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_ANSI) {
    if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_SCNDANSI) {
      return(sion_filedesc->fileptr->second_fileptr);
    } else {
      return(sion_filedesc->fileptr->fileptr);
    }
  } else {
    return(NULL);
  }
}

int     sion_get_fd(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_get_fp: invalid sion_filedesc sid=%d ...\n",sid);
    return (SION_ID_UNDEF);
  }

  if(sion_filedesc->fileptr->flags&SION_FILE_FLAG_POSIX) {
    return(sion_filedesc->fileptr->fd);
  } else {
    return(SION_ID_UNDEF);
  }
  
}


_sion_filedesc   *_sion_get_filedesc(int sid)
{
  _sion_filedesc *sion_filedesc;

  if ((sid<0) || (_sion_vcdtype(sid) != SION_FILEDESCRIPTOR)
      || !(sion_filedesc = _sion_vcdtovcon(sid))) {
    _sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_get_fp: invalid sion_filedesc sid=%d ...\n",sid);
    return (NULL);
  }

  return (sion_filedesc);
}


