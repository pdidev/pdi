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

#ifndef SION_SION_FD_H
#define SION_SION_FD_H

#define SION_FILEDESCRIPTOR 11  /*!< int id -> data structure mapping (from visit library) */
#define SION_APIDESCRIPTOR  12  /*!< int id -> data structure mapping (from visit library) */

#ifdef __cplusplus
extern "C" {
#endif

int _sion_newvcd(void *data, int type);
int _sion_freevcd(int sid);
void *_sion_vcdtovcon(int sid);
int _sion_vcdtype(int sid);
int _sion_reassignvcd(int sid, void *data, int type);

  /* SION_FD INTERNAL */
#define SION_FD_CHUNK 2 
struct _sion_fd {
  int  state;   /* 0: fresh, 1: in use, 2: used before, now free */
  int  type;
  void *data;
};

typedef struct _sion_fddata sion_fddata;

int _sion_new_fd(sion_fddata *fdd, void *data, int type);
int _sion_set_fd(sion_fddata *fdd, int fd, void *data,
		     int type, char *text);
void *_sion_fd2ptr(sion_fddata *fdd, int fd);
int _sion_fd2type(sion_fddata *fdd, int fd, char *text);
int _sion_ptr2fd(sion_fddata *fdd, void *ptr, char *text);
int _sion_free_fd(sion_fddata *fdd, int fd, char *text);
int _sion_reassign_fd(sion_fddata *fdd, int fd, void *data, int type, char *text);
int _sion_fd_size(sion_fddata *fdd);

#ifdef __cplusplus
}
#endif

#endif
