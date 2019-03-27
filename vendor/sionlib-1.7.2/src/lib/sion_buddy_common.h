/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_SION_BUDDY_COMMON_H
#define SION_SION_BUDDY_COMMON_H

#include "sion_common.h"

/* TODO: KT --> dynamic list  */
#define MAXREADSTEPS 10

#define _SION_BW_SCHED_NUM_PASSES 3
#define _SION_BW_SCHED_NOACTION   0
#define _SION_BW_SCHED_ACTIONA    1
#define _SION_BW_SCHED_ACTIONB    2

struct _sion_generic_buddy_info_struct {
  int    rank;
  int    size;
  void  *commgroup;
  int    sid;
  int    collsize;
  int    filelrank;
  int    groupnum;    		/* number of commgroup in overall numbering */
  int    from_index;
  int    to_index;
  int    stepnum;
  int    groupid;
  int    myrole;
  int    bnum;                  /* number of buddy level (-1 ... NUM_BUDDIES) */
  int    filenum;               /* number of multi file */
};
typedef struct _sion_generic_buddy_info_struct _sion_generic_buddy_info;

/* container describing a buddy cp */
struct _sion_generic_buddy_struct {
  _sion_generic_buddy_info buddy_coll;
  _sion_generic_buddy_info buddy_send;
  int numgroups;
  int numsteps;
  int currentgroup;
  _sion_generic_buddy_info *groups[MAXREADSTEPS];
};
typedef struct _sion_generic_buddy_struct _sion_generic_buddy;

char* _sion_buddy_role_to_str (unsigned int flag);
sion_io_stat_t* sion_get_io_info_buddy(int sid, int roles, int flag);

#endif
