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
 * Hybrid (MPI+OpenMP) utility functions
 */

#define _XOPEN_SOURCE 700

#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include <sys/types.h>
#include <fcntl.h>

#include <unistd.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_internal.h"
#include "sion_fd.h"
#include "sion_filedesc.h"
#include "sion_printts.h"



#ifdef SION_MPI

#ifdef _SION_BGQ

#include <firmware/include/personality.h>
int sion_get_IO_comm_ompi(MPI_Comm *commSame) {

    Personality_t personality;
    int           rc, rank, factor, bridgeid;
    
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);

    /* get location information */
    Kernel_GetPersonality(&personality, sizeof(Personality_t));

    factor=1;
    bridgeid = personality.Network_Config.cnBridge_E;
    factor   *= personality.Network_Config.Enodes;
    bridgeid += personality.Network_Config.cnBridge_D*factor;
    factor   *= personality.Network_Config.Dnodes;
    bridgeid += personality.Network_Config.cnBridge_C*factor;
    factor   *= personality.Network_Config.Cnodes;
    bridgeid += personality.Network_Config.cnBridge_B*factor;
    factor   *= personality.Network_Config.Bnodes;
    bridgeid += personality.Network_Config.cnBridge_A*factor;

    /* communicator consists of all task working with the same I/O-node */
    rc=MPI_Comm_split(MPI_COMM_WORLD, bridgeid, rank, commSame);
    return(rc);
}

#else
/* default */
int sion_get_IO_comm_ompi(MPI_Comm *commSame) {

    int rc;
    rc=MPI_Comm_dup(MPI_COMM_WORLD, commSame);
    return(rc);
}

#endif
/* end of ifdef MPI */
#endif
