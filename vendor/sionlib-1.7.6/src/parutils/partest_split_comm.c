/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2019                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/

#define _XOPEN_SOURCE 700

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <mpi.h>
#include <time.h>
#include <math.h>

#include "partest_split_comm.h"
#include "partest_util.h"

#ifdef _SION_BGQ
#include <firmware/include/personality.h>
#include <spi/include/kernel/process.h>
#include <spi/include/kernel/location.h>
#ifdef __GNUC__
#include <hwi/include/bqc/A2_inlines.h>   /* for GetTimebase() */
#endif
#include <hwi/include/common/uci.h>
#include <mpix.h>
#endif

#ifdef _SION_BGP
#include <common/bgp_personality.h>
#include <common/bgp_personality_inlines.h>
#endif

#ifdef _SION_FX
#include <mpi-ext.h>
#endif

#ifdef _SION_AIX
#include <unistd.h>
#endif

#ifdef _SION_LINUX
#include <unistd.h>
#endif


int split_communicator(_test_communicators * communicators, int bluegene, int bluegene_np, int bluegene_sort, int numfiles, int read_task_offset, int verbose)
{
  int       proc_per_file;

  communicators->work_size = communicators->work_rank = -2;
  communicators->local_size = communicators->local_rank = -2;



#ifdef _SION_BGP
  if (bluegene) {               /* order MPI-tasks by I/O-node */
    _BGP_Personality_t personality;
    MPI_Comm  commSame, commDiff;
    int       sizeSame, sizeDiff;
    int       rankSame, rankDiff;
    char      location[128];
    unsigned  procid, x, y, z, t;
    char      cbuffer[MAXCHARLEN];

    /* get location information */
    Kernel_GetPersonality(&personality, sizeof(personality));
    BGP_Personality_getLocationString(&personality, location);
    procid = Kernel_PhysicalProcessorID();
    MPIX_rank2torus(communicators->all_rank, &x, &y, &z, &t);

    /* task of communicator working with different I/O-nodes */
    MPIX_Pset_diff_comm_create(&commDiff);
    MPI_Comm_size(commDiff, &sizeDiff);
    MPI_Comm_rank(commDiff, &rankDiff);
    communicators->ionode_number = rankDiff;

    /* communicator consists of all task working with the same I/O-node */
    MPIX_Pset_same_comm_create(&commSame);
    MPI_Comm_size(commSame, &sizeSame);
    MPI_Comm_rank(commSame, &rankSame);

    /*  if -p not specified all proc will write! */
    if (bluegene_np == 0) {
      bluegene_np = sizeSame;
    }

    /*  Get a communicator with all writing tasks => new global communicator */
    MPI_Comm_split(communicators->all, (rankSame < bluegene_np), communicators->all_rank, &communicators->work);
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    if (rankSame >= bluegene_np) {
      /* not working task */
      communicators->work_size = communicators->work_rank = -1;
      communicators->local_size = communicators->local_rank = -1;
    }

    /*  If only one file will be used => dont split further */
    /*  if numfile > 1 sion will generate correct local communicator */
    if (numfiles >= 1) {
      communicators->local = communicators->work;
    }
    else if(numfiles<0) {
      if(numfiles==-1) {
	/*  Split the common communicator for each IO node to get a local comm with only the writing tasks for this IO Node */
	MPI_Comm_split(commSame, (rankSame < bluegene_np), communicators->all_rank, &communicators->local);
      } else {
	/*  local communicator contains only one task per IO-node */
	/* bluegene_np has to be 512 */
	communicators->local=commDiff;
      }
    } 
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

    /* determine filenumber */
    if (numfiles < 1) {
      /* one file per I/O-node */
      if(numfiles==-1)  communicators->file_number = rankDiff;
      else              communicators->file_number = rankSame;
    }
    else {
      communicators->file_number = -1;
    }

    /* print log message about location, ...  */
    sprintf(cbuffer, "");
    if (rankSame < bluegene_np) {
      if (verbose) {
        sprintf(cbuffer, "BGP[%05d] diff_comm: %4d of %4d  same_comm: %5d of %5d file_comm: %5d of %5d %s phys_xyzt(%ud,%ud,%ud,%ud)\n",
                communicators->all_rank, rankDiff + 1, sizeDiff, rankSame + 1, sizeSame, communicators->local_rank + 1, communicators->local_size,
                location, x, y, z, t);
      }
    }
    collective_print_gather(cbuffer, communicators->work);

  }
#endif

#ifdef _SION_BGQ
  if (bluegene) {               /* order MPI-tasks by I/O-node */
    Personality_t personality;
    MPI_Comm  commSame, commDiff;
    MPIX_Hardware_t hw;
    int       sizeSame, sizeDiff;
    int       rankSame, rankDiff;
    int       baserank;
    int       factor, bridgeid,core, hwthread,procid;
    int       dist_to_bridge, isonbridge;
    char      cbuffer[MAXCHARLEN];
    char      location[64];
    BG_UniversalComponentIdentifier uci;
    unsigned int row, col, mp, nb, cc;
    double    starttime;
    int       key, color, baseid;

    if(communicators->all_rank==0) {
      starttime=MPI_Wtime();
      printf("partest_split_comm[%d]: starting at Wt=%10.3fs\n",communicators->all_rank,starttime);
    }
    
    /* get location information */
    Kernel_GetPersonality(&personality, sizeof(Personality_t));
    MPIX_Hardware(&hw);
    uci = personality.Kernel_Config.UCI;
    bg_decodeComputeCardOnNodeBoardUCI(uci, &row, &col, &mp, &nb, &cc);

    procid   = Kernel_ProcessorID();       /* 0-63 */
    core     = Kernel_ProcessorCoreID();   /* 0-15 */
    hwthread = Kernel_ProcessorThreadID(); /* 0-3 */

    sprintf(location, "R%x%x-M%ud-N%02x-J%02x <%d,%d,%d,%d,%d> p%02dc%02dt%1d", row, col, mp, nb, cc,
	    personality.Network_Config.Acoord, personality.Network_Config.Bcoord, 
	    personality.Network_Config.Ccoord, personality.Network_Config.Dcoord, 
	    personality.Network_Config.Ecoord,
	    procid,core,hwthread);

    if ( 
	( personality.Network_Config.Acoord==personality.Network_Config.cnBridge_A ) &&
	( personality.Network_Config.Bcoord==personality.Network_Config.cnBridge_B ) &&
	( personality.Network_Config.Ccoord==personality.Network_Config.cnBridge_C ) &&
	( personality.Network_Config.Dcoord==personality.Network_Config.cnBridge_D ) &&
	( personality.Network_Config.Ecoord==personality.Network_Config.cnBridge_E )
	 )
      {
      isonbridge=1;
    } else {
      isonbridge=0;
    }
    
    dist_to_bridge=MPIX_IO_distance();

    /* following could be replaced by MPIX_IO_link_id()  */
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


    if(bluegene==2) {
      /* per ION */
      /* communicator consists of all task working with the same I/O-bridge */
      if(bluegene_sort==0) {
	key   = MPIX_IO_distance();
      } else {
	key   = communicators->all_rank; 
      }
      
      MPI_Comm_split(communicators->all, bridgeid, key, &commSame);
      MPI_Comm_size(commSame, &sizeSame);
      MPI_Comm_rank(commSame, &rankSame);
      baseid=bridgeid;
      
      /* communicator consists of all task working with the different I/O-nodes */
      MPI_Comm_split(communicators->all, rankSame, bridgeid, &commDiff);
      MPI_Comm_size(commDiff, &sizeDiff);
      MPI_Comm_rank(commDiff, &rankDiff);
      
      communicators->ionode_number = rankDiff;
    } else {
	/* similar to MPIX_Pset_same_comm_create but with ION-id */
      MPI_Comm  commSameIOB;
      int       sizeSameIOB;
      int       rankSameIOB;
      int       ionid;
      
      /* only needed for ionid distribution */
      key   = communicators->all_rank; 
      MPI_Comm_split(communicators->all, bridgeid, key, &commSameIOB);
      MPI_Comm_size(commSameIOB, &sizeSameIOB);
      MPI_Comm_rank(commSameIOB, &rankSameIOB);

      if(communicators->all_rank==0) {
	printf("partest_split_comm[%d]: after split commSameIOB deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
      }

      if(rankSameIOB==0) {
	/* ionid=MPIX_IO_node_id() & 0xFFFF;  */
	ionid=MPIX_IO_node_id();
     }
      MPI_Bcast(&ionid, 1, MPI_INT, 0, commSameIOB);

      if(communicators->all_rank==0) {
	printf("partest_split_comm[%d]: after get ionid         deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
      }

      color = (int) ionid;
      if(bluegene_sort==0) {
	 key   = MPIX_IO_distance();
      } else {
	key   = communicators->all_rank; 
      }
      int rc=0;
      rc=MPI_Comm_split(communicators->all,color,key,&commSame);
      MPI_Comm_size(commSame, &sizeSame);
      MPI_Comm_rank(commSame, &rankSame);
      baseid=ionid;
      
      if(communicators->all_rank==0) {
	printf("partest_split_comm[%d]: after split commSame    deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
      }

      /* distribute global rank of first task in samecomm  */
      if(rankSame==0)  baserank=communicators->all_rank;
      MPI_Bcast(&baserank, 1, MPI_INT, 0, commSame);
    
      if(communicators->all_rank==0) {
	printf("partest_split_comm[%d]: after bcast baserank    deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
      }

      /* similar to MPIX_Pset_diff_comm_create but with ION-id, but without: ... *hw.ppn+hw.coreID */
      color = rankSame;
      key   = baserank; 	/* rank of task 0 of samecomm in MPI_COMM_WORLD */
      MPI_Comm_split(communicators->all,color,key,&commDiff);
      MPI_Comm_size(commDiff, &sizeDiff);
      MPI_Comm_rank(commDiff, &rankDiff);

      if(communicators->all_rank==0) {
	printf("partest_split_comm[%d]: after split commDiff    deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
      }

    }

    /* printf("WF: %02d of %02d here rSame=%2d rDiff=%2d bg_np=%2d --> factor=%2d, bridgeid=%2d %s -> bridge(%d,%d,%d,%d,%d)\n",communicators->all_rank,communicators->all_size, rankSame, rankDiff, bluegene_np, factor, bridgeid,location, */
    /* 	   personality.Network_Config.cnBridge_A, personality.Network_Config.cnBridge_B, personality.Network_Config.cnBridge_C, personality.Network_Config.cnBridge_D, personality.Network_Config.cnBridge_E); */

    /*  if -p not specified all proc will write! */
    if (bluegene_np == 0) {
      bluegene_np = sizeSame;
    }

    /*  Get a communicator with all writing tasks => new global communicator */
    /* TODO: better to MPI_UNDEFINED when rankSame >= bluegene_np */
    MPI_Comm_split(communicators->all, (rankSame < bluegene_np), communicators->all_rank, &communicators->work);
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    if (rankSame >= bluegene_np) {
      /* not working task */
      communicators->work_size = communicators->work_rank = -1;
      communicators->local_size = communicators->local_rank = -1;
    }
    if(communicators->all_rank==0) {
      printf("partest_split_comm[%d]: after split work        deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
    }
    
    /*  If only one file will be used => dont split further */
    /*  if numfile > 1 sion will generate correct local communicator */
    if (numfiles >= 1) {
      communicators->local = communicators->work;
    }
    else if(numfiles<0) {
      if(numfiles==-1) {
#ifdef SPLITCASCADE
	/*  Split the common communicator for each IO node to get a local comm with only the writing tasks for this IO Node */
	MPI_Comm_split(commSame, (rankSame < bluegene_np), rankSame, &communicators->local);
#else
	/*  Split the common communicator for each IO node to get a local comm with only the writing tasks for this IO Node */
	color=(rankSame < bluegene_np)?baseid:MPI_UNDEFINED;
	MPI_Comm_split(communicators->all, color, rankSame, &communicators->local);
#endif
	if (rankSame < bluegene_np) {
	  MPI_Comm_size(communicators->local, &communicators->local_size);
	  MPI_Comm_rank(communicators->local, &communicators->local_rank);
	}

	if(communicators->all_rank==0) {
	  printf("partest_split_comm[%d]: after split local       deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
	}

      } else {
	/*  local communicator contains only one task per IO-node */
	/* bluegene_np has to be 512 */
	communicators->local=commDiff;
	MPI_Comm_size(communicators->local, &communicators->local_size);
	MPI_Comm_rank(communicators->local, &communicators->local_rank);
      }
    } 

    /* determine filenumber */
    if (numfiles < 1) {
      /* one file per I/O-node */
      if(numfiles==-1)  communicators->file_number = rankDiff;
      else              communicators->file_number = rankSame;
    }
    else {
      communicators->file_number = -1;
    }

    if(communicators->all_rank==0) {
      printf("partest_split_comm[%d]: before print verbose    deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
    }
    /* print log message about location, ...  */
    if (verbose) {
      sprintf(cbuffer, "");
      if (rankSame < bluegene_np) {
        sprintf(cbuffer, "BGQ[%05d] diff_comm: %4d of %4d  same_comm: %5d of %5d file_comm: %5d of %5d %s bridge=%d, dist=%d\n",
                communicators->all_rank, rankDiff + 1, sizeDiff, rankSame + 1, sizeSame, communicators->local_rank + 1, 
		communicators->local_size, location,isonbridge,dist_to_bridge);
      }
      collective_print_gather(cbuffer, communicators->work);
    }
    if(communicators->all_rank==0) {
      printf("partest_split_comm[%d]: after print verbose/end deltaWt=%10.3fs\n",communicators->all_rank,MPI_Wtime()-starttime);
    }

  } /* if (bluegene) */
#endif

#ifdef _SION_AIX
  /* no communicator adjustment */
#endif

#ifdef _SION_DARWIN
  if (verbose) {
    char      location[256];
    gethostname(location, 256);
    char      cbuffer[MAXCHARLEN];
    sprintf(cbuffer, "DARWIN[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %s\n",
            communicators->all_rank, communicators->all_rank, communicators->all_size,
            communicators->work_rank, communicators->work_size, communicators->local_rank, communicators->local_size, location);
    collective_print_gather(cbuffer, communicators->all);
  }

#endif

#if defined( _SION_LINUX) && (!defined(_SION_FX))
  if (verbose) {
    char      location[256];
    gethostname(location, 256);
    char      cbuffer[MAXCHARLEN];
    sprintf(cbuffer, "LINUX[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %s\n",
            communicators->all_rank, communicators->all_rank, communicators->all_size,
            communicators->work_rank, communicators->work_size, communicators->local_rank, communicators->local_size, location);
    collective_print_gather(cbuffer, communicators->all);
  }

#endif

#ifdef _SION_AIX
  if (verbose) {
    char      location[256];
    gethostname(location, 256);
    int       sizeSame = 0, sizeDiff = 0;
    char      cbuffer[MAXCHARLEN];
    sprintf(cbuffer, "AIX[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %s\n",
            communicators->all_rank, communicators->all_rank, communicators->all_size,
            communicators->work_rank, communicators->work_size, communicators->local_rank, communicators->local_size, location);
    collective_print_gather(cbuffer, communicators->all);
  }

#endif

#ifdef _SION_FX
  if (bluegene) {               /* order MPI-tasks by I/O-node */
    int  rank, x, y, z, a, b, c, rc;
    char location[256];
    char cbuffer[MAXCHARLEN];
    int  ionodeid;
    MPI_Comm  commSame, commDiff;
    int       sizeSame = 0, sizeDiff;
    int       rankSame = 0, rankDiff;

    gethostname(location, 256);
    MPI_Comm_rank(MPI_COMM_WORLD, &rank);
    rc=FJMPI_Topology_sys_rank2xyzabc(rank, &x, &y, &z, &a, &b, &c);
    
    ionodeid=x * 65536 + y;
    
    if(bluegene>0) {
    /* per ION */
    /* communicator consists of all task working with the same I/O-node */
      MPI_Comm_split(communicators->all, ionodeid, communicators->all_rank, &commSame);
      MPI_Comm_size(commSame, &sizeSame);
      MPI_Comm_rank(commSame, &rankSame);
      
      /* communicator consists of all task working with the different I/O-nodes */
      MPI_Comm_split(communicators->all, rankSame, ionodeid, &commDiff);
      MPI_Comm_size(commDiff, &sizeDiff);
      MPI_Comm_rank(commDiff, &rankDiff);
      
      communicators->ionode_number = rankDiff;

    } else {
      bluegene_np = sizeSame;
    }
    
    /*  Get a communicator with all writing tasks => new global communicator */
    MPI_Comm_split(communicators->all, (rankSame < bluegene_np), communicators->all_rank, &communicators->work);
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
    if (rankSame >= bluegene_np) {
      /* not working task */
      communicators->work_size = communicators->work_rank = -1;
      communicators->local_size = communicators->local_rank = -1;
    }
    
    /*  If only one file will be used => dont split further */
    /*  if numfile > 1 sion will generate correct local communicator */
    if (numfiles >= 1) {
      communicators->local = communicators->work;
    }
    else if(numfiles<0) {
      if(numfiles==-1) {
	/*  Split the common communicator for each IO node to get a local comm with only the writing tasks for this IO Node */
	MPI_Comm_split(commSame, (rankSame < bluegene_np), communicators->all_rank, &communicators->local);
      } else {
	/*  local communicator contains only one task per IO-node */
	/* bluegene_np has to be 512 */
	communicators->local=commDiff;
      }
    } 
    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);
    
    /* determine filenumber */
    if (numfiles < 1) {
      /* one file per I/O-node */
      if(numfiles==-1)  communicators->file_number = rankDiff;
      else              communicators->file_number = rankSame;
    }
    else {
      communicators->file_number = -1;
    }

    /* print log message about location, ...  */
    sprintf(cbuffer, "");
    if (rankSame < bluegene_np) {
      sprintf(cbuffer, "FX[%03d] diff_comm: %4d of %4d  same_comm: %4d of %4d file_comm: %4d of %4d %dx%dx%d %dx%dx%d %s ioid=%d ion=%d rc=%d numfiles=%d\n",
	      
	      communicators->all_rank, rankDiff, sizeDiff, rankSame, sizeSame,
	      communicators->local_rank, communicators->local_size, 
	      x,y,z,a,b,c, location, ionodeid, communicators->ionode_number, rc, numfiles);
    }
    collective_print_gather(cbuffer, communicators->all);

  }
#endif
                                                                                                          

  /* initial set of communicators not changed? */
  if (communicators->work_size == -2) {
    /* all task do work */
    communicators->work = communicators->all;
    MPI_Comm_size(communicators->work, &communicators->work_size);
    MPI_Comm_rank(communicators->work, &communicators->work_rank);
  }
  /* local communicators  */
  if (communicators->local_size == -2) {
    if (numfiles == 1) {
      communicators->local = communicators->work;
    }
    /* set a default distribution on files, will be computed again by sion_open */
    if (numfiles < 1) {
      numfiles = communicators->work_size / 2;
      if (numfiles == 0)
        numfiles = 1;
    }
    proc_per_file = communicators->work_size / numfiles;

    /* remaining tasks are write/read to/from the last file */
    if (communicators->work_rank >= (numfiles * proc_per_file)) {
      communicators->file_number = numfiles - 1;
    }
    else {
      communicators->file_number = communicators->work_rank / proc_per_file;
    }

    MPI_Comm_split(communicators->work, communicators->file_number, communicators->all_rank, &communicators->local);

    MPI_Comm_size(communicators->local, &communicators->local_size);
    MPI_Comm_rank(communicators->local, &communicators->local_rank);

    communicators->ionode_number = communicators->file_number;

  }

  /* shift working tasks  */
  if (communicators->work_size != -1) {
    /* only if task in communicator work */
    int newtasknr;
    newtasknr=(communicators->work_rank+read_task_offset)%communicators->work_size;
    MPI_Comm_split(communicators->work, 0, newtasknr, &communicators->workread);

    MPI_Comm_size(communicators->workread, &communicators->workread_size);
    MPI_Comm_rank(communicators->workread, &communicators->workread_rank);
    /*    printf("WF: %d %d %% %d-> %d (%d %d)\n",
	   communicators->work_rank,read_task_offset,
	   communicators->work_size,newtasknr,
	   communicators->workread_rank,communicators->workread_size);*/
   
  } else {
    /* this rtask will not be used for reading */
    communicators->workread_size = communicators->workread_rank = -1;
    communicators->local_size = communicators->local_rank = -1;
  }

  return(1);
}

