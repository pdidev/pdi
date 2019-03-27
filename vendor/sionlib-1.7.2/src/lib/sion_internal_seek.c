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
#include <stdarg.h>
#include <string.h>
#include <time.h>

#include <sys/time.h>

#include "sion.h"
#include "sion_debug.h"
#include "sion_error_handler.h"
#include "sion_internal.h"
#include "sion_metadata.h"
#include "sion_filedesc.h"
#include "sion_fd.h"
#include "sion_file.h"
#include "sion_printts.h"
#include "sion_buffer.h"

#include "sion_internal_seek.h"


#define DFUNCTION "_sion_seek_on_all_ranks_read"
/*!
 * @brief Function to set the file pointer to a new position, updates internal data structure.
 *
 * This function works on a SINGLE SION file
 *
 * @param[in,out]  sion_filedesc  sion data structure
 * @param[in]      rank           rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr        block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk       position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_all_ranks_read( _sion_filedesc *sion_filedesc,
                                  int  rank,
                                  int  blocknr,
                                  sion_int64 posinblk ) {
  int        rc = SION_SUCCESS;
  int        blknum;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",
            rank,blocknr, (long) posinblk,sion_filedesc->fname));

  if ((sion_filedesc->all_blockcount == NULL)
      || (sion_filedesc->all_blocksizes == NULL)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: internal error, data structure not initialized, aborting ...\n"));
  }

  /* check if RANK changed */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc->rank) ) {

    DPRINTFP((32, DFUNCTION, -1, "rank has changed %d -> %d\n", sion_filedesc->rank, rank));

    if ((rank<0) || (rank >= sion_filedesc->ntasks)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d (max. %d) out of range, aborting ...\n",
                              rank, sion_filedesc->ntasks));
    }

    /* check and update current position */
    _sion_update_fileposition(sion_filedesc);

    /* store current position in all_* vectors */
    sion_filedesc->all_currentpos[sion_filedesc->rank]     = sion_filedesc->currentpos;
    sion_filedesc->all_currentblocknr[sion_filedesc->rank] = sion_filedesc->currentblocknr;

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc->all_keyvalptr[sion_filedesc->rank] = sion_filedesc->keyvalptr;

    /* switch to new rank and restore current position of this rank */
    sion_filedesc->rank           = rank;
    sion_filedesc->currentblocknr = sion_filedesc->all_currentblocknr[sion_filedesc->rank];
    sion_filedesc->currentpos     = sion_filedesc->all_currentpos[sion_filedesc->rank];
    sion_filedesc->lastchunknr    = sion_filedesc->all_blockcount[sion_filedesc->rank]-1;
    sion_filedesc->startpos       = sion_filedesc->all_startpointers[sion_filedesc->rank];
    sion_filedesc->chunksize      = sion_filedesc->all_chunksizes[sion_filedesc->rank];

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc->keyvalptr=sion_filedesc->all_keyvalptr[sion_filedesc->rank];

    for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
      sion_filedesc->blocksizes[blknum] = sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + sion_filedesc->rank];
    }

    /* /\* do nothing on empty blocks *\/ */
    /* if (!sion_filedesc->lastchunknr) { */
    /*   rc = SION_NOT_SUCCESS; */
    /*   DPRINTFP((2, DFUNCTION, -1, "leave seek rc=%d\n",rc)); */
    /*   return rc; */
    /* } */

    /* rank has changed, therefore this information could not get from fileposition */
    if (blocknr == SION_CURRENT_BLK) {
      blocknr=sion_filedesc->currentblocknr;
    }
    if (posinblk == SION_CURRENT_POS) {
      posinblk=sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);

      if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE) {

        /* check if just behind current block, go to next block */
        if(posinblk>=sion_filedesc->blocksizes[blocknr]) {
          posinblk=0;
          /* do no more checks on empty blocks */
          if (sion_filedesc->lastchunknr) {
            blocknr++;
            if(blocknr > sion_filedesc->lastchunknr) {
              return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: seek after end of file, returning  ...\n"));
            }
          }
        }

      }
    }

  }

  /* seek new position in current rank, sets in all cases the filepointer  */
  rc=_sion_seek_on_current_rank_read(sion_filedesc,SION_CURRENT_RANK,blocknr,posinblk);

  DPRINTFP((2, DFUNCTION, -1, "leave seek rc=%d\n",rc));

  return(rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_on_all_ranks_read_master"
/*!
 * @brief Function to set the file pointer to a new position, updates internal data structure.
 *
 * This function works on Multi-files
 *
 * @param[in,out]  sion_filedesc  sion data structure
 * @param[in]      rank           rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr        block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk       position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_all_ranks_read_master( _sion_filedesc *sion_filedesc,
                                         int  rank,
                                         int  blocknr,
                                         sion_int64 posinblk ) {
  int        rc = SION_SUCCESS;
  int        blknum, lfile, lrank;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",
            rank,blocknr, (long) posinblk,sion_filedesc->fname));

#if 0
  if ((sion_filedesc->all_blockcount == NULL)
      || (sion_filedesc->all_blocksizes == NULL)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: internal error, data structure not initialized, aborting ...\n"));
  }
#endif

  /* lookup file which contains current rank */
  lfile=sion_filedesc->mapping[sion_filedesc->rank*2+0];
  lrank=sion_filedesc->mapping[sion_filedesc->rank*2+1];

  /* check if RANK changed */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc->rank) ) {

    if ((rank<0) || (rank >= sion_filedesc->ntasks)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d (max. %d) out of range, aborting ...\n",
                              rank, sion_filedesc->ntasks));
    }

    /* check and update current position */
    _sion_update_fileposition(sion_filedesc);

    /* store current position in all_* vectors */
    sion_filedesc->multifiles[lfile]->all_currentpos[lrank]     = sion_filedesc->currentpos;
    sion_filedesc->multifiles[lfile]->all_currentblocknr[lrank] = sion_filedesc->currentblocknr;

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc->multifiles[lfile]->all_keyvalptr[lrank] = sion_filedesc->keyvalptr;


    /* switch to new rank and restore current position of this rank */
    sion_filedesc->rank           = rank;

    /* lookup again file which contains current rank */
    lfile=sion_filedesc->mapping[sion_filedesc->rank*2+0];
    lrank=sion_filedesc->mapping[sion_filedesc->rank*2+1];
    

    sion_filedesc->currentblocknr = sion_filedesc->multifiles[lfile]->all_currentblocknr[lrank];
    sion_filedesc->currentpos     = sion_filedesc->multifiles[lfile]->all_currentpos[lrank];
    sion_filedesc->lastchunknr    = sion_filedesc->multifiles[lfile]->all_blockcount[lrank]-1;
    sion_filedesc->startpos       = sion_filedesc->multifiles[lfile]->all_startpointers[lrank];
    sion_filedesc->chunksize      = sion_filedesc->multifiles[lfile]->all_chunksizes[lrank];
    sion_filedesc->globalskip     = sion_filedesc->multifiles[lfile]->globalskip;
    for (blknum = 0; blknum < sion_filedesc->multifiles[lfile]->all_blockcount[lrank]; blknum++) {
      sion_filedesc->blocksizes[blknum] = sion_filedesc->multifiles[lfile]->all_blocksizes[sion_filedesc->multifiles[lfile]->ntasks * blknum + lrank];
    }
    sion_filedesc->fileptr        = sion_filedesc->multifiles[lfile]->fileptr;

    DPRINTFP((32, DFUNCTION, -1, "switch to file %d and lrank %d currentpos=%ld, currentblocknr=%d\n",lfile, lrank,(long) sion_filedesc->currentpos, sion_filedesc->currentblocknr ));

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc->keyvalptr=sion_filedesc->multifiles[lfile]->all_keyvalptr[lrank];

    /* set rank info in sub-file */
    sion_filedesc->multifiles[lfile]->rank=lrank;

    /* rank has changed, therefore this information could not get from fileposition */
    if (blocknr == SION_CURRENT_BLK) {
      blocknr=sion_filedesc->currentblocknr;
    }
    if (posinblk == SION_CURRENT_POS) {
      posinblk=sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);

      if(sion_filedesc->keyvalmode==SION_KEYVAL_NONE) {

        /* check if just behind current block, go to next block */
        if(posinblk>=sion_filedesc->blocksizes[blocknr]) {
          posinblk=0;blocknr++;
          if(blocknr > sion_filedesc->lastchunknr) {
            return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: seek after end of file, returning  ...\n"));
          }
        }

      }
    }

  }

  /* seek new position in current rank in sub-file, sets in all cases the filepointer  */
  rc=_sion_seek_on_current_rank_read(sion_filedesc,SION_CURRENT_RANK,blocknr,posinblk);

  DPRINTFP((2, DFUNCTION, -1, "leave seek rc=%d\n",rc));

  return(rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_seek_on_current_rank_read"
/*!
 * @brief Function to set the file pointer to a new position in the same rank, updates internal data structure.
 *
 * @param[in,out]  sion_filedesc  sion data structure
 * @param[in]      rank           rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr        block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk       position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_current_rank_read( _sion_filedesc *sion_filedesc,
                                     int  rank,
                                     int  blocknr,
                                     sion_int64 posinblk ) {
  int rc = SION_SUCCESS;
  int        newblocknr = -1;
  sion_int64 newposinblk=-1;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",rank,blocknr, (long) posinblk,sion_filedesc->fname));


  /* check RANK */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc->rank) ) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: parameter rank is different from current rank in parallel openened file, returning  ...\n"));
  }

  /* check requested BLOCK NUMBER */
  if (blocknr == SION_ABSOLUTE_POS) {
    /* search absolute position */
    if(!_sion_seek_search_abs_pos(sion_filedesc,posinblk,&newblocknr,&newposinblk)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN, "sion_seek: error in searching abs pos, returning  ...\n"));
    } else posinblk=newposinblk; /* for checking posinblk later */
    DPRINTFP((32, DFUNCTION, -1, "sion_absolute_pos  newblocknr=%d newpos=%ld fn=%s\n",newblocknr, (long) newposinblk, sion_filedesc->fname));
  }
  else if (blocknr == SION_END_POS) {
    /* search position relative to end */
    if (!_sion_seek_search_end_pos(sion_filedesc, posinblk, &newblocknr, &newposinblk)) {
      return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN, "sion_seek: error in searching end pos, returning  ...\n");
    }
    else { posinblk = newposinblk;   /* for checking posinblk later */
    }
    DPRINTFP((32, DFUNCTION, -1, "sion_end_pos  newblocknr=%d newpos=%ld fn=%s\n", newblocknr, (long)newposinblk, sion_filedesc->fname));
  }
  else {
    if (blocknr == SION_CURRENT_BLK) {
      newblocknr=sion_filedesc->currentblocknr;
      DPRINTFP((32, DFUNCTION, -1, "sion_current_blk  newblocknr=%d newpos=%ld fn=%s\n",newblocknr, (long) newposinblk, sion_filedesc->fname));
    } else {
      /* a blocknr is specified */
      if ( (blocknr >= 0) && (blocknr <= sion_filedesc->lastchunknr) ) {
        newblocknr=blocknr;
      } else {
        return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                                "sion_seek: parameter chunk number (%d) is out of range (0 .. %d), returning  ...\n",
                                blocknr,sion_filedesc->lastchunknr));
      }
      DPRINTFP((32, DFUNCTION, -1, "new blocknr  newblocknr=%d newpos=%ld fn=%s\n",newblocknr, (long) newposinblk, sion_filedesc->fname));
    }
  }


  /* check requested POSITION IN BLOCK */
  if (posinblk == SION_CURRENT_POS) {
    _sion_update_fileposition(sion_filedesc);
    newposinblk=sion_filedesc->currentpos - (sion_filedesc->startpos + sion_filedesc->currentblocknr * sion_filedesc->globalskip);
    DPRINTFP((32, DFUNCTION, -1, "sion_current_pos  newblocknr=%d newpos=%ld fn=%s %ld   %ld %ld %ld\n",newblocknr, (long) newposinblk, sion_filedesc->fname,
              (long) sion_filedesc->currentpos, (long) sion_filedesc->startpos,(long) sion_filedesc->currentblocknr,(long) sion_filedesc->globalskip));
  } else {
    /* a posinblk is specified */
    if ( (sion_filedesc->keyvalmode!=SION_KEYVAL_NONE)  || /* allow position outside block for keyval, file could already be scanned completly */
         ( (posinblk >= 0) && (posinblk <= sion_filedesc->blocksizes[newblocknr]) )
         )
      {
        newposinblk=posinblk;
      } else {
      DPRINTFP((2, DFUNCTION, -1, "sion_seek: parameter posinblk (%lld) is out of range (0 .. %lld), aborting ...\n",
                posinblk, sion_filedesc->blocksizes[newblocknr]));
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                              "sion_seek: parameter posinblk (%lld) is out of range (0 .. %lld), aborting ...\n",
                              posinblk, sion_filedesc->blocksizes[newblocknr]));
    }
    DPRINTFP((2, DFUNCTION, -1, "new posinblk  newblocknr=%d newpos=%ld fn=%s\n",newblocknr, (long) newposinblk, sion_filedesc->fname));
  }

  DPRINTFP((32, DFUNCTION, -1,
            " before set pos: rank=%4d startpos=%ld currentblocknr=%d globalskip=%ld newposinblk=%ld\n", sion_filedesc->rank
            ,(long) sion_filedesc->startpos, sion_filedesc->currentblocknr, (long) sion_filedesc->globalskip, (long) newposinblk));

  /* SET NEW POSITION */
  DPRINTFP((32, DFUNCTION, 0, "file pos=%ld (%ld)\n", (long) _sion_file_get_position(sion_filedesc->fileptr), (long) sion_filedesc->currentpos));
  sion_filedesc->currentblocknr = newblocknr;
  sion_filedesc->currentpos     =   sion_filedesc->startpos
                                  + sion_filedesc->currentblocknr * sion_filedesc->globalskip
                                  + newposinblk;
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  DPRINTFP((32, DFUNCTION, -1,
            " set pos: rank=%4d newblocknr=%4d newposinblk=%4ld, set fileptr to position %14ld (%14ld) file=%s\n",
            sion_filedesc->rank, sion_filedesc->currentblocknr, (long) newposinblk,
            (long) _sion_file_get_position(sion_filedesc->fileptr),(long) sion_filedesc->currentpos,
            sion_filedesc->fname));


  DPRINTFP((2, DFUNCTION, -1, "leave seek rc=%d\n",rc));

  return(rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_on_all_ranks_read_mapped"
/*!
 * @brief Function to set the file pointer to a new position, updates internal data structure.
 *
 * This function works on a SINGLE SION file, Multi-files have to be handle in upper layer
 *
 * @param[in,out]  sion_filedesc_master  sion data structure
 * @param[in]      rank                  rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr               block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk              position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_all_ranks_read_mapped( _sion_filedesc *sion_filedesc_master,
                                          int  rank,
                                          int  blocknr,
                                          sion_int64 posinblk ) {
  int rc = SION_SUCCESS;
  int lfile, lrank, blknum, filenr, t;
  _sion_filedesc *sion_filedesc_sub;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",
            rank,blocknr, (long) posinblk,sion_filedesc_master->fname));

  /* check if RANK changed */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc_master->globalrank) ) {

    DPRINTFP((32, DFUNCTION, -1, "rank has changed %d -> %d\n", sion_filedesc_master->globalrank, rank));

    if ((rank<0) || (rank >= sion_filedesc_master->ntotaltasksinfile)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d (max. %d) out of range, aborting ...\n",
                              rank, sion_filedesc_master->ntotaltasksinfile));
    }

    /* check and update current position */
    _sion_update_fileposition(sion_filedesc_master);

    /* transfer meta data to corresponding sub datastructure */
    lfile=sion_filedesc_master->filenumber;
    lrank=sion_filedesc_master->rank; /* index to local list */
    sion_filedesc_sub=sion_filedesc_master->multifiles[lfile];

    sion_filedesc_sub->currentpos     = sion_filedesc_master->currentpos;
    sion_filedesc_sub->currentblocknr = sion_filedesc_master->currentblocknr;

    /* pointer to keyval structure */
    DPRINTFP((32, DFUNCTION, -1, "set keyvalptr on sub to master lfile=%d,lrank=%d to %x\n",lfile,lrank,sion_filedesc_master->keyvalptr));
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc_sub->keyvalptr = sion_filedesc_master->keyvalptr;

    DPRINTFP((32, DFUNCTION, -1, "on file %d: sub,maxchunk=%d master,maxchunk=%d \n", lfile,sion_filedesc_sub->maxchunks, sion_filedesc_master->maxchunks));

    /* store data of current rank on sub datastructure */
    DPRINTFP((32, DFUNCTION, -1, "store current information lrank=%d lastchunknr=%d\n", lrank,sion_filedesc_sub->lastchunknr));
    sion_filedesc_sub->all_currentpos[lrank]     = sion_filedesc_sub->currentpos;
    sion_filedesc_sub->all_currentblocknr[lrank] = sion_filedesc_sub->currentblocknr;

    /* pointer to keyval structure -> all_keyvalptr */
    DPRINTFP((32, DFUNCTION, -1, "keyvalptr sub file=%d all_keyvalptr[%d] = %x\n",lfile,lrank,sion_filedesc_sub->keyvalptr));
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE)  sion_filedesc_sub->all_keyvalptr[lrank] = sion_filedesc_sub->keyvalptr;

    /* lookup file which contains new global rank and set master */
    sion_filedesc_sub=NULL;
    lfile=lrank=-1;
    DPRINTFP((32, DFUNCTION, -1, " nfiles=%d\n", sion_filedesc_master->nfiles));

    for(filenr=0;( (filenr<sion_filedesc_master->nfiles) && (lrank==-1) );filenr++) {
      DPRINTFP((4, DFUNCTION, -1, " filenr=%d nlocaltasksinfile\n",
                filenr,sion_filedesc_master->multifiles[filenr]->nlocaltasksinfile));

      for(t=0;( (t<sion_filedesc_master->multifiles[filenr]->nlocaltasksinfile) && (lrank==-1) );t++) {
        DPRINTFP((4, DFUNCTION, -1, " check filenr=%d t=%d grank=%d with rank=%d\n",
                  filenr,t,sion_filedesc_master->multifiles[filenr]->all_globalranks[t],rank));
        if(sion_filedesc_master->multifiles[filenr]->all_globalranks[t]==rank) {
          sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
          lfile=filenr;
          lrank=t;
          break;
        }
      }
    }
    DPRINTFP((32, DFUNCTION, -1, "grank %d is found in file %d with lrank %d\n", rank,lfile,lrank));
    if((lrank==-1) || (sion_filedesc_sub == NULL)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d is not opened on this tasks (mapped mode), aborting ...\n",
                              rank));
    }

    /* switch to new rank and restore current position of this rank */
    sion_filedesc_master->globalrank     = sion_filedesc_sub->all_globalranks[lrank];
    sion_filedesc_master->rank           = lrank;
    sion_filedesc_master->filenumber     = lfile;
    sion_filedesc_master->currentblocknr = sion_filedesc_sub->all_currentblocknr[lrank];
    sion_filedesc_master->currentpos     = sion_filedesc_sub->all_currentpos[lrank];
    sion_filedesc_master->lastchunknr    = sion_filedesc_sub->all_blockcount[lrank]-1;
    sion_filedesc_master->startpos       = sion_filedesc_sub->all_startpointers[lrank];
    sion_filedesc_master->chunksize      = sion_filedesc_sub->all_chunksizes[lrank];

    /* pointer to keyval structure */
    if(sion_filedesc_master->keyvalmode!=SION_KEYVAL_NONE)  {
      DPRINTFP((32, DFUNCTION, -1, "set keyvalptr on lfile=%d,lrank=%d to %x\n",lfile,lrank,sion_filedesc_sub->all_keyvalptr[lrank]));
      sion_filedesc_master->keyvalptr = sion_filedesc_sub->all_keyvalptr[lrank];
    }

    if(sion_filedesc_sub->maxchunks  > sion_filedesc_master->maxchunks) {
      _sion_realloc_filedesc_blocklist(sion_filedesc_master, sion_filedesc_sub->maxchunks);
    }
    for (blknum = 0; blknum <= sion_filedesc_master->lastchunknr; blknum++) {
      sion_filedesc_master->blocksizes[blknum] = sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->ntasks * blknum + lrank];
    }
    sion_filedesc_master->globalskip     = sion_filedesc_sub->globalskip;
    sion_filedesc_master->fileptr        = sion_filedesc_sub->fileptr;

    /* rank has changed, therefore this information could not get from fileposition */
    if (blocknr == SION_CURRENT_BLK) {
      blocknr=sion_filedesc_master->currentblocknr;
    }
    if (posinblk == SION_CURRENT_POS) {
      posinblk=sion_filedesc_master->currentpos - (sion_filedesc_master->startpos + sion_filedesc_master->currentblocknr * sion_filedesc_master->globalskip);

      if(sion_filedesc_master->keyvalmode==SION_KEYVAL_NONE) {

        /* check if just behind current block, go to next block */
        if(posinblk>=sion_filedesc_master->blocksizes[blocknr]) {
          posinblk=0;blocknr++;
          if(blocknr > sion_filedesc_master->lastchunknr) {
            return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: seek after end of file, returning  ...\n"));

          }
        }

      }
    }

  }

  _sion_print_filedesc(sion_filedesc_master, 512, "_sion_seek_on_all_ranks_read_mapped", 1);

  /* seek new position in current rank, set in all cases the filepointer  */
  _sion_seek_on_current_rank_read(sion_filedesc_master,SION_CURRENT_RANK,blocknr,posinblk);

  DPRINTFP((2, DFUNCTION, -1, "leave seek\n"));

  return(rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_seek_on_all_ranks_write"
/*!
 * @brief Function to set the file pointer to a new position, updates internal data structure.
 *
 * This function works on a SINGLE SION file, Multi-files have to be handle in upper layer
 *
 * @param[in,out]  sion_filedesc  sion data structure
 * @param[in]      rank           rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr        block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk       position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_all_ranks_write( _sion_filedesc *sion_filedesc,
                                   int  rank,
                                   int  blocknr,
                                   sion_int64 posinblk ) {
  int rc = SION_SUCCESS;
  int        blknum;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",
            rank,blocknr, (long) posinblk,sion_filedesc->fname));

  if ((sion_filedesc->all_blockcount == NULL)
      || (sion_filedesc->all_blocksizes == NULL)) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: internal error, data structure not initialized, aborting ...\n"));
  }

  /* check if RANK changed */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc->rank) ) {

    DPRINTFP((32, DFUNCTION, -1, "rank has changed %d -> %d\n", sion_filedesc->rank, rank));

    if ((rank<0) || (rank >= sion_filedesc->ntasks)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d (max. %d) out of range, aborting ...\n",
                              rank, sion_filedesc->ntasks));
    }

    /* flush write buffer */
    if (sion_filedesc->usebuffer) {
      _sion_buffer_flush(sion_filedesc);
    }

    /* update meta data of current rank */
    _sion_flush_block(sion_filedesc);

    /* store current position in all_* vectors */
    DPRINTFP((32, DFUNCTION, -1, "store data for rank %d currentpos=%d currentblocknr=%d lastchunknr=%d\n", sion_filedesc->rank,
              (int) sion_filedesc->currentpos,
              (int) sion_filedesc->currentblocknr,
              (int) sion_filedesc->lastchunknr  ));
    sion_filedesc->all_currentpos[sion_filedesc->rank]     = sion_filedesc->currentpos;
    sion_filedesc->all_currentblocknr[sion_filedesc->rank] = sion_filedesc->currentblocknr;

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc->all_keyvalptr[sion_filedesc->rank] = sion_filedesc->keyvalptr;

    sion_filedesc->all_blockcount[sion_filedesc->rank]     = sion_filedesc->lastchunknr + 1;
    for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
      sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + sion_filedesc->rank] = sion_filedesc->blocksizes[blknum];
      sion_filedesc->blocksizes[blknum]=0; /* reset entry */
    }

    /* switch to new rank and restore current position of this rank */
    sion_filedesc->rank           = rank;
    sion_filedesc->currentblocknr = sion_filedesc->all_currentblocknr[sion_filedesc->rank];
    sion_filedesc->currentpos     = sion_filedesc->all_currentpos[sion_filedesc->rank];
    sion_filedesc->lastchunknr    = sion_filedesc->all_blockcount[sion_filedesc->rank]-1;
    sion_filedesc->startpos       = sion_filedesc->all_startpointers[sion_filedesc->rank];
    sion_filedesc->chunksize      = sion_filedesc->all_chunksizes[sion_filedesc->rank];

    /* pointer to keyval structure */
    if(sion_filedesc->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc->keyvalptr = sion_filedesc->all_keyvalptr[sion_filedesc->rank];

    for (blknum = 0; blknum <= sion_filedesc->lastchunknr; blknum++) {
      sion_filedesc->blocksizes[blknum] = sion_filedesc->all_blocksizes[sion_filedesc->ntasks * blknum + sion_filedesc->rank];
    }

  }

  /* seek new position in current rank, set in all cases the filepointer  */
  _sion_seek_on_current_rank_write(sion_filedesc,SION_CURRENT_RANK,blocknr,posinblk);

  DPRINTFP((2, DFUNCTION, -1, "leave seek\n"));

  return(rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_on_all_ranks_write_mapped"
/*!
 * @brief Function to set the file pointer to a new position, updates internal data structure.
 *
 * This function works on a SINGLE SION file, Multi-files have to be handle in upper layer
 *
 * @param[in,out]  sion_filedesc_master  sion data structure
 * @param[in]      rank                  rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr               block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk              position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_all_ranks_write_mapped( _sion_filedesc *sion_filedesc_master,
                                          int  rank,
                                          int  blocknr,
                                          sion_int64 posinblk ) {
  int rc = SION_SUCCESS;
  int lfile, lrank, blknum, filenr, t;
  _sion_filedesc *sion_filedesc_sub;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",
            rank,blocknr, (long) posinblk,sion_filedesc_master->fname));

  /* check if RANK changed */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc_master->globalrank) ) {

    DPRINTFP((32, DFUNCTION, -1, "rank has changed %d -> %d\n", sion_filedesc_master->globalrank, rank));

    if ((rank<0) || (rank >= sion_filedesc_master->ntotaltasksinfile)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d (max. %d) out of range, aborting ...\n",
                              rank, sion_filedesc_master->ntotaltasksinfile));
    }

    /* flush write buffer */
    if (sion_filedesc_master->usebuffer) {
      _sion_buffer_flush(sion_filedesc_master);
    }

    /* update meta data of current rank */
    _sion_flush_block(sion_filedesc_master);

    /* transfer meta data to corresponding sub datastructure */
    lfile=sion_filedesc_master->filenumber;
    lrank=sion_filedesc_master->rank; /* index to local list */
    sion_filedesc_sub=sion_filedesc_master->multifiles[lfile];

    sion_filedesc_sub->currentpos     = sion_filedesc_master->currentpos;
    sion_filedesc_sub->currentblocknr = sion_filedesc_master->currentblocknr;
    sion_filedesc_sub->lastchunknr    = sion_filedesc_master->lastchunknr;

    /* pointer to keyval structure */
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_sub->keyvalptr = sion_filedesc_master->keyvalptr;

    DPRINTFP((32, DFUNCTION, -1, "on file %d: sub,maxchunk=%d master,maxchunk=%d \n", lfile,sion_filedesc_sub->maxchunks, sion_filedesc_master->maxchunks));
    if(sion_filedesc_sub->maxchunks  < sion_filedesc_master->maxchunks) {
      _sion_realloc_filedesc_blocklist(sion_filedesc_sub, sion_filedesc_master->maxchunks);
    }

    /* store data of current rank on sub datastructure */
    DPRINTFP((32, DFUNCTION, -1, "store current information lrank=%d lastchunknr=%d\n", lrank,sion_filedesc_sub->lastchunknr));
    sion_filedesc_sub->all_currentpos[lrank]     = sion_filedesc_sub->currentpos;
    sion_filedesc_sub->all_currentblocknr[lrank] = sion_filedesc_sub->lastchunknr;
    sion_filedesc_sub->all_blockcount[lrank]     = sion_filedesc_sub->lastchunknr + 1;

    /* pointer to keyval structure */
    if(sion_filedesc_sub->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_sub->all_keyvalptr[lrank]     = sion_filedesc_sub->keyvalptr;

    for (blknum = 0; blknum <= sion_filedesc_sub->lastchunknr; blknum++) {
      sion_filedesc_sub->blocksizes[blknum] = sion_filedesc_master->blocksizes[blknum];
      sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->ntasks * blknum + lrank] = sion_filedesc_master->blocksizes[blknum];
    }

    /* lookup file which contains new global rank and set master */
    sion_filedesc_sub=NULL;
    lfile=lrank=-1;
    DPRINTFP((32, DFUNCTION, -1, " nfiles=%d\n", sion_filedesc_master->nfiles));

    for(filenr=0;( (filenr<sion_filedesc_master->nfiles) && (lrank==-1) );filenr++) {
      DPRINTFP((32, DFUNCTION, -1, " filenr=%d nlocaltasksinfile\n",
                filenr,sion_filedesc_master->multifiles[filenr]->nlocaltasksinfile));

      for(t=0;( (t<sion_filedesc_master->multifiles[filenr]->nlocaltasksinfile) && (lrank==-1) );t++) {
        DPRINTFP((32, DFUNCTION, -1, " check filenr=%d t=%d grank=%d with rank=%d\n",
                  filenr,t,sion_filedesc_master->multifiles[filenr]->all_globalranks[t],rank));
        if(sion_filedesc_master->multifiles[filenr]->all_globalranks[t]==rank) {
          sion_filedesc_sub=sion_filedesc_master->multifiles[filenr];
          lfile=filenr;
          lrank=t;
          break;
        }
      }
    }
    DPRINTFP((32, DFUNCTION, -1, "grank %d is found in file %d with lrank %d\n", rank,lfile,lrank));
    if((lrank==-1) || (sion_filedesc_sub == NULL)) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,"sion_seek: parameter rank %d is not opened on this tasks (mapped mode), aborting ...\n",
                              rank));
    }

    /* switch to new rank and restore current position of this rank */
    sion_filedesc_master->globalrank     = sion_filedesc_sub->all_globalranks[lrank];
    sion_filedesc_master->rank           = lrank;
    sion_filedesc_master->filenumber     = lfile;
    sion_filedesc_master->currentblocknr = sion_filedesc_sub->all_currentblocknr[lrank];
    sion_filedesc_master->currentpos     = sion_filedesc_sub->all_currentpos[lrank];
    sion_filedesc_master->lastchunknr    = sion_filedesc_sub->all_blockcount[lrank]-1;
    sion_filedesc_master->startpos       = sion_filedesc_sub->all_startpointers[lrank];
    sion_filedesc_master->chunksize      = sion_filedesc_sub->all_chunksizes[lrank];

    /* pointer to keyval structure */
    if(sion_filedesc_master->keyvalmode!=SION_KEYVAL_NONE) sion_filedesc_master->keyvalptr     = sion_filedesc_sub->all_keyvalptr[lrank];

    if(sion_filedesc_sub->maxchunks  > sion_filedesc_master->maxchunks) {
      _sion_realloc_filedesc_blocklist(sion_filedesc_master, sion_filedesc_sub->maxchunks);
    }
    for (blknum = 0; blknum <= sion_filedesc_sub->lastchunknr; blknum++) {
      sion_filedesc_master->blocksizes[blknum] = sion_filedesc_sub->all_blocksizes[sion_filedesc_sub->ntasks * blknum + lrank];
    }

    sion_filedesc_master->globalskip     = sion_filedesc_sub->globalskip;
    sion_filedesc_master->fileptr        = sion_filedesc_sub->fileptr;

  }


  _sion_print_filedesc(sion_filedesc_master, 512, "_sion_seek_on_all_ranks_write_mapped", 1);

  /* seek new position in current rank, set in all cases the filepointer  */
  _sion_seek_on_current_rank_write(sion_filedesc_master,SION_CURRENT_RANK,blocknr,posinblk);

  DPRINTFP((2, DFUNCTION, -1, "leave seek\n"));

  return(rc);

}
#undef DFUNCTION


#define DFUNCTION "_sion_seek_on_current_rank_write"
/*!
 * @brief Function to set the file pointer to a new position in the same rank, updates internal data structure.
 *
 * @param[in,out]  sion_filedesc  sion data structure
 * @param[in]      rank           rank number of the process (SION_CURRENT_RANK to select the current rank)
 * @param[in]      blocknr        block number (SION_CURRENT_BLK to select the current block)
 * @param[in]      posinblk       position in the block (SION_CURRENT_POS to select the current position)
 *
 * @retval      1 if file pointer can be moved to new position
 *              0 otherwise
 */
int _sion_seek_on_current_rank_write( _sion_filedesc *sion_filedesc,
                                      int  rank,
                                      int  blocknr,
                                      sion_int64 posinblk ) {
  int rc = SION_SUCCESS;

  DPRINTFP((2, DFUNCTION, -1, "enter seek r=%d b=%d p=%ld fn=%s\n",rank,blocknr, (long) posinblk,sion_filedesc->fname));

  /* check RANK */
  if ( (rank != SION_CURRENT_RANK) && (rank != sion_filedesc->rank) ) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: parameter rank is different from current rank in parallel openened file, returning  ...\n"));
  }

  /* check requested BLOCK NUMBER */
  if (blocknr != SION_CURRENT_BLK) {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: serial write to file currently only for SION_CURRENT_BLK implemented, aborting ...\n"));
  }

  /* check requested POSITION IN BLOCK */
  if (posinblk != SION_CURRENT_POS) {
      return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                              "sion_seek: serial write to file currently only for SION_CURRENT_POS implemented, aborting ...\n"));
  }

  /* flush write buffer */
  if (sion_filedesc->usebuffer) {
    _sion_buffer_flush(sion_filedesc);
  }

  /* SET NEW POSITION */
  /* currently not necessary, due to that movements
     inside rank currently not allowed for write-mode*/

  /* SET NEW POSITION */
  _sion_file_purge(sion_filedesc->fileptr);
  _sion_file_set_position(sion_filedesc->fileptr, sion_filedesc->currentpos);

  DPRINTFP((2, DFUNCTION, -1, "leave seek\n"));

  return(rc);

}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_search_abs_pos"
/*!
 * @brief Find block and position inside block for absolute position `abspos`.
 *
 * @param[in]    sion_filedesc   sion data structure
 * @param[in]    abspos          absolut position in file
 * @param[out]   newblocknr      block number for absolut position
 * @param[out]   newposinblk     position in the block for absolut position
 *
 * @retval      1 if abspos lays inside available data
 *              0 otherwise
 */
int _sion_seek_search_abs_pos( _sion_filedesc *sion_filedesc,
                               sion_int64      abspos,
                               int            *newblocknr,
                               sion_int64     *newposinblk ) {
  int rc = SION_SUCCESS;
  sion_int64 bytes=-1;
  sion_int64 bytesinblock = 0;

  if (abspos>=0) {
    *newposinblk=abspos;
    bytes=0;*newblocknr=0;
    bytesinblock = sion_filedesc->blocksizes[*newblocknr];
    while ( (*newblocknr<sion_filedesc->lastchunknr) && (bytes + bytesinblock < *newposinblk) ) {
      bytes += bytesinblock;
      (*newblocknr)++;
      bytesinblock = sion_filedesc->blocksizes[*newblocknr];
    }
    *newposinblk-=bytes;
  } else {
    return(_sion_errorprint(SION_NOT_SUCCESS,_SION_ERROR_RETURN,
                            "sion_seek: parameter pos in chunk (%d) is negative, aborting  ...\n", (int) abspos));
  }


  return(rc);
}
#undef DFUNCTION

#define DFUNCTION "_sion_seek_search_end_pos"
/*!
 * @brief Find block and position inside block position relative to end.
 *
 * @param[in]    sion_filedesc   sion data structure
 * @param[in]    posend          position in file relative to end (-1 seeks before last byte)
 * @param[out]   newblocknr      block number for new position
 * @param[out]   newposinblk     position in the block for new position
 *
 * @retval      1 if posend lays inside available data
 *              0 otherwise
 */
int _sion_seek_search_end_pos(_sion_filedesc* sion_filedesc,
                              sion_int64      posend,
                              int*            newblocknr,
                              sion_int64*     newposinblk)
{
  int rc = SION_SUCCESS;

  if (posend <= 0) {
    *newposinblk = posend;
    *newblocknr = sion_filedesc->lastchunknr;
    do {
      *newposinblk += sion_filedesc->blocksizes[*newblocknr];
      (*newblocknr)--;
    }
    while ((*newblocknr >= 0) && (*newposinblk < 0));
    /* Add one after loop */
    (*newblocknr)++;
    if (*newposinblk < 0) {
      return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN,
                              "sion_seek: seek before beginning of file (posend = %ld, abspos = %ld), aborting  ...\n", posend, *newposinblk);
    }
  }
  else {
    return _sion_errorprint(SION_NOT_SUCCESS, _SION_ERROR_RETURN,
                            "sion_seek: seek past end of file (%ld > 0), aborting  ...\n", posend);
  }


  return rc;
}
#undef DFUNCTION
