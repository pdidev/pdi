#include "sion_const.h"

************************************
** Common block for f77 interface **
************************************
*
* @file sion_f77.h
*
* @brief Common block for Fortran77 interface
*
* @author Florian Janetzko
* @date 29.05.2013
*

****************************
* SION version information *
****************************
      INTEGER sion_main_version,sion_sub_version,sion_version_patchlevel
      INTEGER sion_fileformat_version
      PARAMETER (sion_main_version       = SION_MAIN_VERSION,
     &           sion_sub_version        = SION_SUB_VERSION,
     &           sion_version_patchlevel = SION_VERSION_PATCHLEVEL,
     &           sion_fileformat_version = SION_FILEFORMAT_VERSION)

******************
* SION constants *
******************
      INTEGER sion_current_rank,sion_current_blk,sion_current_chunk
      INTEGER sion_current_pos,sion_absolute_pos, sion_success
      PARAMETER (sion_current_rank  = SION_CURRENT_RANK,
     &           sion_current_blk   = SION_CURRENT_BLK,
     &           sion_current_chunk = SION_CURRENT_CHUNK,
     &           sion_current_pos   = SION_CURRENT_POS,
     &           sion_absolute_pos  = SION_ABSOLUTE_POS,
     &           sion_success       = SION_SUCCESS)
