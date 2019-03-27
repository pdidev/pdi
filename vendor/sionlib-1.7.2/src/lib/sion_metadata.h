/****************************************************************************
**  SIONLIB     http://www.fz-juelich.de/jsc/sionlib                       **
*****************************************************************************
**  Copyright (c) 2008-2018                                                **
**  Forschungszentrum Juelich, Juelich Supercomputing Centre               **
**                                                                         **
**  See the file COPYRIGHT in the package base directory for details       **
****************************************************************************/
#ifndef SION_SION_METADATA_H
#define SION_SION_METADATA_H

#include "sion_const.h"

#define SION_ID "sion"  /*!< Sion identification string (offset: 0x00) */

/*!
  \file
  example meta data format

  \code
  Metadata 1:
   
  M11: SIONSTR                          4 bytes
       endianness                       sion_uint32
       sion_version                     sion_int32 (not in data structure)
       sion_version_patchlevel          sion_int32 (not in data structure)
       sion_fileformat_version          sion_int32
       fsblksize                        sion_int32
       ntasks                           sion_int32
       nfiles                           sion_int32
       filenumber                       sion_int32
       flag1                            sion_int64 
       flag2                            sion_int64
       prefix                           SION_FILENAME_LENGTH 

  M12: globalranks(ntasks)              ntasks*sion_int64
       chunksizes(ntasks)               ntasks*sion_int64 

  M13: maxusedchunks                    sion_int32
       start_of_varheader               sion_int64

  Metadata 2: (starts at sion_filedesc->start_of_varheader)

  M21: all_blockcount                   ntasks*sion_int64
       all_blocksizes                   ntasks*maxusedchunks*sion_int64

  M22: mapping_size                     sion_int32
       mapping                          mapping_size*2**sion_int32
  \endcode

 */

#include "sion_datatypes.h"
#include "sion_filedesc.h"

/* WRITE */
int _sion_write_header(_sion_filedesc *sion_filedesc );                            /* writes M11, M12  */
int _sion_write_header_var_info(_sion_filedesc *sion_filedesc );                   /* writes M13  */
int _sion_write_header_var_part_blocksizes(_sion_filedesc *sion_filedesc );        /* writes M21  */
int _sion_write_header_var_part_mapping(_sion_filedesc *sion_filedesc, 
					sion_int32 mapping_size, 
					sion_int32 *mapping  );                    /* writes M22  */

/* READ */
int _sion_read_header_fix_part( _sion_filedesc *sion_filedesc );                   /* reads  M11 */
int _sion_read_header_var_part( _sion_filedesc *sion_filedesc );                   /* reads  M12, M13 */
int _sion_read_header_var_part_blocksizes( _sion_filedesc *sion_filedesc );        /* reads  M21 */
int _sion_read_header_var_part_blocksizes_rank(_sion_filedesc *sion_filedesc);     /* reads part of M21 (one rank) */
int _sion_read_header_var_part_mapping( _sion_filedesc *sion_filedesc );           /* reads  M22 */
int _sion_read_header_var_part_mapping_rank(_sion_filedesc *sion_filedesc);        /* reads part of M22 (one rank) */

/* for parallel support, don't read all blocksizes of all tasks in memory */
int _sion_write_header_var_part_blockcount_from_field( _sion_filedesc *sion_filedesc,
						    int             field_size, 
						    sion_int64     *field         );
int _sion_write_header_var_part_nextblocksizes_from_field( _sion_filedesc *sion_filedesc,
							int             field_size, 
							sion_int64     *field           );

int _sion_read_header_var_part_blockcount_to_field( _sion_filedesc *sion_filedesc,
						    int             field_size, 
						    sion_int64     *field         );
int _sion_read_header_var_part_nextblocksizes_to_field( _sion_filedesc *sion_filedesc,
							int             field_size, 
							sion_int64     *field           );

#endif
