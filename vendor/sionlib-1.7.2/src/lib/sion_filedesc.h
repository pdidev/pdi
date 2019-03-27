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

#ifndef SION_SION_FILEDESC_H
#define SION_SION_FILEDESC_H

#include "sion_const.h"
#include "sion_datatypes.h"
#include "sion_file.h"

#define _SION_FLAG1_NONE      0                 /*!< Flag1 not used */
#define _SION_FLAG2_NONE      0                 /*!< Flag2 not used */

#define _SION_FLAG1_USECACHE         2          /*!< bit mask for storing use cache flag */


#define SION_FILESTATE_UNKNOWN       -1         /*!< Unknown file state(error) */
#define SION_FILESTATE_PAROPEN       0          /*!< File opened in parallel */
#define SION_FILESTATE_SEROPEN       1          /*!< File opened in serial */
#define SION_FILESTATE_SEROPENRANK   2          /*!< File opened in serial for a specific rank */
#define SION_FILESTATE_SEROPENMASTER 3          /*!< File opened in serial, master  */
#define SION_FILESTATE_PAROPENMAPPEDMASTER  4   /*!< File opened in parallel with mapping task, master */
#define SION_FILESTATE_PAROPENMAPPEDMANAGED 5   /*!< File opened in parallel with mapping task and managed by this tasks */
#define SION_FILESTATE_PAROPENMAPPED        6   /*!< File opened in parallel with mapping task and managed by other tasks */
#define SION_FILESTATE_CLOSE        10          /*!< File closed */
#define SION_FILEMODE_UNKNOWN       -20         /*!< File mode unknown (error) */
#define SION_FILEMODE_READ          20          /*!< File opened for reading */
#define SION_FILEMODE_WRITE         30          /*!< File opened for writing */

#define SION_CACHE_TODISK           40          /*!< Cache will store data directly to disk (in sion_write or in sion_close if file is opened for reading and cachedata is dirty) */
#define SION_CACHE_TOMEM            41          /*!< Cache will write only to memory */
#define SION_CACHE_UNKNOWN          42          /*!< Unknown */
#define SION_CACHE_FNLEN            16          /*!< length of filename for segment */

#define SION_DESCSTATE_ORIG         200         /*!< data structure is NOT a duplicate of another data structure */
#define SION_DESCSTATE_DUP          201         /*!< data structure is a duplicate of another data structure */
#define SION_DESCSTATE_DUP_SEL_RANK 202         /*!< data structure is a duplicate of another data structure, only for one rank */
#define SION_DESCSTATE_DUP_SEL_RANK_KEY 203     /*!< data structure is a duplicate of another data structure, only for one rank and one key */

#define SION_HINTS_TYPE_UNKNOWN     160         /*!< use hints of type UNKNOWN */
#define SION_HINTS_TYPE_LINUX       161         /*!< use hints for Linux filessystem */
#define SION_HINTS_TYPE_GPFS        162         /*!< use hints for GPFS filessystem */

#define SION_CAPABILITY_FULL        50          /*!< Capability of a task in collective mode */
#define SION_CAPABILITY_ONLY_SENDER 51          /*!< Capability of a task in collective mode */
#define SION_CAPABILITY_NONE        52          /*!< Capability of a task in collective mode */

#define _SION_DEBUG_PRINT_ALL       1
#define _SION_DEBUG_PRINT_RECURSIVE 2

/*!
 * @brief SION File Descriptor Alias
 */
typedef struct _sion_filedesc_struct _sion_filedesc;

/*!
 * \struct _sion_filedesc_struct
 * \brief Sion File Descriptor Structure
 *
 * The sion file descriptor structure stores all the properties for the sion file.
 * This properties are the META data of a sion file
 *
 * DO NOT change types of elements, becauses these are directly used to read/write header
 *
 * CHUNK: reserved space for each rank, each rank can have serveral chunks in a file
 *
 * BLOCK: used space in a CHUNK
 */
struct _sion_filedesc_struct {

  /* file pointer and temporary data */
  _sion_fileptr *fileptr;         /*!< Pointer to the current sion file */
  void*      dataptr;              /*!< pointer to data structure for upper layers */
  void*      keyvalptr;            /*!< pointer to data structure for key-value */
  sion_int32 debug;                /*!< 0 or 1 for debugging */
  sion_int32 sid;                  /*!< sid of data structure */
  char*      fpbuffer;             /*!< buffer for optimizing ANSI C I/O function fwrite/fread */
  sion_int32 fpbuffer_size;        /*!< buffer size */
  sion_int32 usebuffer;            /*!< is set to 1 if data is/should be buffered internally in sion_fwrite/sion_fread */
  char*      buffer;               /*!< buffer for internal buffering data in sion_fwrite/sion_fread */
  sion_int32 buffer_size;          /*!< buffer size */
  sion_int32 buffer_ptr;           /*!< index of the first free byte in buffer */

  /* current position in file */
  sion_int32 rank;                 /*!< Rank of the task writing the file (global rank) */
  sion_int64 currentpos;           /*!< only used if opened  for one rank, reading */
  sion_int32 currentblocknr;       /*!< only used if opened  for one rank, reading */

  /* data for one task  */
  sion_int64 *blocksizes;          /*!< Array of the blocksizes in all chunks of the current task */
  sion_int32 lastchunknr;          /*!< number of last chunk used on this task (0..n) */
  sion_int64 startpos;             /*!< Start position for the current task, start of first chunk (fix for task) */
 
  /* meta header data in file */
  char       *fname;               /*!< Name of the file */
  sion_int32 globalrank;           /*!< Globalrank of the task given by parameter or implicitly */
  sion_int32 ntasks;               /*!< Number of tasks using the sion file */
  sion_int32 state;                /*!< INTERNAL STATE */
  sion_int32 mode;                 /*!< Mode used to open the file(rb/wb) */
  sion_int32 endianness;           /*!< Endianness used. Written symmetric.
                                     First (and last) byte indicates meta data.
                                     Second (and second last) byte indicates
                                     user data. 0 = little, 1 = big */
  sion_int32 swapbytes;            /*!< swapping bytes is needed for this sion file  */
  sion_int32 filesionversion;      /*!< Version number of library which wrote sion file (version*100+sub_version) */
  sion_int32 filesionpatchlevel;   /*!< patch-level of library which wrote sion file */
  sion_int32 fileversion;          /*!< Version number of sion file format */
  sion_int32 fsblksize;            /*!< Filesystem block size */
  sion_int32 maxchunks;            /*!< Max number of chunks per task */
  sion_int64 flag1;                /*!< keyval-mode since file format version 5 */            
  sion_int64 flag2;                /*!< placeholder for future flags */            
  sion_int64 chunksize;            /*!< chunksize for the current task(From Meta block 1) */
  sion_int64 chunksize_req;        /*!< Requested chunksize for the current task */
  sion_int64 globalskip;           /*!< number of bytes to skip to next block of same rank */
  sion_int64 end_of_header;        /*!< End of META block 1 (beginning of the sion file) */
  sion_int64 start_of_varheader;   /*!< Beginning of the META block 2 (end of the sion file) */
  sion_int64 start_of_data;        /*!< Start of the data in the sion file */
  sion_int32 nfiles;               /*!< Number of files in the set */
  sion_int32 filenumber;           /*!< file number in the set */
  sion_int32 maxusedchunks;        /*!< Max number of chunks per task already used by the tasks */

  /* dupped filedesc */
  sion_int32 dup_mode;             /*!< dup mode; used to indicate if datastructure was duplicated from another one */
  sion_int32 dup_sel_rank;         /*!< Used to indicate if datastructure was duplicated from another one */
  sion_uint64 dup_sel_key;         /*!< Used to indicate if datastructure was duplicated from another one */

  /* mapped files */
  sion_int32 lrank;		   /*!< local rank, needed to omit re-computation of mapping */
  sion_int32 ntotaltasksinfile;    /*!< total number of  tasks in this the sion file (mapped-mode) */
  sion_int32 nlocaltasksinfile;    /*!< number of local tasks in this the sion file (mapped-mode) */
  sion_int32 filemanagedbytask;    /*!< task number of managing task of this the sion file (mapped-mode) */

  /* all tasks */
  sion_int64 *all_chunksizes;      /*!< list of all chunk sizes stored in this file */
  sion_int64 *all_globalranks;     /*!< list of all globalranks stored in this file  */
  sion_int64 *all_localranks;      /*!< list of all localranks stored in this file  */
  sion_int64 *all_startpointers;   /*!< list of all start pointers stored in this file  */
  sion_int64 *all_currentpos;      /*!< only used if sion is opened from a single task for read  */
  sion_int64 *all_currentblocknr;  /*!< only used if sion is opened from a single task for read  */
  sion_int32 *all_coll_collector;  /*!< only used on rank 0 if usecoll=1  */
  sion_int32 *all_coll_collsize;   /*!< only used on rank 0 if usecoll=1  */
  sion_int32 *all_coll_capability; /*!< capability of task (FULL, ONLYSENDER)  */

  void       **all_keyvalptr;      /*!< keyval data structure for each task */

  /* all tasks, all chunks */
  sion_int64 *all_blockcount;      /*!< size, only used if sion is opened from a single task for read  */
  sion_int64 *all_blocksizes;      /*!< size*maxblocks, only used if sion is opened from a single task for read  */

  /* multi-file */
  sion_int32 mapping_size;         /*!< size of mapping table, -1 if not available */
  sion_int32 *mapping;             /*!< pointer to mapping table if reading multi file */
  char       *prefix;              /*!< Filenames of all files in the set */
  _sion_filedesc **multifiles;     /*!< pointer to field containing data structures of sion files of a sion multi-file */

  /* compression */
  sion_int32 compress;             /*!< is set to 1 if data should be/is compressed */

  /* Key-value */
  sion_int32 keyvalmode;           /*!< describes if and in which mode keyval will be used  */

  /* persistent cache */
  sion_int32 usecache;             /*!< is set to 1 if data is/should be cached in persistent memory */
  sion_int32 cachesize;            /*!< cache size in number of bytes */
  sion_int32 cacheid;              /*!< cache id */
  char       cachefn[SION_CACHE_FNLEN];           /*!< cache id */
  sion_int32 cachemode;            /*!< cache mode (TOMEM,TODISK) */
  char*      cacheptr;             /*!< pointer to cache */

  /* collective I/O */
  sion_int32 usecoll;              /*!< is set to 1 if collective operation should be used for I/O */
  sion_int32 collsize;             /*!< number of tasks working together (for this task) */
  sion_int32 collector;            /*!< tasks number of corresponding collector */
  sion_int32 coll_capability;      /*!< capability of task (FULL, ONLYSENDER)  */
  sion_int32 colldebug;            /*!< print debug info on stderr, i level of detail, 0->off  */
  sion_int32 collcmdused;          /*!< indicates if collective call was already performed */
  sion_int32 fileptr_exported;     /*!< indicates that file pointer is exported via paramater to application */
  sion_int32 collmergemode;        /*!< merge data on collector task */

  /* buddy */
  sion_int32 usebuddy;             /*!< is set to 1 if buddy mode is selected */
  sion_int32 buddylevel;           /*!< buddy level (number of copies) */
  sion_int32 buddynr;              /*!< number of buddy file, 0 for original file  */
  void      *buddies;              /*!< pointer to payload container structure describing buddies */

  /* hints */
  sion_int32 usehints;             /*!< is set to 1 if hints should be applied */
  sion_int32 hinttype;             /*!< type of hints */

};

_sion_filedesc * _sion_alloc_filedesc(void);
int _sion_init_filedesc(  _sion_filedesc *sion_filedesc );
int _sion_print_filedesc( _sion_filedesc *sion_filedesc, int level, char *desc, int all );
int _sion_free_filedesc(  _sion_filedesc *sion_filedesc );
int _sion_realloc_filedesc_blocklist(  _sion_filedesc *sion_filedesc, sion_int32 maxchunks );

int _sion_alloc_filedesc_all_chunksizes(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_all_startpointers(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_all_globalranks(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_all_localranks(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_arrays( _sion_filedesc *sion_filedesc );
int _sion_alloc_filedesc_all_keyvalptr(_sion_filedesc *sion_filedesc);

int _sion_free_filedesc_arrays( _sion_filedesc *sion_filedesc );
int _sion_free_filedesc_all_chunksizes(_sion_filedesc *sion_filedesc);
int _sion_free_filedesc_all_globalranks(_sion_filedesc *sion_filedesc);
int _sion_free_filedesc_all_startpointers(_sion_filedesc *sion_filedesc);
int _sion_free_filedesc_all_localranks(_sion_filedesc *sion_filedesc);
int _sion_free_filedesc_all_keyvalptr(_sion_filedesc *sion_filedesc);



int _sion_alloc_filedesc_block_arrays(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_block_arrays_only(_sion_filedesc *sion_filedesc);

int _sion_alloc_filedesc_coll_arrays(_sion_filedesc *sion_filedesc);
int _sion_free_filedesc_coll_arrays(_sion_filedesc *sion_filedesc);

int _sion_alloc_filedesc_arrays_mapped(_sion_filedesc *sion_filedesc);
int _sion_alloc_filedesc_block_arrays_mapped(_sion_filedesc *sion_filedesc);

char* _sion_fileptrflags_to_str (unsigned int flag);
int _sion_get_size_of_filedesc(_sion_filedesc *sion_filedesc, int *numbytes, int *numfds);

_sion_filedesc * _sion_dup_filedesc(_sion_filedesc *sion_filedesc);


#endif
