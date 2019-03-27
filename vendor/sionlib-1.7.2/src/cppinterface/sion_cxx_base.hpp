#ifndef SION_CXX_BASE_HPP_
#define SION_CXX_BASE_HPP_

#include "sion.h"
#include <string>
#include <string.h>

class SION_Base {

public:
  SION_Base() : _sid(-9999) {}
  ~SION_Base() {}


  // Common functions
  char* getSionFileName() const;

  void setMode(std::string mode);
  std::string getMode() const;

  void setNumberOfFiles(int num_files);
  int getNumberOfFiles() const;

  void setNumberOfTasks(int num_tasks);
  int getNumberOfTasks() const;

  void setRank(int rank);
  int getRank() const;

  void setChunkSize(sion_int64 chunk_size);
  sion_int64 getChunkSize() const;

  void setChunkSizes(sion_int64 * chunk_sizes);
  sion_int64 * getChunkSizes() const;

  void setGlobalRanks(int * global_ranks);
  int * getGlobalRanks() const;

  void setFileSystemBlockSize(sion_int32 fs_blk_size);
  sion_int32 getFileSystemBlockSize() const;

  int getNumberOfSuccessfulReadElements() const;
   
  int getSid() const;

  int getReturnCode() const;


  /* Seeking */
  void seek();
  //     void seekFp();

  /* get information (with sion datatypes) */
  int getFileEndianness() const;
  sion_int64 getBytesWritten() const;
  sion_int64 getBytesRead() const;
  sion_int64 getBytesAvailInBlock() const;
  sion_int64 getBytesAvailInChunk() const;
  sion_int64 getPosition() const;

protected:
  // Common attributes
  char * _sion_file_name;
  std::string _mode;
  int _num_files;
  int _num_tasks;
  int _rank;
  sion_int64 * _chunk_sizes;
  sion_int64 _chunk_size;
  sion_int32 _fs_blk_size;
  int * _global_ranks;
  FILE * _file_ptr;
  int _number_of_elements_sucessfully_read;
  int _return_code;
  int _sid;

  //  get information (with sion datatypes) 
  int _file_endianness;
  sion_int64 _bytes_written;
  sion_int64 _bytes_read;
  sion_int64 _bytes_avail_in_block;
  sion_int64 _bytes_avail_in_chunk;
  sion_int64 _position;
 
};

#endif /* SION_CXX_BASE_HPP_ */
