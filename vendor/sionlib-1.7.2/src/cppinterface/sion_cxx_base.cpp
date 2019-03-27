
#include <iostream>
#include "sion_cxx_base.hpp"

// Common functions
char* SION_Base::getSionFileName() const {
  return _sion_file_name;
}

void SION_Base::setMode(std::string mode) {
  _mode = mode;
}

std::string SION_Base::getMode() const {
  return _mode;
}


void SION_Base::setNumberOfFiles(int num_files) {
  _num_files = num_files;
}

int SION_Base::getNumberOfFiles() const {
  return _num_files;
}

void SION_Base::setNumberOfTasks(int num_tasks) {
  _num_tasks = num_tasks;
}

int SION_Base::getNumberOfTasks() const {
  return _num_tasks;
}

void SION_Base::setRank(int rank) {
  _rank = rank;
}

int SION_Base::getRank() const {
  return _rank;
}

void SION_Base::setChunkSize(sion_int64 chunk_size) {
  _chunk_size = chunk_size;
}

sion_int64 SION_Base::getChunkSize() const {
  return _chunk_size;
}

void SION_Base::setChunkSizes(sion_int64 * chunk_sizes) {
  _chunk_sizes = chunk_sizes;
}

sion_int64 * SION_Base::getChunkSizes() const {
  return _chunk_sizes;
}

void SION_Base::setGlobalRanks(int * global_ranks) {
  _global_ranks = global_ranks;
}

int * SION_Base::getGlobalRanks() const {
  return _global_ranks;
}

void SION_Base::setFileSystemBlockSize(sion_int32 fs_blk_size) {
  _fs_blk_size = fs_blk_size;
}

sion_int32 SION_Base::getFileSystemBlockSize() const {
  return _fs_blk_size;
}

int SION_Base::getNumberOfSuccessfulReadElements() const {
  return _number_of_elements_sucessfully_read;
}

int SION_Base::getReturnCode() const {
  return _return_code;
}

int SION_Base::getSid() const {
  return _sid;
}


/* get information (with sion datatypes) */
int SION_Base::getFileEndianness() const {
  return sion_get_file_endianness(_sid);
}

sion_int64 SION_Base::getBytesWritten() const {
  return sion_get_bytes_written(_sid);
}

sion_int64 SION_Base::getBytesRead() const {
  return sion_get_bytes_read(_sid);
}

sion_int64 SION_Base::getBytesAvailInBlock() const {
  return sion_bytes_avail_in_block(_sid);
}

sion_int64 SION_Base::getBytesAvailInChunk() const {
  return sion_bytes_avail_in_chunk(_sid);
}

sion_int64 SION_Base::getPosition() const {
  return sion_get_position(_sid);
}


/* Seeking */
void SION_Base::seek() {
  _return_code = sion_seek(_sid,SION_CURRENT_RANK,SION_CURRENT_BLK,SION_CURRENT_POS);
}

// void SION_Base::seekFp() {
//   _return_code = sion_seek_fp(_sid, _rank, SION_CURRENT_BLK,SION_CURRENT_POS,&_file_ptr);

