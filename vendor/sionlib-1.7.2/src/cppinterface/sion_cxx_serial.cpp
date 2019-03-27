
#include <iostream>
#include "sion_cxx_serial.hpp"

namespace sionlib {

  namespace serial {

    SIONFile::SIONFile() {
    }

    SIONFile::SIONFile(std::string sion_file_name, std::string mode, int num_tasks,
		       int num_files, sion_int64 * chunk_sizes, sion_int32 _fs_blk_size, int * global_ranks) {

      size_t ncharacter = sion_file_name.length()+1;
      const char * tmp_sion_file_name = sion_file_name.c_str();

      _sion_file_name = new char[ncharacter];

      strncpy(_sion_file_name, tmp_sion_file_name, ncharacter);

      _mode = mode;
      _num_files = num_files;
      _num_tasks = num_tasks;

      _rank = 0;

      //	_chunk_sizes = NULL;
      _fs_blk_size = -1;
      //	_global_ranks = global_ranks;

      _file_ptr = NULL;

    }

    SIONFile::~SIONFile() {
      delete [] _sion_file_name;
      _sion_file_name = NULL;
    }

    void SIONFile::open() {
      _sid = sion_open(_sion_file_name, _mode.c_str(), &_num_tasks, &_num_files,
		       &_chunk_sizes, &_fs_blk_size, &_global_ranks, &_file_ptr);
    }

    void SIONFile::openRank() {
      _sid = sion_open_rank(_sion_file_name, _mode.c_str(), &_chunk_size,
			    &_fs_blk_size, &_rank, &_file_ptr);
    }

    int SIONFile::close() {
      return sion_close(_sid);
    }

  } /* serial */
} /* namespace sionlib */
