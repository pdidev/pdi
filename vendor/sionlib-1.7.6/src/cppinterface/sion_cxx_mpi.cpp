
#include "mpi.h"

#include "sion_cxx_mpi.hpp"

namespace sionlib {

  namespace mpi {

    SIONFile::SIONFile() {
      std::cerr << "At least name has to be given\n";
      exit (EXIT_FAILURE);
    }

    SIONFile::SIONFile(std::string sion_file_name, std::string mode, int num_files,
		       int global_rank, MPI_Comm gComm, MPI_Comm lComm) {

      size_t ncharacter = sion_file_name.length()+1;
      const char * tmp_sion_file_name = sion_file_name.c_str();
      _sion_file_name = new char[ncharacter];

      strncpy(_sion_file_name, tmp_sion_file_name, ncharacter);

      _mode = mode;
      _num_files = num_files;

      _g_comm = gComm;

      _l_comm = lComm;

      _chunk_size = 2097152;
      _fs_blk_size = -1;

      _file_ptr = NULL;
      _new_sion_file_name = new char[255];

    }

    SIONFile::~SIONFile() {
      delete [] _sion_file_name;
      _sion_file_name = NULL;
      delete [] _new_sion_file_name;
      _new_sion_file_name = NULL;
      //	sion_parclose_mpi(_sid);
    }

    //void SIONFile::setSionFileName(std::string sion_file_name) {
    //	_sion_file_name = sion_file_name;
    //}

    void SIONFile::setLocalCommunicator(MPI_Comm lComm) {
      _l_comm = lComm;
    }

    MPI_Comm SIONFile::getLocalCommunicator() const {
      return _l_comm;
    }

    void SIONFile::setGlobalCommunicator(MPI_Comm gComm) {
      _g_comm = gComm;
    }

    MPI_Comm SIONFile::getGlobalCommunicator() const {
      return _g_comm;
    }

    void SIONFile::setGlobalRank(int global_rank) {
      _global_rank = global_rank;
    }

    int SIONFile::getGlobalRank() const {
      return _global_rank;
    }

    char * SIONFile::getNewSionFileName() const {
      return _new_sion_file_name;
    }
    //std::ostream* SIONFile::getSionStream() const {
    //	return _sion_stream;
    //}

    // sion_paropen_mpi
    //int SIONFile::open() {
    //
    //	_sid = sion_paropen_mpi(_sion_file_name,
    //			_mode.c_str(),
    //			&_num_files,
    //			MPI_COMM_WORLD,
    //			&_l_comm,
    //			&_chunk_size,
    //			&_fs_blk_size,
    //			&_global_rank,
    //			&_file_ptr,
    //			&_new_sion_file_name);
    ////	std::cout << "in open() _new_sion_file_name:\t" << _new_sion_file_name << std::endl;
    //	return _sid;
    //
    //}

    void SIONFile::open() {
      _sid = sion_paropen_mpi(_sion_file_name, _mode.c_str(), &_num_files,
			      _g_comm, &_l_comm, &_chunk_size, &_fs_blk_size,
			      &_global_rank, NULL, &_new_sion_file_name);
      _return_code = _sid;
    }

    void SIONFile::close() {
      //	fwrite(void *).
      _return_code = sion_parclose_mpi (_sid);
    }

    //void SIONFile::read() {
    ////	char * buffer = new char[8];
    //	double * buffer = new double();
    //	sion_fread(buffer, 1, 8, _sid);
    //	std::cout << *buffer << std::endl;
    //}

    void SIONFile::ensureFreeSpace(long numbytes) {
      _return_code = sion_ensure_free_space(_sid, numbytes);
    }

    void SIONFile::endOfFile() {
      _return_code = sion_feof(_sid);
    }

  } /* namespace mpi */
} /* namespace sionlib */
