
#ifndef SION_CXX_MPI_HPP_
#define SION_CXX_MPI_HPP_

#ifdef MPI_VERSION

#include <mpi.h>
#include "sion.h"
#include <iostream>
#include <string>
#include <string.h>
#include <stdlib.h>
#include <fstream>
#include "sion_cxx_base.hpp"

namespace sionlib {
  namespace mpi {

    class SIONFile : public SION_Base {
    public:
    
      SIONFile();
      SIONFile(std::string sion_file_name, std::string mode = "bw",
	       int num_files = 1, int global_rank = 0, MPI_Comm gComm=MPI_COMM_WORLD, MPI_Comm lComm=MPI_COMM_WORLD);
      virtual ~SIONFile();

      // Don't copy SIONFile
      SIONFile(const SIONFile& rhs) {
      }

      char * getNewSionFileName() const;

      //	std::ostream* getSionStream() const;

      //	template<class T>
      //	SIONFile& SIONFile::operator<<(T rhs);

      //	int open();

      void setLocalCommunicator(MPI_Comm lComm);
      MPI_Comm getLocalCommunicator() const;

      void setGlobalCommunicator(MPI_Comm gComm);
      MPI_Comm getGlobalCommunicator() const;

      void setGlobalRank(int global_rank);
      int getGlobalRank() const;



      // Wrapper for SION functions
      void open();
      void close();

      template<class T>
      void write(T data);

      template<typename dataT>
      void read(dataT * data);

      //	Nur zu Versuchszwecken
      template<typename dataT>
      void read(dataT * data, int unit, int length);

      //	Nur zu Versuchszwecken
      template<class T>
      void write(T & data, int unit, int length);

       template<typename outT>
      friend SIONFile& operator<<(SIONFile & sf, const outT& rhs);

      template<typename inT>
      friend SIONFile& operator>>(SIONFile & sf, inT& rhs);

      void ensureFreeSpace(long numbytes);
      void endOfFile();
 
    private:
      char * _new_sion_file_name;
      MPI_Comm _g_comm;
      MPI_Comm _l_comm;
      int _global_rank;
    };

    template<class T>
    void SIONFile::write(T data) {
      //      sion_fwrite(reinterpret_cast<char*>(data), 1, _chunk_size, _sid);
      //      sion_fwrite(reinterpret_cast<char*>(&data), 1, sizeof(data), _sid);
      sion_fwrite(reinterpret_cast<char*>(&data), sizeof(data), 1, _sid);
      //sion_coll_fwrite_mpi(reinterpret_cast<char*>(&data), 1, sizeof(data), _sid);
      //	sion_coll_fwrite_mpi(reinterpret_cast<char*>(&data), 1, sizeof(data), _sid);
    }

    template<class T>
    void SIONFile::write(T & data, int unit, int length) {
      //	fwrite(reinterpret_cast<char*>(&data), 1, sizeof(data), _file_ptr);
      //	sion_fwrite(reinterpret_cast<char*>(&data), unit, length, _sid);
      //	std::cout << "write() from mpi namespace" << std::endl;
      sion_coll_fwrite_mpi((void *) data.data(), unit, length, _sid);
    }

    template<typename outT>
    SIONFile& operator<<(SIONFile & sf, const outT& rhs) {
      sf.write(rhs);
      return sf;
    }

    template<typename dataT>
    void SIONFile::read(dataT * data) {
      dataT tmp = *data;
      //	std::cout << "size of T:\t" << sizeof(tmp) << std::endl;
      //      _return_code = sion_fread(data, 1, sizeof(tmp), _sid);
      _return_code = sion_fread(data, sizeof(tmp), 1, _sid);
      //   sion_coll_fread_mpi(data, 1, sizeof(tmp), _sid);
    }

    template<typename dataT>
    void SIONFile::read(dataT * data, int unit, int length) {
      sion_coll_fread_mpi(data, unit, length, _sid);
    }

    template<typename inT>
    SIONFile& operator>>(SIONFile & sf, inT & rhs) {
      sf.read(&rhs);
      return sf;
    }
  } /* namespace mpi */

} /* namespace sionlib */

#endif /* MPI_VERSION */

#endif /* SION_CXX_MPI_HPP_ */
