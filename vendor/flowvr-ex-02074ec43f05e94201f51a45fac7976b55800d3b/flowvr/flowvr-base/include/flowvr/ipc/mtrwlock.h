/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Base Libraries                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490). ALL RIGHTS RESERVED.                                *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/ipc/mtlock.h                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_MTRWLOCK_H
#define FLOWVR_IPC_MTRWLOCK_H

#include "flowvr/ipc/locker.h"
#ifdef FLOWVR_IPC_DEBUG
#include "flowvr/mpdata.h"
#endif

#include <pthread.h>
#include <iostream>

namespace flowvr
{

namespace ipc
{





/// Multi-thread (simple) lock.
class MTrwLock
{
public:

    class Read;
    class Write;
    class ScoppedRead;
    class ScoppedWrite;
    
  MTrwLock(const char* myname
#ifndef FLOWVR_IPC_DEBUG
	 = NULL
#endif
	 )
  {
    init(myname);
  }

  ~MTrwLock()
  {
    close();
  }

  /// Initialization.
#ifdef FLOWVR_IPC_DEBUG
  std::string name;
  void init(const char* myname)
  {
    if (myname!=NULL) name = myname;
    std::cout<<">MTrwLock("<<(std::string)name<<").init()"<<std::endl;
#else
  void init(const char* /*myname*/=NULL)
  {
#endif
    pthread_rwlock_init(&rwlock,NULL);
#ifdef FLOWVR_IPC_DEBUG
    std::cout<<"<MTrwLock("<<(std::string)name<<").init()"<<std::endl;
#endif
  }

  /// Destruction.
  void close()
  {
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"MTrwLock("<<(std::string)name<<").close()"<<std::endl;
#endif
    pthread_rwlock_destroy(&rwlock);
  }

  /// Acquire a read lock.
#ifdef FLOWVR_IPC_DEBUG
  void rdlock(const char* by=NULL)
  {
    if (!name.empty())
    {
      std::cout<<">MTrwLock("<<(std::string)name<<").rdlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void rdlock(const char* /*by*/=NULL)
  {
#endif
    pthread_rwlock_rdlock(&rwlock);
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"<MTrwLock("<<(std::string)name<<").rdlock()"<<std::endl;
#endif
  }
  
  /// Acquire the write lock.
#ifdef FLOWVR_IPC_DEBUG
  void wrlock(const char* by=NULL)
  {
    if (!name.empty())
    {
      std::cout<<">MTrwLock("<<(std::string)name<<").wrlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void wrlock(const char* /*by*/=NULL)
  {
#endif
    pthread_rwlock_wrlock(&rwlock);
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"<MTrwLock("<<(std::string)name<<").wrlock()"<<std::endl;
#endif
  }

  /// Release acquired lock.
#ifdef FLOWVR_IPC_DEBUG
  void unlock(const char* by=NULL)
  {
    if (!name.empty()) 
    {
      std::cout<<">MTrwLock("<<(std::string)name<<").unlock()";
      if (by) std::cout<<" by "<<by;
      std::cout<<std::endl;
    }
#else
  void unlock(const char* /*by*/=NULL)
  {
#endif
    pthread_rwlock_unlock(&rwlock);
#ifdef FLOWVR_IPC_DEBUG
    if (!name.empty()) std::cout<<"<MTrwLock("<<(std::string)name<<").unlock()"<<std::endl;
#endif
  }

protected:
  pthread_rwlock_t rwlock;
  friend class MTSignal;
}; // class MTrwLock


class MTrwLock::Read
{
public:
    Read( MTrwLock & rwlock )
        : _rwlock( rwlock )
    {}
    void lock(const char* by=NULL)   { _rwlock.rdlock(by); }
    void unlock(const char* by=NULL) { _rwlock.unlock(by); }
private:
    MTrwLock & _rwlock;
};
class MTrwLock::ScoppedRead
{
public:
    ScoppedRead( MTrwLock & rwlock, const char * txt = NULL )
        : _read( rwlock )
        , _locker( _read, txt )
    {}
private:
    MTrwLock::Read          _read;
    Scoped<MTrwLock::Read>  _locker;
};

class MTrwLock::Write
{
public:
    Write( MTrwLock & rwlock )
        : _rwlock( rwlock )
    {}
    void lock(const char* by=NULL)   { _rwlock.wrlock(by); }
    void unlock(const char* by=NULL) { _rwlock.unlock(by); }
private:
    MTrwLock & _rwlock;
};
class MTrwLock::ScoppedWrite
{
public:
    ScoppedWrite( MTrwLock & rwlock, const char * txt = NULL )
        : _write( rwlock )
        , _locker( _write, txt )
    {}
private:
    MTrwLock::Write          _write;
    Scoped<MTrwLock::Write>  _locker;
};



#if 0
#include "flowvr/ipc/atomic.h"
template< typename T >
class SharedData
{
public:
    
    class ScoppedRead;
    class ScoppedWrite;
    friend class ScoppedRead;
    friend class ScoppedWrite;
    
    typedef T                        data_type; 
    typedef flowvr::ipc::MTAtomicInt atomic_type; 
    
    SharedData()
        : _data()
        , _atom( 0 )
    {}

    SharedData( const data_type & data )
        : _data( data )
        , _atom( 0 )
    {}

private:
    
    enum {
        write_flag = 1 << 30,
        read_count = write_flag-1
    };

    void read() const {
        while( true ) {
            // wait end of any pending write
            while ( write_flag & _atom );
            // ask for a read
            _atom.inc();
            // double-check
            if ( write_flag & _atom ) {
                _atom.dec();
                continue;
            } else {
                return;
            }
        }
    }
    
    void write() {
        // acquire exclusive write rights
        while ( ! _atom.compare_and_swap( _atom & read_count, _atom | write_flag ) );
        // wait end of pending reads
        while ( read_count & _atom );
    }
    
            data_type   _data;
    mutable atomic_type _atom;
    
};

template< typename T >//, typename A = flowvr::ipc::MTAtomicInt >
class SharedData<T>::ScoppedRead
{
public:
    ScoppedRead( const SharedData& access ) : _access( access ) { _access.read(); }
    ~ScoppedRead() { _access._atom.dec(); }
    operator const T & () const { return _access._data; }
    const T & it() const { return operator const T & (); }
private:
    const SharedData& _access;
};
        
template< typename T >//, typename A = flowvr::ipc::MTAtomicInt >
class SharedData<T>::ScoppedWrite
{
public:
    ScoppedWrite( SharedData& access ) : _access( access ) { _access.write(); }
    ~ScoppedWrite() {
        SharedData::atomic_type & atom( _access._atom );
        while ( ! atom.compare_and_swap( atom, atom & read_count ) );
    }
    operator T & () { return _access._data; }
    T & it() { return operator T & (); }
private:
    SharedData& _access;
};

template class SharedData< int >;
template class SharedData< int* >;
template class SharedData< const int >;
template class SharedData< const int* >;
#endif


} // namespace ipc

} // namespace flowvr

#endif
