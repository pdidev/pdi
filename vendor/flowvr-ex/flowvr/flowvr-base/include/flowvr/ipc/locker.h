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
* File: include/flowvr/ipc/locker.h                               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_IPC_LOCKER_H
#define FLOWVR_IPC_LOCKER_H

#include "flowvr/common.h"
#include <iostream>

namespace flowvr
{

namespace ipc
{

/// Utility class holding a lock while in scope (for safe locking).
/// Inspired from boost::thread::ScopedLock ( http://www.boost.org/libs/thread/doc/lock_concept.html#ScopedLock-concept )
template <class Lock>
class Scoped
{
 public:
  typedef Lock LockType;
#ifdef FLOWVR_IPC_DEBUG
  const char* name;
  Scoped(Lock& thelock, const char* name)
    : name(name), lock(thelock)
  {
    lock.lock(name);
  }
  ~Scoped()
  {
    lock.unlock(name);
  }
#else
  Scoped(Lock& thelock, const char* /*name*/=NULL)
  : lock(thelock)
  {
    lock.lock();
  }
  ~Scoped()
  {
    lock.unlock();
  }
#endif
 private:
  Lock& lock;
  /// copy forbidden
  Scoped(const Scoped<Lock>& base);
  /// copy forbidden
  Scoped<Lock>& operator=(const Scoped<Lock>& base);
};

/// Utility class holding a lock while in scope (for safe locking).
/// Inspired from boost::thread::ScopedLock ( http://www.boost.org/libs/thread/doc/lock_concept.html#ScopedLock-concept )
template <class Lock>
class TryScoped
{
 public:
  typedef Lock LockType;

  TryScoped( Lock& thelock, const char* /*name*/=NULL )
  : _lock( thelock )
  , _is_locked( _lock.trylock() )
  {}
  
  ~TryScoped() {
      if ( _is_locked )
          _lock.unlock();
  }
  
  bool is_locked() const { return _is_locked; }
  
private:
  Lock&      _lock;
  const bool _is_locked;
  
  /// copy forbidden
  TryScoped(const TryScoped<Lock>& base);
  /// copy forbidden
  TryScoped<Lock>& operator=(const TryScoped<Lock>& base);
};

} // namespace ipc

} // namespace flowvr

#endif
