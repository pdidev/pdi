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
* File: include/flowvr/thread.h                                   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_THREAD_H
#define FLOWVR_THREAD_H

#include "flowvr/config.h"
#include "flowvr/common.h"
#include "flowvr/topo.h"
#include <hwloc.h>

#include <pthread.h>


namespace flowvr
{


/**
 *  Thread utility class
 *  Threads are started using the Thread::start() method
 *  typical usage pattern is to sub-class and implement
 *  the Thread::run() method. This method is run in a dedicated
 *  thread, you can utilize the members of your specialized
 *  instance to provide context.
 */
class Thread
{
 public:
  /**
   *  Constructor. It initializes the pthread resources,
   *  and sets the cpu mask to zero for this thread.
   */
  Thread();

  /**
   * deletes pthread resources. Expect things to go crazy
   * when calling the destructor on a thread that is still
   * running.
   */
  virtual ~Thread();

  /**
   *  Start the thread. A thread that was started can not be started
   *  again until someone has called Thread::wait(). If you call start()
   *  on an already running thread, start() will return true and
   *  nothing else will happen.
   */
  bool start();


  /**
   * a detached thread can not and does not need to be waited for,
   * that means upon exit, the system will clean up automatically.
   * For this thread implementation: after returning from the run()
   * method, the state internals will be reset, and the thread can
   * be started again. There is no revoke after a successful detach(),
   * you have to end the thread or wait until it has ended to claim
   * it back again. A thread has to be started first before it can be
   * detached.
   * @see isDetached()
   * @see wait()
   */
  void detach();

  /**
   * @return true when this thread is detached, false else.
   */
  bool isDetached() const;

  /**
   * test for equality
   * @return true when oOther is the same thread as this thread
   */
  bool equals( const Thread &oOther ) const;

  /**
   *  Wait for termination of this thread, if the thread was not detached
   *  beforehand. Waiting on a detached thread is not possible.
   *  The call to wait() will try to join() the caller thread with the running
   *  thread, so expect a deadlock when calling this on a thread that will
   *  never finish.
   *  A thread that was successfully waited for can be started again by a call
   *  to Thread::start().
   *  @see detach()
   *  @return -1 when called on a detached thread
              (exit value) the value that was returned be the thread run method
   *
   */
  int wait();

  /**
    * Kill this thread, or to be more precise: calling this method will
    * send signal SIGKILL. This can not be handled, any thread can be
    * killed by that. In case your thread was sitting on exclusive resources,
    * expect some trouble or deadlocks. The routine can safely be called
    * on a non-started thread.
    */
  void kill();

  /**
    * Terminate this thread, or to be more precise: calling this method will
    * send signal SIGTERM. This can be handled, any thread can be
    * killed by that. In case the thread is waiting on blocking resources,
    * there is a good chance that the signal will interrupt the wait.
    * Anyhow, the thread implementation should be aware of that, otherwise
    * spurious things might happen.
    * The routine can safely be called on a non-started thread.
   */
  void terminate();



  enum eAffinityError
  {
	  ERR_OK = 0,
	  ERR_INVALID_TOPOLOGY,
	  ERR_CPUBIND_FAILED,

  };
  /**
   * sets affinity of this thread according to the cpuset (mask) given.
   * The thread can be set to have an affinity for different cpus
   * before or after it was started. A thread that is already running
   * will be migrated by the scheduler to run on the cpu set given.
   * To unset the affinity, call setAffinity() with a zero mask.
   * @param mask the cpu mask (see libtopology for more information)
   * @param topo an optional topology auxilliary class, typically,
                 this is for advanced usage, so normal users can ignore this.
   * @return true  the thread was successfully bound to a cpuset
             false else (diffcult to say, why...)
   */
  eAffinityError setAffinity( hwloc_cpuset_t mask, const Topo &topo = Topo(false) );

  /**
   * @returns the current cpu affinity mask (a zero-mask if none was set before)
   */
  hwloc_cpuset_t getAffinity() const;

  /**
   * @return the current operating system index for the CPU at
             the time of call. This is mainly informative or
             for some statistics.
             The call needs to have the sched_getcpu call on the OS
             to function properly. All other systems return -1 here.
   */
  static int       getCurrentCPU();


  /**
   * yield ( = throws away ) the callers current time-slice.
   */
  static void yield();

 protected:
  bool           detached; /**< mark detached state */
  pthread_t      tID;      /**< ID of the Thread */

  Topo           sys_topology; /** copy of system topology,
                                   needed for a call to bind */
  hwloc_cpuset_t  thread_cpu_mask; /** the cpu mask as set, for getAffinity() */

  /**
   *  Main thread function. Must be implemented by subclasses.
   *  Return any value you like here. Callers get that value on
   *  a call to wait() as a return value.
   */
  virtual int run()=0;

 private:
  /**
   * cleanup helper upon finalizing detached threads
   */
  static void  resetState( void * );

  /**
   * the physical thread function that will in-turn
   * call run on the thread instance.
   */
  static void* threadFunc( void * );


  Thread(const Thread& t); ///< Copy forbidden
  void operator=(const Thread& t); ///< Copy forbidden
};


/**
 * helper template to bind a free-floating function to a
 * thread.
 * - define an argument type, say "int"
 * - define a function void foo(int *)
 * - define an argument int i=42;
 * - define a ThreadFunction<int> f(foo, &i);
 * - call f.start() to run foo in its own thread.
 *
 * When you need more than one argument, define a struct or
 * class to pass that. The arguments should outlive the
 * thread object, as only the pointer is stored!
 */
template<class ArgT>
class ThreadFunction : public Thread
{
public:
	typedef void (*tFunc)(ArgT*);

	ThreadFunction( tFunc targ, ArgT *arg )
	: Thread()
	, foo(targ)
	, _args(arg)
	{

	}

	/**
	 * @return the arguments set during construction.
	           Can be useful for letting other readers have
	           access to the arguments during run.
	           Note that this does not ensure syncronicity problems.
	 */
	ArgT *getArgs() const
	{
		return _args;
	}
protected:

	/**
	 * simply calls the function with the arguments passed.
	 * @return 0 always.
	 */
	virtual int run()
	{
		(*foo)(_args);
		return 0;
	}
private:
	tFunc  foo; /** pointer to function */
	ArgT  *_args; /** pointer to arguments */
};


} // namespace flowvr



#endif
