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
* File: include/flowvr/moduleapimultiprocessimpl.h                *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MODULEAPIMULTIPROCESSIMPL_H
#define FLOWVR_MODULEAPIMULTIPROCESSIMPL_H

#include "flowvr/moduleapi.h"
#include "flowvr/daemondata.h"

namespace flowvr
{


// forward declaration
namespace mem
{
	class SharedMemoryArea;
}


/// ModuleAPI Implementation using IPC (Shared memory)
class ModuleAPIMultiProcessImpl : public ModuleAPI
{
 public:

  /// Mandatory virtual destructor.
  virtual ~ModuleAPIMultiProcessImpl();

  /// Initialize the module.
  ///
  /// This connects to the daemon and register the module.
  /// The ports are also initialized.
  /// Design note: a std::vector is used as a container as
  /// the list of ports is small and not modified after init.
  /// Design note 2: ports are always passed by pointers as
  /// they contain private fields modified by ModuleAPI.
  virtual int init(const std::vector<Port*>& ports, const std::vector<Trace*>& traces);


  virtual Allocator *getAllocator() const;

  /// Alloc a shared memory buffer
  virtual BufferWrite alloc(size_t size);

  /// Alloc a shared memory buffer and initialize it with the specified std::string
  virtual BufferWrite allocString(const std::string& str);

  /// Realloc a shared memory buffer.
  virtual bool realloc(BufferWrite &buffer, size_t size, bool amortized=false);

  /// Wait for the next iteration.
  ///
  /// This is the most important method of ModuleAPI.
  /// It updates the messages available on the input ports
  /// according to the conditions externally defined.
  /// The point is that the module's implementation have
  /// no knowledge of what these conditions are.
  virtual int wait();

  /// Get the current message available on an input port.
  ///
  /// The message moves an internal reference into the \c message parameter
  /// after clearing the latest. Once the function has returned, the module
  /// doesn't have a copy of this reference anymore and another call for the
  /// same port would supply an invalid reference.
  ///
  /// The user can only modify the buffer itself after successfully testing for
  /// its uniqueness :
  /// \verbatim if( message.data.unique( Buffer::ALLSEGMENTS ) )
  virtual int get(InputPort* port, Message& message);

  /// Send a new message to an output port.
  ///
  /// Note: The message buffer should not be modified by the module
  /// after calling this method.
  virtual int put(OutputPort* port, MessagePut& message);

  /// Send a new message to an output port.
  ///
  /// Note: The message buffer should not be modified by the module
  /// after calling this method.
  virtual int put(OutputPort* port, MessageWrite& message);

  /// Send a new stamplist to an output port.
  ///
  /// Note: Should only be done after the first wait, before putting a message
  virtual int put(OutputPort* port, const StampList *stamplist);

  /// Send a cpuset for the regulator to bind itself to
  int cpuset( const char* cpuset );

  /// Abort the FlowVR application
  virtual void abort();
  
  /// Explicitly close the current module.
  ///
  /// This is automatically done when calling the destructor.
  virtual int close();

  /// Return the current state of this module.
  ///
  /// Each of the other methods also returns the current status after completion.
  /// The exact meaning of this have to be defined...
  virtual int getStatus() const;

  /// Get the module ID.
  virtual std::string getID() const;

  /// Get a port source ID.
  virtual std::string getPortID(Port* port) const;

  /// Check if a port is connected (only valid after the first wait).
  virtual bool isPortConnected(const Port* port) const;
  virtual bool isBoundByInports() const;

  /// Return the local host name.
  virtual std::string getHostName() const;

  /// Generate a unique 64 bits ID.
  virtual ID generateID();

  virtual Port *getPortByName( const std::string &strName ) const;
  virtual Trace *getTraceByName( const std::string &strName ) const;


  virtual size_t getNumberOfInputPorts() const;
  virtual size_t getNumberOfOutputPorts() const;


 protected:

  /// Constructor. Called by ModuleAPIFactory.
  ModuleAPIMultiProcessImpl(const std::string& modulename,
			 const std::string& parentinfo,
			 const std::string& daemoninfo,
			 int flags=0);

  /// Execute an Action forwarded by the Regulator.
  std::string doAction(std::string xmlText, BufferWrite arg);

  friend class ModuleAPIFactory;
  std::vector<Port*> inputPorts;
  std::vector<Port*> outputPorts;
  std::string moduleName;
  std::string parentInfo;
  std::string daemonInfo;
  int flags;

  std::vector<Trace*> outputTraces;
  TypedTrace<int> traceWaitBegin;
  TypedTrace<int> traceWaitEnd;

  MPDaemonHeader* daemonHeader;

  MPModuleDescription* desc() const
  {
    return (*descBuf).getWrite<MPModuleDescription>(0);
  }

 private:
  mem::SharedMemoryArea *sharedArea;
  BufferWrite *descBuf;
  
  void hwbind( const void * shm_p, size_t shm_size );

}; // class ModuleAPIMultiProcessImpl

} // namespace flowvr

#endif
