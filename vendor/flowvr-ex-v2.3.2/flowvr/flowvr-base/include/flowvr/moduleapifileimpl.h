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
* File: include/flowvr/moduleapifileimpl.h                        *
*                                                                 *
* Contacts:                                                       *
*  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MODULEAPIFILEIMPL_H
#define FLOWVR_MODULEAPIFILEIMPL_H

#include "flowvr/moduleapi.h"
#include "flowvr/portreader.h"
#include "flowvr/portwriter.h"

namespace flowvr
{

/// ModuleAPI Implementation using Files
class ModuleAPIFileImpl : public ModuleAPI
{
 public:

  /// Mandatory virtual destructor.
  virtual ~ModuleAPIFileImpl();

  /// Initialize the module.
  virtual int init(const std::vector<Port*>& ports, const std::vector<Trace*>& traces);

  /// Abort method has no effect with this implementation
  virtual void abort();

  virtual Allocator *getAllocator() const;

  /// Alloc a memory buffer
  virtual BufferWrite alloc(size_t size);

  /// Alloc a memory buffer and initialize it with the specified std::string
  virtual BufferWrite allocString(const std::string& str);

  /// Realloc a memory buffer
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
    
  virtual int put(OutputPort* port, const StampList *stamplist);

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
  virtual bool isPortConnected( const Port* port ) const;

  /// Return the local host name.
  virtual std::string getHostName() const;

  /// Generate a unique 64 bits ID.
  virtual ID generateID();

  virtual Port *getPortByName( const std::string &strName ) const;

  virtual Trace *getTraceByName( const std::string &strName ) const;

  virtual bool isBoundByInports() const;

  virtual size_t getNumberOfInputPorts() const;
  virtual size_t getNumberOfOutputPorts() const;

 protected:

  /// Constructor. Called by ModuleAPIFactory.
  ModuleAPIFileImpl(const std::string& modulename,
		    const std::string& parentinfo,
		    const std::string& daemoninfo,
		    int flags=0);

  friend class ModuleAPIFactory;

  class InputPortData
  {
  public:
    InputPort* port;
    PortReader reader;
    bool repeat;
    flowvr::Message msg;
    InputPortData(InputPort* _port, const std::string& filename, bool _repeat)
    : port(_port), reader(filename), repeat(_repeat)
    {
    }
  };

  class OutputPortData
  {
  public:
    OutputPort* port;
    PortWriter writer;
    bool raw;
    int nextnum;

    OutputPortData(OutputPort* _port, const std::string& filename, bool _raw)
    : port(_port), writer(filename), raw(_raw), nextnum(0)
    {
    }
  };

  std::vector<InputPortData*> inputPorts;
  std::vector<OutputPortData*> outputPorts;
  std::string moduleName;
  std::string parentInfo;
  std::string daemonInfo;
  int flags;
  int status;
  int it;

}; // class ModuleAPIFileImpl

} // namespace flowvr

#endif
