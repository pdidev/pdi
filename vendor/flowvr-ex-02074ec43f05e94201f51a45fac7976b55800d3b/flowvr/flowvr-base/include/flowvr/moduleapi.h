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
* File: include/flowvr/moduleapi.h                                *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_MODULEAPI_H
#define FLOWVR_MODULEAPI_H

#include "flowvr/buffer.h"
#include "flowvr/stamp.h"
#include "flowvr/trace.h"
#include "flowvr/id.h"
//#include "flowvr/topo.h"
//#include <hwloc.h>

#include <string>
#include <vector>


namespace flowvr
{

class ModuleAPI;

class Message;
class MessagePut;
class MessageWrite;

/**
 * @brief basic interface for ports
 *
 * A port can be used to query for the module it is associated to.
 * Ports should outlive their module and have to be cleaned up by module code.
 *
 * @ingroup Messagehandling
 */
class Port
{
 public:
	/**
	 * @param myname the symbolic name for the port.
	          must be unique under all inports and outports.
	          This is not enforced here, but gives you trouble when
	          you try to recover the port from the moduleapi by name.
	 * @param mystamps the stamplist to use for this port.
	          when you pass a pointer here, it is assumed that
	          you will cleanup the pointer upon module exit.
	          If that is not the case (you do want the pointer to
	          be deleted once the port is deleted, pass bOwn=true
	          as next parameter. In case you pass NULL here, the port
	          will create a default stamplist, and destroy that upon
	          a call to the destructor.
	 * @param bOwn this parameter is only used when you pass a pointer
	          in mystamps. In case you want the stamplist to be deleted
	          upon destruction of the port, pass 'true' here.
	          Default is 'false'.
	 */
  Port(const std::string& myname, StampList* mystamps=NULL, bool bOwn = false);

  /**
   * @brief destuctor
   *
   * If the stamplist passed during construction was marked as being 'owned',
   * it is deleted by when calling the destructor. Otherwise, the pointer is
   * just forgotten.
   */
  virtual ~Port();

  /**
   * @brief test method for direction (input)
   *
   *
   * test if this port can receive some data (e.g., is a subclass of InputPort)
   * @return true if this port can receive data
   */

  virtual bool isInput() const = 0;

  /**
   * @brief test method for direction (output)
   *
   *
   * test if this port can send some data (e.g., is a subclass of OutputPort)
   *
   * @return true if this port can send data
   */
  virtual bool isOutput() const = 0;

  /**
   * @brief Returns the module associated to this port
   *
   * @return the parent module of this port or NULL if this port was not assigned
   */
  ModuleAPI* getModule() const { return module; }

  /**
   * @brief check connectivity.
   *
   * Check if this port is connected (only valid after the first wait).
   *
   * @return true when this port is connected to another port in the
   *         flowvr network.
   */
  bool isConnected() const;


  std::string name;  ///< Human readable name of the port
  StampList* stamps; ///< Descriptions of the stamps associated with this port.

 protected:

  ModuleAPI* module; ///< Module associated to this port
  void* privateInfo; ///< Data used by ModuleAPI Implementation
  bool  stampsOwned; ///< marker to mark ownership of stamplist passed during construction */


  friend class ModuleAPI; // ModuleAPI sets and reads the privateInfo;

  TypedTrace<int> trace; ///< Trace of each message's it stamp
};

/**
 * @brief port to receive data from
 *
 * InputPorts can be marked 'non-blocking' during construction and before a call
 * to ModuleAPI::init(). A 'non-blocking' port will not be considered during the
 * update of incoming messages by the daemon regulator. Thus, invalid (no-data/no-stamps)
 * messages can occur on a non-blocking port after wait. This feature is typically used
 * for event routes, e.g. inports with 'low-update-frequency' that should not dominate
 * the iteration frequency of a module.
 *
 *
 * @ingroup Messagehandling
 */
class InputPort : public Port
{
 public:
	 /**
	  * @param the name of the port
	  * @param mystamps pointer to a specific stamps list for this port
	  * @param bIsNonBlockingPort false causes default behavior: module-wait
	  *         will delay processing until a message arrived,
	  *         if set to true, the state of the port is not considered when
	  *         waiting for new messages. Default: false, standard behavior,
	  *         the port is blocking the processing of the module until
	  *         a new message arrived at all ports.
	  */
  InputPort(const std::string& myname,
		    StampList* mystamps=NULL,
		    bool bOwnStampList = false,
		    bool bIsNonBlockingPort = false);



  /**
   * @return true
   */
  virtual bool isInput() const;


  /**
   * @return false
   */
  virtual bool isOutput() const;

  /**
   * @return true in case this port is not blocking the wait of the module,
             e.g., a sampling port. Non-blocking ports can contain invalid,
             or empty messages.
   */
  bool isNonBlockingPort() const;

  /**
   * change the blocking state for this port, the change is only affecting
   * the way the port is treated <b>before</b> a it was passed to initModule().
   * Once passed, the change of the blocking state is not regarded.
   * @param bBlock false causes default behavior. true defines the port as non
   * 		blocking.
   */
  void setNonBlockingFlag(bool bBlock);

 private:
  bool isNonBlocking; /**< have to store this state in a member here,
                                 as we do not have access to the flags field
                                 from underlying sub-system used for initialization */
};

/**
 * @brief class that can send messages
 *
 * @ingroup Messagehandling
 */

class OutputPort : public Port
{
 public:
  OutputPort(const std::string& myname, StampList* mystamps=NULL, bool bOwn = false);
  /// Returns false.
  virtual bool isInput() const;
  /// Returns true.
  virtual bool isOutput() const;
};

class Buffer;

enum ModuleStatus
{
  StatusError = 0,
  StatusInit,
  StatusWait,
  StatusRun,
  StatusAbort
};

/**
 * @brief basic interface for module implementations
 *
 * A module API interfaces with the daemon to exchange messages.
 * It serves the following purposes:
 *
 *  - it manages ports and connects to the daemon
 *  - implements put() and get() and wait() methods
 *    to do the state management of modules.
 *  - this interface can be used to query for the Allocator,
 *    which has to be used to reserve message memory.
 *  - the topology interface can be used to assign a module to run
 *    on a dedicated set of CPUs available
 *
 * @ingroup Messagehandling
 */
class ModuleAPI
{
 public:

  /**
   * @brief constructor
   *
   * Initializes the local topology to use to the simplest model (use operating
   * system scheduling).
   */
  ModuleAPI();


  /**
   * @brief empty destuctor.
   *
   * Note that users should call close() before deleting the module.
   */
  virtual ~ModuleAPI();

  /// @name Module API
  /// @{

  /**
   *  @brief Initialize the module.
   *
   *  This connects to the daemon and register the module.
   *
   *  @param ports the vector of ports to register. Pointers given here should outlive
   *         the ModuleAPI and have to be deleted by the module itself (i.e.,
   *         the ownership is on the side of the user)
   *  @param traces vector of event traces to trace
   *
   *  @todo more information about the traces
   */
  virtual int init(const std::vector<Port*> & ports,
		           const std::vector<Trace*>& traces=std::vector<Trace*>() ) = 0;

  /**
   * @brief Ask the launcher to stop the application
   *
   *  Stops the application from a module as if it was requested from the prompt of the FlowVR
   * launcher. Can be called from any module after initialisation. May be called
   * several times safely.
   */
  virtual void abort() = 0;

  /// Explicitly close the current module.
  /// This API can be used to close a module. Closing a module means to
  /// disable communication with the daemon and releasing all module
  /// related resources.
  virtual int close()=0;

  /// @}

  /// @name state management / messaging
  /// @{

  /**
   * @brief Wait for the next iteration.
   *
   * Puts a module into sleep mode until a condition is reached
   * determined by the deamon to wake up again.
   * Typically this means that new messages are available on the
   * input ports. But as well, the daemon can decide to stop the
   * module and end the application.
   *
   * @return a return code (0 = end iterating)
   *
   */
  virtual int wait()=0;

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
  virtual int get(InputPort* port, Message& message)=0;

  /// Send a new message to an output port.
  ///
  /// Note: The message buffer should not be modified by the module
  /// after calling this method.
  ///
  virtual int put(OutputPort* port, MessageWrite& message)=0;

  /// Send a new message to an output port.
  ///
  /// Note: The message buffer should not be modified by the module
  /// after calling this method.
  virtual int put(OutputPort* port, MessagePut& message)=0;

  /// Send a new stamplist to an output port.
  ///
  /// Note: Should only be done after the first wait, before putting a message
  virtual int put(OutputPort* port, const StampList *)=0;
  /// @}

  /// @name Information retrieval
  /// @{

  /// Return the current state of this module.
  ///
  /// Each of the other methods also returns the current status after completion.
  /// The exact meaning of this have to be defined...
  virtual int getStatus() const =0;

  /// Get the module ID.
  virtual std::string getID() const =0;
  /// @}

  /// @name port query api
  /// @{

  /// Get a port source ID.
  virtual std::string getPortID(Port* port) const =0;

  /// Check if a port is connected (only valid after the first wait).
  virtual bool isPortConnected( const Port* port) const = 0;

  /**
   * @brief blocking state query
   *
   * This method can be used to see if a wait() will block until
   * all connected inports are assigned a message.
   * Non-connected or non-blocking inports do not bind the module.
   *
   * @return true iff wait() will possibly block, as blocking inports
   *         are connected. false else.
   */
  virtual bool isBoundByInports() const = 0;

  virtual size_t getNumberOfInputPorts() const = 0;
  virtual size_t getNumberOfOutputPorts() const = 0;

  /**
   * @brief retrieve a port by name from the vector of given port during
   *        init
   *
   * @return a port with name strName, or NULL if this port was not
   *         given during the call to init()
   */
  virtual Port *getPortByName( const std::string &strName ) const = 0;

  /**
   * @brief retrieve a trace by name from the vector of given trace during
   *        init
   *
   * @return a trace with name strName, or NULL if this trace was not
   *         given during the call to init()
   */
  virtual Trace *getTraceByName( const std::string &strName ) const = 0;

  /// @}


  /// @name memory allocation
  /// @{

  /**
   * @brief Alloc a new writeable buffer.
   *
   * Alias for getAllocator()->alloc().
   */
  virtual BufferWrite alloc(size_t size) = 0;

  /**
   * @brief Realloc a writeable buffer.
   *
   * Alias for getAllocator()->realloc()
   */

  virtual bool realloc(BufferWrite &buffer, size_t size, bool amortized=false) = 0;

  /**
   * @brief accessor for the allocator used for this module.
   *
   * return the allocator used for this module.
   */
  virtual Allocator *getAllocator() const = 0;


  /**
   * @brief macro function to assign a string to a BufferWrite object
   *
   * Alloc a shared memory buffer and initialize it with the specified std::string
   *
   * @param str the string to copy to the BufferWrite
   * @return a BufferWrite object containing str
   */
  virtual BufferWrite allocString(const std::string& str)=0;
  /// @}

  /// @name Topology
  /// @{
        //  virtual hwloc_cpuset_t getCPUMask() const;
        //  virtual bool           setCPUMask(hwloc_cpuset_t mask, const Topo &topo = Topo(false) );
        //  virtual int            getCurrentCPU() const;
  /// @}

  /// @name Utility functions
  /// @{

  /// Return the local host name.
  virtual std::string getHostName() const =0;

  /// Generate a unique 64 bits ID.
  virtual ID generateID() = 0;

  /// Log the module PID in directory defined by env variable FLOWVR_PID_LOG_DIR (flowvr-kill read from there)
  void writePID(const std::string &name = std::string(""));
  /// @}

 protected:
        //  Topo topology;
        //  hwloc_cpuset_t cpumask;

  // These functions must be there because of c++ friend classes handling

  void setPortPrivate(Port* port, void* info)
  {
	  port->privateInfo = info;
  }

  void* getPortPrivate(const Port* port) const
  {
	  return port->privateInfo;
  }

  void setPortModule(Port* port)
  {
	  port->module=this;
  }

  TypedTrace<int>* getPortTrace(Port* port) const
  {
    return &(port->trace);
  }

}; // class ModuleAPI

/// Registers and initializes the module. Returns NULL in case of error.
/**
 * @param ports the vector of input AND output ports as used by the module.
 *         note that each port name should be unique, this is implicitly assumed.
 * @param name the instance name of this module (suffix for the created ID)
 * @param modulename to use (infix of the created ID), if empty, the
 *         environment variable FLOWVR_MODNAME is used instead.
 **/
extern flowvr::ModuleAPI* initModule(const std::vector<flowvr::Port*> &ports,
				     const std::string &name = std::string(""),
				     const std::string &modulename = std::string(""));

/// Registers and initializes the module with traces. Returns NULL in case of error.
extern flowvr::ModuleAPI* initModule(const std::vector<flowvr::Port*> &ports,
				     const std::vector<flowvr::Trace*> &traces,
				     const std::string &instancename = std::string(""),
				     const std::string &modulename = std::string(""));

} // namespace flowvr

#endif
