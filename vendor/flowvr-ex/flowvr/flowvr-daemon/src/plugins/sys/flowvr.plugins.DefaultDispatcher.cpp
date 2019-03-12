/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
* File: src/plugins/flowvr.plugins.DefaultDispatcher.cpp          *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/routingtable.h"
#include "flowvr/plugd/actionhandler.h"

#include "flowvr/plugins/dispatcherobject.h"

#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/stamp.h"
#include <iostream>
#include <queue>
#include <unistd.h>

#include"flowvr/ipc/mtlock.h"
#include"flowvr/ipc/locker.h"

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

class DefaultDispatcher : public flowvr::plugins::DispatcherObject
{
public:

  RoutingTable table;
  ActionHandler* controlMessageHandler;
  ActionHandler* remoteControlMessageHandler;

  StampList stamps;

  class ThreadDispatcher : public flowvr::plugd::Dispatcher
  {
  public:
    DefaultDispatcher* parent;

    //modification Lo�ck
    typedef std::pair<const std::vector<Action*>*,Message> Entry;

    //queue of vectors of actions
    std::queue<Entry> actionQueue;
    ipc::MTLock       queueLock;
    std::queue<Entry> workQueue;
    ipc::MTLock       workLock;

    ThreadDispatcher(DefaultDispatcher* _parent)
      : parent(_parent)
      , queueLock( "queue" )
      , workLock( "work" )
    {
    }

    virtual ~ThreadDispatcher()
    {
    }


    /// Process a new message (called by the output message queues)
    virtual void process(const Message& msg, const std::vector<Action*>* actionvector)
    {
        // MAIN METHOD
        if ( actionvector == NULL )
            return;
        // there is at least one action linked to this source
        ipc::TryScoped<ipc::MTLock> wlocker( workLock );
        {
            ipc::ScopedMTLock qlocker( queueLock );
            if ( wlocker.is_locked() ) {
                // will work, put all pending actions to the work list
                swap( actionQueue, workQueue );
                workQueue.push( Entry(actionvector,msg) );
            } else {
                // won't work, only add to the pending queue
                actionQueue.push( Entry(actionvector,msg) );
                return;
            }
        }
        // 'workLock' is acquired
        do {
            // perform actions from 'workQueue'
            do {
                // pop an action
                const Entry & entry = workQueue.front();
                const std::vector<Action*> & actionvec = * entry.first;
                Message m = entry.second;
                workQueue.pop();
                // then perform it
                for ( size_t i = 0  ;  i < actionvec.size()  ;  i++ ) {
                    // \todo make 'action->doIt' add new actions to workQueue ??
                    actionvec[i]->doIt(m,this);
                    actionvec[i]->inc();
                }
            } while ( ! workQueue.empty() );
            // you must get more work to avoid deadlocks
            ipc::ScopedMTLock qlocker( queueLock );
            swap( actionQueue, workQueue );
        } while ( ! workQueue.empty() );
    }

    /// Process a new message.
    virtual void process(const Message& msg)
    {
      // MAIN METHOD

      //find the source and the message type of the message
      std::string source;
      Message::Type msgtype;
      if (!msg.stamps.read(parent->stamps.source,source))
	{
	  std::cerr<<"Dispatching error: message source stamp not found"<<std::endl;
	  return;
	}
      msgtype = msg.getType();

      //get the list action
      const std::vector<Action*> * actionvector;
      {
	//get the routing table
	RoutingTable* table =getRoutingTable();
	ipc::ScopedMTLock locker(table->lockIterate(),"flowvr.plugins.DefaultDispatcher.ThreadDispatcher.process");
	actionvector = table->getAction(source,msgtype);
      }

      //call the main process method
      process(msg, actionvector);

    }

    /// Generate a dispatcher for a new thread.
    virtual Dispatcher* threadCopy()
    {
      return new ThreadDispatcher(parent);
    }

    /// Get the routing table.
    virtual RoutingTable* getRoutingTable()
    {
      return &parent->table;
    }

    /// Signal the the dispatcher is not used anymore.
    virtual void close()
    {
      delete this;
    }


    /// Process a locally-generated control message.
    ///
    /// This method should be used by any object sending a news command or by a controller's regulator.
    virtual void processControlMessage(const Message& msg)
    {
      // to avoid race conditions at initialization, wait a little if no handler yet
      for (int i=0;i<10 && parent->controlMessageHandler==NULL;i++) sleep(1);
      if (parent->controlMessageHandler!=NULL)
        parent->controlMessageHandler->doIt(msg,this);
    }

    /// Process a remotely-generated control message.
    ///
    /// This method should be used by any network handler receiving a control message.
    virtual void processRemoteControlMessage(const Message& msg)
    {
      // to avoid race conditions at initialization, wait a little if no handler yet
      for (int i=0;i<10 && parent->remoteControlMessageHandler==NULL;i++) sleep(1);
      if (parent->remoteControlMessageHandler!=NULL)
        parent->remoteControlMessageHandler->doIt(msg,this);
    }

    /// Set the ActionHandler for locally-generated control messages.
    virtual void setControlMessageHandler(ActionHandler* handler)
    {
      if (parent->controlMessageHandler!=NULL)
        parent->controlMessageHandler->remove();
      parent->controlMessageHandler=handler;
    }

    /// Set the ActionHandler for remotely-generated control messages.
    virtual void setRemoteControlMessageHandler(ActionHandler* handler)
    {
      if (parent->remoteControlMessageHandler!=NULL)
        parent->remoteControlMessageHandler->remove();
      parent->remoteControlMessageHandler=handler;
    }

  };

  ThreadDispatcher* dispatch;

  /// Constructor.
  DefaultDispatcher(std::string objID)
    : DispatcherObject(objID), controlMessageHandler(NULL), remoteControlMessageHandler(NULL), dispatch(NULL)
  {
  }

  virtual Class* getClass() const;

  /// Initialization. Returns a XML document containing the result.
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher)
  {
    flowvr::plugd::Result result = DispatcherObject::init(xmlRoot, dispatcher);
    dispatch = new ThreadDispatcher(this);
    return result;
  }
  
  /// Get the dispatcher.
  virtual Dispatcher* getDispatcher()
  {
    return dispatch;
  }

  /// Execute an action in immediate mode.
  virtual flowvr::plugd::Result doAction(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher)
  { // this object doesn't implement any action
    return Result(flowvr::plugd::Result::ERROR,"No action");
    //return std::string("<result status=\"error\">No action</result>");
  }

  /// Create an ActionHandler for batch mode action execution.
  virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot)
  { // this object doesn't implement any action
    return NULL;
  }

};

flowvr::plugd::GenClass<DefaultDispatcher> DefaultDispatcherClass
(
 "flowvr.plugins.DefaultDispatcher", // name
 "", // description
 "flowvr.plugins.Dispatcher" // base
);

Class* DefaultDispatcher::getClass() const
{
  return &DefaultDispatcherClass;
}

} // namespace plugins

} // namespace flowvr
