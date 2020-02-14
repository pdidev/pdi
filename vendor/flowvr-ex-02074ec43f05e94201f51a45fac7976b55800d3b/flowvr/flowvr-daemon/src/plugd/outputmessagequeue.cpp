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
* File: include/flowvr/plugd/messagequeue.h                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/plugd/outputmessagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/daemon.h"

namespace flowvr
{

namespace plugd
{

OutputMessageQueue::OutputMessageQueue(const std::string &ObjectID)
:  trace("undefined")
, objectID(ObjectID)
, active(false)
, valid(false)
, msgtype(Message::FULL)
{
	count = 0;
	numout = 0;
}

/// Execute the action processing the specified message.
/// The dispatcher parameter is used to dispatch any generated message
void OutputMessageQueue::put(MessagePut msg, Dispatcher* dispatcher, int num)
{
	{
		// a lock is needed to ensure that the active status will not change during this steps
		ipc::ScopedMTLock slock(locker, "outputmessagequeue put");

		if (!valid)
		{
			std::cerr << "ERROR: OutputMessageQueue::put called without stamps specification on "
					<< source << std::endl;
			return;
		}

		// check message type
		if (msg.data.valid())
		{
			if (msgtype != Message::FULL)
			{
				std::cerr
						<< "ERROR: OutputMessageQueue::put called with a FULL message on a STAMPS port "
						<< source << std::endl;
				return;
			}
		}
		else
		{
			if (msgtype != Message::STAMPS)
			{
				std::cerr
						<< "ERROR: OutputMessageQueue::put called with a STAMPS message on a FULL port "
						<< source << std::endl;
				return;
			}
		}

		// set the number of the message to send
		if (num != -1)
		{
			numout = num;
			msg.stamps.write(stamps.num, num);
		}
		else
		{
			msg.stamps.write(stamps.num, numout.exchange_and_add(1));
		}

		msg.stamps.write(stamps.source, source);

		// if the output message queue is non active then buffer the message
		if (!active)
		{
			buffer.push_back(msg);
			return;
		}
		else
		{
			count++;
		}

		// Trace it
		int nummsg;
		msg.stamps.read(stamps.num, nummsg);
		trace.write(nummsg);

		//no need lock anymore
	}
	dispatcher->process(msg, actionvector);
}

/// to stop the dataflow in order to pause a flowvr application
int OutputMessageQueue::pause()
{
  // lock is needed to be sure that the value will not change during this call
  ipc::ScopedMTLock(locker,"outputmessagequeue pause");
  if (!active)
  {
    std::cerr<<"WARNING: Duplicate pause on "<<source<<std::endl;
  }
  else
  {
    active=false;
  }
  return count;
}

/// used when the application is launching or relaunching (no difference is made between a launch of a new application and a pause/restart)
void OutputMessageQueue::start(Dispatcher* dispatcher,int it)
{
  ipc::ScopedMTLock slock(locker,"outputmessagequeue start");

  if (source.empty())
  {
    if (flowvr::daemon::verboseLevel > 0) {
      std::cerr<<"ERROR: OutputMessageQueue::setName must be called before start() on "<<source<<std::endl;
    }
    return;
  }

  if (active)
  {
    std::cerr<<"WARNING: Duplicate start on "<<source<<std::endl;
    return;
  }

  //set the active tag
  active=true;

  //call go if ready
  if (valid) go(dispatcher);
}

void OutputMessageQueue::newStampSpecification(Dispatcher* dispatcher, int it)
{
  ipc::ScopedMTLock slock(locker,"outputmessagequeue put");

  MessageWrite msgspc;

  if (valid)
  {
    std::cerr<<"WARNING: Duplicate stamp specification on "<<source<<std::endl;
  }

  //set the valid tag
  valid=true;

  StampListSpecification spec;

  msgspc.stamps.write(spec.source,source);
  msgspc.stamps.write(spec.it,it);
  msgspc.stamps.write(spec.num,-1);
  xml::DOMNode* xmlspec = stamps.generateXML();
  msgspc.stamps.write(spec.spec,xml::DOMWriter::toString(xmlspec));
  delete xmlspec;

  if (msgtype==Message::FULL)
  {
	///@todo share memory allocation here
    msgspc.data = Allocator::getAllocator()->alloc(0);
  }

  buffer.push_back(msgspc);
  //call go if ready
  if (active) go(dispatcher);
}


void OutputMessageQueue::go(Dispatcher* dispatcher)
{
  if (!active || !valid) return;

  //get the actionvector for this source
  actionvector=(dispatcher->getRoutingTable())->getAction(source,msgtype);

  //check if actionvector is correct
  if (actionvector == NULL || actionvector->empty())
  {
    const std::vector<Action*>* actionvector2 = dispatcher->getRoutingTable()->getAction(source,(msgtype==Message::FULL?Message::STAMPS:Message::FULL));
    if (actionvector2 != NULL && !actionvector2->empty())
    {
      std::cerr<<"WARNING: Connections for "<<(msgtype==Message::FULL?"STAMPS":"FULL")<<" messages on "<<(msgtype==Message::FULL?"FULL":"STAMPS")<<" port "<<source<<std::endl;
    }
  }

  //send the messages stored in the message queue
  while (!buffer.empty() && active)
  {
    Message m=buffer.front();
    buffer.pop_front();
    count++;

    // Trace it
    int nummsg;
    m.stamps.read(stamps.num,nummsg);
    trace.write(nummsg);

    //the process call can take some time and is required not to keep the lock during this call
    locker.unlock();
    dispatcher->process(m,actionvector);
    //get back the lock
    locker.lock();
  }
}

} // namespace plugins

} // namespace flowvr
