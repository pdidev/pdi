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
* File:./src/plugins/filters/flowvr.plugins.FilterFinitieQueue.cpp*
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugins/filter.h"
#include <iostream>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;


/**
 * @brief A filter with a fixed size message queue. Can be used
          as a limiter when you experience long message queues
          on data sources that otherwise flood the network.
 *
 * This filter implements a "limiter" functionality. It maintains
 * a finite length queue. The number of messages stored (queue_length) is user
 * determined and given during initialization of the object.
 * A number of messages can be requested by giving a stamp
 * value on the 'trigger' port of this filter. The value is a number X:
 * - if X < 0,  send only the latest message
 * - if 0 < X <= queue_length, send X messages (wait for X messages to
 * arrive if there are not enough)
 * - if X > queue_length, set X = queue_length.
 *
 * The filter tries to maintain FIFO order, but only for message windows that
 * are sent. This means that once a trigger is dispatched ALL other messages (the unsent ones)
 * in the queue are removed and forgotten. For example: there are 10 messages in the queue,
 * numbered from 0 to 9 (0 the oldest, 9 the youngest). The user requests the 5 latest messages
 * by giving a trigger of 5 on the trigger port. This filter will
 * - send 5 messages [5], [6], [7], [8], [9] in that order
 * - remove all 10 messages from the queue.
 *
 * The default queue length is 1, in that case only one message
 * is kept in the queue and forwarded upon request.
 *
 * Use cases for this filter:
 * - you have a fast producer that floods your network <b>and</b> you know
     that you are not going to use all the messages, but only a <b>window</b>
     of messages
 * - you want to limit the memory consumed by a fast producer and you can assign
     the size of the memory to a message <b>and</b> you know that you are not
     going to need all the messages, but just a <b>window</b> of the latest messages.
 *
 * Side notes
 * - it is simple to create a long message queue at the 'trigger' port of this filter,
     as requests are pooled in FIFO order with no loss
 * - setting a window size of 1 will probably stress the access on the queue quite much,
     so that should be avoided
 * - if the trigger requests a number of messages to be delivered, this filter will
     inject the number of messages one by one. This is <b>not</b> a merge filter.
     In case you want to merge the messages, put a merge filter after this one.
 * - this is not a replacement for a greedy synchronization, for example this filter
     is not well placed when you have to synchronize multiple modules with the same
     information, as they have to agree on the trigger used.
 *
 * <i>Parameters</i> that can be given to this filter:
 * - <b>queue_length</b>: the maximal length of the queue in number of slots.
     This parameter must be >= 1. This property is enforced upon a call to
     init() of the FiniteQueue
 *
 * <i>Ports</i>
 * @inport{trigger,stamp,mandatory,
          Number of samples to forward. In case you forget to connect this port\,
          your system might deadlock. Note also that all messages coming in on this port should have a
          stamp of type int and called "trigger" attached to it.
          The value of this stamp determines the number of messages to forward\, unmerged.}
 * @inport{in,full,mandatory,
           The port to route incoming messages to.
           If the next incoming message is saturating the queue length property\,
           the oldest message in the queue is discarded and the new message is enqueued
           at the tail of the queue.}
 * @outport{out,full,Forwards a message (or a number of messages) from the queue of this filter.}
 */
class FiniteQueue : public Filter
{
public:
	FiniteQueue(const std::string &objID);

	virtual flowvr::plugd::Class* getClass() const;

	virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot,
									 flowvr::plugd::Dispatcher* dispatcher);

	virtual void newMessageNotification(int mqid,
									  int msgnum,
									  const Message& msg,
									  Dispatcher* dispatcher);
	virtual void newStampListSpecification(int mqid,
										 const Message& msg,
										 Dispatcher* dispatcher);


	enum
	{
		IDPORT_IN=0,  /**< the FULL message queue to store incoming messages */
		IDPORT_TRIGGER, /**< the STAMP message queue to specify the number of messages to forward */
		NBPORTS /**< stop sign */
	};


private:
   int nQueueLength; /**< internal state variable, noting the length of the window.
                          is set by the parameter queue_length given to this component. */


	virtual void handleNewMessage(int mqid,
								  int msgnum,
								  const Message& msg,
								  Dispatcher* dispatcher);

	virtual void handleNewTrigger(int mqid,
								  int msgnum,
								  const Message& msg,
								  Dispatcher* dispatcher);

	/**
	 * utility function to remove messages from the queue by
	 * popping off the front element one by one.
	 */
	virtual void purgeQueue();
};


FiniteQueue::FiniteQueue(const std::string &objID)
: Filter(objID)
, nQueueLength(1)
{

}


flowvr::plugd::Result FiniteQueue::init(flowvr::xml::DOMElement* xmlRoot,
									 flowvr::plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
	if (result.error())
		return result; // something already failed for the basic init, so do not continue

	xml::DOMNodeList* node;
	std::string nb;
	node = xmlRoot->getElementsByTagName("queue_length"); // find attribute set by user
	if (node->getLength() > 0)
	{
		nb = node->item(0)->getTextContent();
		nQueueLength = atoi(nb.c_str()); // convert to number

		if(nQueueLength <= 0) // we check the queue length directly here
		{
			delete node; // free memory
			return flowvr::plugd::Result(flowvr::plugd::Result::ERROR, "nQueueLength must be >= 1");
		}
	}

	delete node; // get rid of the memory



	initInputs(NBPORTS);
	inputs[IDPORT_IN]->setName("in"); // expect same type of message than retreived from "out" port
	inputs[IDPORT_TRIGGER]->setName("trigger");// STAMPS

	initOutputs(1);
	outputs[0]->setName("out");

	return result; // re-use the result object here
}


void FiniteQueue::purgeQueue()
{
	// pop off the front message until the queue is empty
	// we expect the queue to be locked all the time here
    while( !inputs[IDPORT_IN]->empty() )
    	inputs[IDPORT_IN]->eraseFront();
}


void FiniteQueue::newMessageNotification(int mqid,
		                              int msgnum,
		                              const Message& msg,
		                              Dispatcher* dispatcher)
{
	switch(mqid)
	{
		case IDPORT_TRIGGER:
		{
			// new request for old messages
			handleNewTrigger(mqid,msgnum,msg,dispatcher);
			break;
		}
		case IDPORT_IN:
		{
			// new messages to inspect and maybe some
			// old to throw away.
			handleNewMessage(mqid,msgnum,msg,dispatcher);
			break;
		}
		default:
			break;
	}
}

void FiniteQueue::newStampListSpecification(int mqid,
		                                 const Message& msg,
		                                 Dispatcher* dispatcher)
{
	if (mqid == IDPORT_IN)
	{
		//forwarding stamplist specification to the OMQ
		outputs[0]->stamps = inputs[mqid]->getStampList();
	    outputs[0]->msgtype = (msg.data.valid() ? Message::FULL : Message::STAMPS);
		outputs[0]->newStampSpecification(dispatcher);
	}
}

void FiniteQueue::handleNewMessage(int mqid,
		                              int msgnum,
		                              const Message& msg,
		                              Dispatcher* dispatcher)
{
	// check constraint: is the queue already saturated?
	if( inputs[mqid]->size() > nQueueLength )
	{
		// we have to discard the oldest ones
		int nSize    = inputs[mqid]->size();  // use a variable for readability
		int distance =  nSize - nQueueLength; // determine the number of messages to purge

		// note that this "newest message" given as an argument is already in the queue
		// as this callback is performed after the message was already
		// entailed. That means, we iterate below only until at least this
		// new message is left in the queue. We have ensured during
		// initialization that the queue_length is not 0.
		while(distance--)
			inputs[mqid]->eraseFront(); // pop off old messages
	}

	// ok, queue size now as desired. However, it might be that there are
	// pending request pooling up in the IDPORT_TRIGGER queue.
	if( !inputs[IDPORT_TRIGGER]->empty() ) // check simply by seeing if the queue is not empty
	{
		// we want to call the API as it is already defined, so we have
		// to process the oldest message in the trigger queue for handling the
		// the request properly.
		const Message &msg = inputs[IDPORT_TRIGGER]->frontMsg();
		handleNewTrigger(IDPORT_TRIGGER,  inputs[IDPORT_TRIGGER]->frontNum(), msg, dispatcher );
	}
}


void FiniteQueue::handleNewTrigger(int mqid,
		                                 int msgnum,
		                                 const Message& msg,
		                                 Dispatcher* dispatcher)
{
	// determine how many items to deliver
	int nToDeliver;
	StampInfo *trigger_info = inputs[IDPORT_TRIGGER]->getStampList()["trigger"];
	if(trigger_info)
	{
		// ok, this is an extended message, specially written for this filter...

		// read the number of messages to deliver off the stamp
		if(msg.stamps.read((*trigger_info), nToDeliver))
		{
			// clamp the deliver value against the constraints, given by
			// - deliver no more than the absolute queue_length is (is a user error,
			//   but if we do, we might deadlock the application)
			nToDeliver = std::min<int>(nToDeliver, nQueueLength);

			// - do not deliver more than we have right now:
			//   pool this request, when no or not enough messages were already delivered
			if(nToDeliver > inputs[IDPORT_IN]->size())
				return; // note that we do not remove this trigger

		}
		else
		{
			// Convenience:
			// we assume that now the latest sample is desired... send that...
			nToDeliver = 1;
		}
	}
	else
	{
		// possibly someone connected a stamps output to the trigger port (endIt?)
		// we can not derive the number of desired samples, we assume the request is
		// just for one item (the youngest)
		nToDeliver = 1;
	}

	// for the sake of readability, we distinguish two cases:
	// 1) we deliver a batch of messages
	// 2) we deliver only the latest message
	if( nToDeliver > 1 )
	{
//		// we simply deliver the desired number of messages one by one,
		// no merge.
		// the order will still be FIFO, but we start from tail - nToDeliver
		// and go up until we transmitted them all

		int nSize    = inputs[IDPORT_IN]->size();
		int nBackNum = inputs[IDPORT_IN]->backNum();
		int nCount   = nToDeliver;

		// we put the num given by the trigger as a number on the outgoing
		// messages.

		/**
		 *  @todo still have to confirm if that is complying with
		          what the number is supposed to express
		 */

		int num;
		msg.stamps.read(inputs[IDPORT_TRIGGER]->getStampList().num,num);

		while(nCount)
		{
			// short note: we are going to purge the complete queue after
			// all the sends, so there is no need to modify (e.g., remove)
			// the messages from the inputs queue right now.
			// Another note: we have to normalize the index by 1 (as the first valid
			// index for any message is 1 and not 0)
			const Message &msg2Send = inputs[IDPORT_IN]->get(nBackNum - nCount + 1);

			MessagePut newmsg;
			// for this message we clone the stamp list to the new message
		    newmsg.stamps.clone(msg2Send.stamps,&inputs[IDPORT_IN]->getStampList());

		    // assign data
		    newmsg.data=msg2Send.data;

			// put to outbound queue
			outputs[0]->put(newmsg,dispatcher,num);
			--nCount; // one down, nCount to go ;)
		}

		// erase the complete queue
		purgeQueue();
	}
	else
	{
		// thats the simple case, as we have only to forward the "latest" message (the one from the
		// end of the queue)

		// we have to get this youngest message from the IDPORT_IN queue
		if(inputs[IDPORT_IN]->empty())
		{
			return; // sampled before any pending input arrived...
			        // note that we leave the trigger in the queue for
			        // a new processing when data arrives...
		}


		const Message &youngest = inputs[IDPORT_IN]->backMsg();
		if(!youngest.valid())
		{
			// for sanity reasons... should not happen
			/// @todo maybe remove this check whether the youngest message is valid or not...
//			std::cerr << "Youngest message was not valid!?" << std::endl;

			// leave the trigger in the queue
			return;
		}
		else
		{
			MessagePut newmsg;
			newmsg.stamps.clone(youngest.stamps,&inputs[IDPORT_IN]->getStampList());
			newmsg.data=youngest.data;

			// now: we have to discard all messages
			purgeQueue();

			// get the it from the stamps (for statistics)
			/**
			 *  @todo still have to check whether this behaviour is
			          desired by the specification
			 */
			int num;
			msg.stamps.read(inputs[IDPORT_TRIGGER]->getStampList().num,num);


			// put to outbound queue
			outputs[0]->put(newmsg,dispatcher,num);
		}
	}

	// we erase *just this one request* from the list of triggers.
	inputs[IDPORT_TRIGGER]->eraseFront();
}


flowvr::plugd::GenClass<FiniteQueue>
						FiniteQueueClass("flowvr.plugins.FilterFiniteQueue", // name
					    "A filter that keeps a finite number of messages in 'in' and delivers \
					    a number specified in 'trigger' messages in FIFO order. \
					    can be seen as a 'limiter' to avoid flooding'."); // description

Class* FiniteQueue::getClass() const
{
  return &FiniteQueueClass;
}


} // namespace plugins

} // namespace flowvr

