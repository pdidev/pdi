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
#ifndef FLOWVR_PLUGD_MESSAGEQUEUE_H
#define FLOWVR_PLUGD_MESSAGEQUEUE_H

#include "flowvr/message.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/mtsignal.h"
#include "flowvr/plugd/actionhandler.h"
#include "flowvr/xml.h"
#include "flowvr/stamp.h"
#include "flowvr/trace.h"

#include <deque>

namespace flowvr
{

namespace plugd
{

/// MessageQueue, messages are sorted by number.
///
/// This class uses the lock given by the parent. This allows that the parent may use
/// one single global lock or one lock per message queue. The parent class must
/// implement the methods messageQueueLock() returning the lock to use.
/// When a message queue receives a message, it will
/// - queue the message
/// - call Parent::newMessageNotification() method.
/// In case a new stampListSpecification arrived, this class will read the
/// stampListSpecification and call the Parent::
///
/// For all method calls except ActionHandler ones, the lock must be currently
/// taken.
/// The queue stores the <i>oldest</i> message in <b>front</b>, the <i>newest</i> ones in the <b>back</b>.
/// the sequence number num0 gives the <b>absolute index</b> in terms of number of
/// messages received by this queue.
/// @see frontNum()
/// @see backNum()

template<class Parent, class Locker>
class MessageQueue: public ActionHandler
{
protected:

	/// @name Internal data
	/// @{

	typedef std::deque<Message> Queue;

	/// Message buffer
	Queue queue;

	/// Number of the last read message. All previous messages are removed.
	int num0;

	/// Number of registered actionhandlers.
	int nbactions;

	/// Stamps specification
	StampList stamplist;

	/// To know if the stamplist specification has been received
	bool stampsreceived;

	std::string name; ///< Optional name;

	bool couldBeDisconnected; ///< true means the port could be disconnected online. Num is updated if num < num0. WARNING: But the stamps in message is not updated

	/// @}

public:

	/// @name External data
	/// @{

	///Stamps specification
	//StampList stamplist;

	Parent* parent; ///< Mandatory parent
	int id; ///< Optional ID;
	TypedTrace<int> trace;

	/// Signal
	//Each message queue owns a signal controlled by the object with the messagequeue
	ipc::MTSignal signalStart;

	/// @}

	/// @name ActionHandler methods
	/// @{

	MessageQueue() :
		queue(), num0(0), nbactions(0), stamplist(), stampsreceived(false),
				name(), couldBeDisconnected(false), parent(NULL), id(0), trace(
						"undefined")
	{}

	virtual ~MessageQueue()	{}

	/// Create an ActionHandler for batch mode action execution.
	virtual ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot)
	{
		if (parent == NULL)
			return NULL;
		Locker locker(parent->messageQueueLock(id),
				"MessageQueue::createAction");

		++nbactions;
		return this;
	}

	int getNbActions() const
	{
		return nbactions;
	}

	bool isConnected() const
	{
		return nbactions != 0;
	}

	StampList& getStampList()
	{
		return stamplist;
	}

	/// Execute the action processing the specified message.
	/// The dispatcher parameter is used to dispatch any generated message
	virtual void doIt(const Message& msg, Dispatcher* dispatcher)
	{

		// no parent, no fun... (can that happen?)
		if (parent == NULL)
			return;


		int num;
		bool notify = false;
		{
			// scoped locker as we are accessing the message queue
			// @remark here we think, we get the lock for id, while in reality we
			//         get the global lock and lock all queues
			Locker locker(parent->messageQueueLock(id), "MessageQueue::doIt");

			// read num stamps and put it into the num variable
			if (!msg.stamps.read(stamplist.num, num))
			{
				std::cerr
						<< "flowvr.plugd.MessageQueue.doIt: num stamp not found, ignoring message"
						<< std::endl;
				return;
			}


			if (num == -1)
			{ // new stamp specification
				StampListSpecification specif;
				std::string xml;
				if (!msg.stamps.read(specif.spec, xml))
				{
					std::cerr
							<< "flowvr.plugd.MessageQueue.doIt: received malformed stamps specification message"
							<< std::endl;
					return;
				}
				xml::DOMParser parse;
				if (parse.parseString(xml.c_str()) == 0)
				{
					stamplist.updateFromXML(parse.getDocument()->RootElement());
					stampsreceived = true;
					notify = true;
				}
				else
				{
					std::cerr
							<< "flowvr.plugd.MessageQueue.doIt: received malformed XML stamps specification"
							<< std::endl;
				}
			}

                        // At this point  num can differ from the num stamp of the message
			if (couldBeDisconnected && num < num0 && num != -1)
			{
				num = num0 + (int) queue.size();
			}


			if (num >= num0)
			{ // valid message

				// Trace it
				int nummsg;
				msg.stamps.read(stamplist.num, nummsg);
				trace.write(nummsg);

                                //Fill the queue with NULL message to ensure 
                                // the message with stamp num get located at the 
                                // num absolute  position in the queue
				while (num >= num0 + (int) queue.size())
					queue.push_back(Message::Null);


				// entail the current message
				queue[num - num0] = Message(msg);
				notify = true;
			}

			// we received a new message / stamp-specification
			if (notify)
			{
				if (num == -1)
				{
					// call update on new stamp-list for my parent
					parent->newStampListSpecification(id, msg, dispatcher);
				}
				else
					// call update on new message for my parent
					parent->newMessageNotification(id, num, msg, dispatcher);
			}
		}
	}



	/// Remove this ActionHandler.
	/// This will destroy this instance if ActionHandlers are dynamically
	/// instantiated or it will remove one reference.
	/// @todo check this implementation for messagequeue
	virtual void remove()
	{
		if (parent == NULL)
			return;

		/// @todo what happens when double locking?
		Locker locker(parent->messageQueueLock(id), "MessageQueue::remove");
		--nbactions;
	}

	/// @}


	/**
	 *  Get the front message.
	 *  In case there is no front message, Message::Null is returned.
	 */
	const Message& frontMsg() const
	{
		if (queue.empty())
			return Message::Null;
		else
			return queue.front();
	}

	/**
	 * Get the back message.
	 * In case there is no back message (empty queue), Message::Null
	 * is returned.
	 */
	const Message& backMsg() const
	{
		if (queue.empty())
			return Message::Null;
		else
			return queue.back();
	}

	/// Erase the front message.
	void eraseFront()
	{
		if (!queue.empty())
			queue.pop_front();
		++num0;
	}


	/**
	 *  Erase the messages before the specified num. num becomes the new front.
	 *  If the queue is empty, num will be the new front index.
	 *  If the queue is not empty, the front element is popped until
	 *  num0 becomes num.
	 */
	void setFront(int num)
	{
		while (num0 < num)
		{
			if (queue.empty())
				num0 = num;
			else
			{
				queue.pop_front();
				++num0;
			}
		}
	}

	/**
	 * Clear content of one message in the queue
	 * This does not mean that this message is erase/removed from the queue, it just means
	 * that the data and stamps are cleared (effectively rendering this an invalid message)
	 * @param num the 'absolute' index of the message, not the 'relative'
	          index in the queue
	 */

	void erase(int num)
	{
		// first check if the index given in num
		// is in the range of ids currently in the queue
		// so:
		if (num >= num0                      // is it greater than num0 (oldest message?)
		 && num < num0 + (int) queue.size()) // and is it smaller than num0 plus the number of messages?
			queue[num - num0].clear(); // it is, so clear the content of the  message,
		                               // using its relative index to num0
	}

	/// @}


	/// @name Indexed access
	/// @{

	/// Get the queue size
	int size() const
	{
		return queue.size();
	}


	/// Get the front message number (the first message which can still be read).
	/// the front message is the <b>oldest</b> message index received
	/// An empty queue that has not yet received any message has the frontNum 0
	int frontNum() const
	{
		return num0;
	}


	/// Get the queue back num
	/// the backNum indicates the youngest message received.
	/// For an empty queue, the backNum is -1
	int backNum() const
	{
		return frontNum() + size() - 1;
	}

	/// Check if the queue is empty
	int empty() const
	{
		return queue.empty();
	}

	/**
	 *  Get one message given its number. Return an empty message if not available.
	 *  @param num the absolute index number, not the queue index
	 *         - num < frontNum(): the message index is older than the oldest available in this queue
	 *         - num >= (num0 + size()): the index is younger than the one available in this queue (returns Message::Null)
	 *  @return either
	 *          - the message (unremoved) to have the <b>absolute</b> message index num
	 *          - Message::Null
	 */
	const Message& get(int num) const
	{
		if (num < num0 || num >= num0 + (int) queue.size())
			return Message::Null;
		else
			return queue[num - num0];
	}

	/**
	 * thats an alias function/operator for get()
	 * @see get()
	 */
	const Message& operator[](int num) const
	{
		return get(num);
	}

	/// @}

	void setName(const std::string &myname)
	{
		name = myname;
		trace.setName(myname);
	}

	std::string getName() const
	{
		return name;
	}

	bool stampsReceived() const
	{
		return stampsreceived;
	}

	/**
	 * \brief could be disconnected only... update properly num
	 */
	void setCouldBeDisconnected()
	{
		couldBeDisconnected = true;
	}

};

} // namespace plugd

} // namespace flowvr


#endif
