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
 * File: src/plugins/flowvr.plugins.GreedySynchronizor.cpp         *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/thread.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/plugins/synchronizor.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <iostream>
#include <list>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// \brief Greedy Synchronizor: A synchronizer which chooses the last available message.
///
/// <b>Init parameters:</b> none.
///
/// <b>Input ports:</b>
/// -  <b>stamps</b>: Stamps of the available messages to choose from.
/// -  <b>endIt</b>: Activation signal from the receiving module (should be connected to its endIt port).
///
/// <b>Output Ports:</b>
/// - <b>order</b>: Decisions orders (should be connected to the order port of a flowvr::plugins::FilterIt filter).

class GreedySynchronizor: public Synchronizor
{
public:

	GreedySynchronizor(const std::string objID);

	virtual ~GreedySynchronizor();

	virtual Class* getClass() const;

	virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot,
			flowvr::plugd::Dispatcher* dispatcher);

	virtual void newMessageNotification(int mqid, int msgnum,
			const Message& msg, Dispatcher* dispatcher);
	virtual void newStampListSpecification(int mqid, const Message& msg,
			Dispatcher* dispatcher);

	enum
	{
		IDPORT_STAMPS = 0, IDPORT_ENDIT, NBPORTS
	};

	enum
	{
		IDPORT_ORDER = 0,
	};

	int last_scratch_stamp;

	int minDiff;
	int maxDiff;
	int bufferSize;

protected:
	virtual void doStart(plugd::Dispatcher* dispatcher);
	virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

	TypedTrace<int> traceout;

	std::string stampname;
	StampInfo* stamp;

};

using namespace flowvr::xml;

/// Constructor.
GreedySynchronizor::GreedySynchronizor(const std::string objID) :
	Synchronizor(objID), last_scratch_stamp(-2), minDiff(0),
			maxDiff(0x7fffffff), bufferSize(4), traceout(TypedTrace<int> (
					"order")), stamp(NULL)
{
}

GreedySynchronizor::~GreedySynchronizor()
{
}

flowvr::plugd::Result GreedySynchronizor::init(
		flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Synchronizor::init(xmlRoot, dispatcher);
	if (result.error())
		return result;

	xml::DOMNodeList* node;
	std::string nb;
	node = xmlRoot->getElementsByTagName("min");
	if (node->getLength() > 0)
	{
		nb = node->item(0)->getTextContent();
		minDiff = atoi(nb.c_str());
	}

	node = xmlRoot->getElementsByTagName("max");
	if (node->getLength() > 0)
	{
		nb = node->item(0)->getTextContent();
		maxDiff = atoi(nb.c_str());
		if (maxDiff <= 0)
			maxDiff = 0x7fffffff;
	}
	if (flowvr::daemon::verboseLevel >= 1)
		std::cout << objectID() << ": minDiff=" << minDiff << " maxDiff="
				<< maxDiff << std::endl;

	node = xmlRoot->getElementsByTagName("buffer");
	if (node->item(0) != NULL)
	{
		std::string val = node->item(0)->getTextContent();
		bufferSize = atoi(val.c_str());
		if (bufferSize <= 0)
			bufferSize = 4;
	}
	if (flowvr::daemon::verboseLevel >= 1)
		std::cout << name() << ": buffer=" << bufferSize << std::endl;

	node = xmlRoot->getElementsByTagName("stamp");
	if (node->getLength() >= 1)
		stampname = node->item(0)->getTextContent();
	else
		stampname = "it";

	delete node;

	initInputs(NBPORTS);
	inputs[IDPORT_STAMPS]->setName("stamps");
	inputs[IDPORT_ENDIT]->setName("endIt");

	initOutputs(2);
	outputs[IDPORT_ORDER]->setName("order");
	outputs[IDPORT_ORDER]->msgtype = Message::STAMPS;
	outputs[IDPORT_ORDER]->newStampSpecification(dispatcher);

	outputTraces.push_back(&traceout);

	result.setXML(xmlRoot);

	return result;
}

void GreedySynchronizor::newMessageNotification(int mqid, int msgnum,
		const Message& msg, Dispatcher* dispatcher)
{
	if (mqid == IDPORT_STAMPS)
	{ // new stamps: only keep the last message
		//ipc::ScopedMTLock locker(globalLock,"newMsg");
#ifdef DEBUG
		std::cout << objectID()<<": new stamps "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
	}
	else
	{
#ifdef DEBUG
		std::cout << objectID()<<": new endIt "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
	}
	if (isStarted())
		sendPendingOrders(dispatcher);
}

void GreedySynchronizor::newStampListSpecification(int mqid,
		const Message& msg, Dispatcher* dispatcher)
{
	if (mqid == IDPORT_STAMPS)
	{
		stamp = inputs[IDPORT_STAMPS]->getStampList()[stampname];
		if (stamp == NULL)
			std::cerr << objectID() << ":stamps ERROR stamp " << stampname
					<< " not found." << std::endl;
#ifdef DEBUG
		else
		std::cout << objectID() << ":stamps : stamp "<<stampname<<" @ "<<stamp->getOffset()<<std::endl;
#endif
	}
}

void GreedySynchronizor::doStart(plugd::Dispatcher* dispatcher)
{
	Synchronizor::doStart(dispatcher);
	// send thread
	if (inputs[IDPORT_STAMPS]->get(-1).valid())
	{
		newStampListSpecification(IDPORT_STAMPS,
				inputs[IDPORT_STAMPS]->get(-1), dispatcher);
	}
	sendPendingOrders(dispatcher);
}

void GreedySynchronizor::sendPendingOrders(plugd::Dispatcher* dispatcher)
{
	std::vector<int> orders;
	bool sendscratch = false;

	if (stamp == NULL)
	{
		if (flowvr::daemon::verboseLevel >= 1)
			std::cout << objectID() << " waiting for good stamps" << std::endl;
		return; // bad stamps
	}

	//ipc::ScopedMTLock locker(globalLock,"sendPendingOrders");
	while ((nbOrder < advance || inputs[IDPORT_ENDIT]->frontMsg().valid())
			&& !inputs[IDPORT_STAMPS]->empty())
	{
		int num0 = inputs[IDPORT_STAMPS]->frontNum();
		int num = inputs[IDPORT_STAMPS]->backNum();
		if (num - num0 > maxDiff - minDiff)
			num = num0 + maxDiff - minDiff;

		// find the last valid message within the given interval
		while (num > num0 && !inputs[IDPORT_STAMPS]->get(num).valid())
			--num;
		if (!inputs[IDPORT_STAMPS]->get(num).valid())
		{ // in case we have a msg on 'endIt' port, but no msg on 'stamp' port.
			orders.push_back(num);
			inputs[IDPORT_ENDIT]->eraseFront();
			break; // No more message available
		}

		if (inputs[IDPORT_STAMPS]->empty())
		{
#ifdef DEBUG
			std::cout << "la pile du port stamps est vide " << std::endl;
#endif
		}

		if (nbOrder >= advance)
			inputs[IDPORT_ENDIT]->eraseFront();
		++nbOrder;

		int stampval;
		Message msg = inputs[IDPORT_STAMPS]->get(num);
		msg.stamps.read(*stamp, stampval);
		orders.push_back(stampval);

		// erase previous messages
		inputs[IDPORT_STAMPS]->setFront(num + minDiff);
	}
#ifdef DEBUG
	std::cout << objectID()<<": sending "<<orders.size()<<" order(s)."<<std::endl;
#endif
	if (orders.size() > 0)
		last_scratch_stamp = orders[orders.size() - 1];
	else if (!inputs[IDPORT_STAMPS]->empty())
	{
		int num0 = inputs[IDPORT_STAMPS]->frontNum();
		int num = inputs[IDPORT_STAMPS]->backNum();
		if (num - num0 > maxDiff - minDiff)
			num = num0 + maxDiff - minDiff;

		// find the last valid message within the given interval
		while (num > num0 && !inputs[IDPORT_STAMPS]->get(num).valid())
			--num;
		if (inputs[IDPORT_STAMPS]->get(num).valid())
		{
			int stampval;
			inputs[IDPORT_STAMPS]->get(num).stamps.read(*stamp, stampval);

			if (last_scratch_stamp + bufferSize < stampval)
			{
				sendscratch = true;
				last_scratch_stamp = stampval;
				inputs[IDPORT_STAMPS]->setFront(num);
			}
		}
	}
	for (unsigned int i = 0; i < orders.size(); i++)
	{
		int stampval = orders[i];
		MessageWrite order;
		order.stamps.write(outputs[IDPORT_STAMPS]->stamps.it, (stampval + 10));
		traceout.write(stampval + 10);
		outputs[IDPORT_STAMPS]->put(order, dispatcher);
	}
	if (sendscratch)
	{
		MessageWrite order;
		order.stamps.write(outputs[IDPORT_STAMPS]->stamps.it,-(last_scratch_stamp + 10));
		outputs[IDPORT_STAMPS]->put(order, dispatcher);
	}
}

flowvr::plugd::GenClass<GreedySynchronizor> GreedySynchronizorClass(
		"flowvr.plugins.GreedySynchronizor", // name
		"", // description
		&flowvr::plugins::SynchronizorClass);

Class* GreedySynchronizor::getClass() const
{
	return &GreedySynchronizorClass;
}

} // namespace plugins

} // namespace flowvr
