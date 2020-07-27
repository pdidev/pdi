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
 * File: src/plugins/flowvr.plugins.FrequencyFilter.cpp            *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/threadedfilter.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <iostream>
#include <sstream>
#include <unistd.h>
#include <time.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// FrequencyFilter.
///
/// This is a filter which generates messages at the indicated frequency.
/// Remark : it only sets the average frequency of activation.
///	
/// It is possible if one iteration takes more than 1/freq second
/// than next iterations takes less than 1/freq second because 
/// several activation signals have accumulated.
///
/// Nevertheless if this behavior is needed, you can use this filter,
/// else if you want to set the max frequency and to avoid
/// this behavior, you should use the synchronizer MaxFrequencySynchronizor.
/// 
/// <b>Init parameters:</b>
/// -  \<freq\>frequency in Hertz (float) (from 0.001 to 1000000)\</nb\>
/// -  \<delay\>delay in seconds before starting (integer) (from 0 to 1000)\</nb\>
///	
/// <b>Input ports:</b>
/// -  <b>NONE</b> 
///
/// <b>Output Ports:</b>
/// - <b>out</b>



class FrequencyFilter: public ThreadedFilter
{
public:

	FrequencyFilter(const std::string &objID);

	virtual ~FrequencyFilter();

	virtual Class* getClass() const;

	virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot,
			flowvr::plugd::Dispatcher* dispatcher);

	virtual void newMessageNotification(int mqid, int msgnum,
			const Message& msg, Dispatcher* dispatcher);

	/// Main thread function.
	virtual int run();

	float freqHz;

	bool running;

	int sleepTime; // time in sec before running

	ipc::MTSignal signalStart;

	flowvr::plugd::Dispatcher* threadDispatcher;

	virtual flowvr::plugd::Result close(flowvr::xml::DOMElement* xmlRoot,
			flowvr::plugd::Dispatcher* dispatcher);

protected:
	virtual void doStart(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
FrequencyFilter::FrequencyFilter(const std::string &objID) :
	ThreadedFilter(objID), freqHz(1.0f)
{
	sleepTime = 0; // default : start immediately
	threadDispatcher = NULL;
}

FrequencyFilter::~FrequencyFilter()
{
}

void FrequencyFilter::newMessageNotification(int mqid, int msgnum,
		const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG 
	std::cout<< name() <<" : ERROR : call to newMessageNotification on a filter with no input ! " <<std::endl;
#endif
}

flowvr::plugd::Result FrequencyFilter::init(flowvr::xml::DOMElement* xmlRoot,
		flowvr::plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
	if (result.error())
		return result;

	threadDispatcher = dispatcher->threadCopy();

	// freq parameter
	{
		freqHz = 1.0f;
		xml::DOMNodeList* lfreq = xmlRoot->getElementsByTagName("freq");
		if (lfreq->getLength() < 1)
			return Result(flowvr::plugd::Result::ERROR, "No freq parameter");
		std::string fr = lfreq->item(0)->getTextContent();
		freqHz = atof(fr.c_str());
		delete lfreq;
		if (freqHz < 0.001f || freqHz > 100000000.0f)
		{
			// incorrect value for frequency in hertz
			return Result(flowvr::plugd::Result::ERROR,
					"Incorrect frequency parameter");
		}
	}
	// delay parameter (optional)
	{
		sleepTime = 0;
		xml::DOMNodeList* lsleep = xmlRoot->getElementsByTagName("delay");
		if (lsleep->getLength() == 1)
		{
			std::string sl = lsleep->item(0)->getTextContent();
			sleepTime = atoi(sl.c_str());
			if (sleepTime < 0 || sleepTime > 1000)
			{
				// incorrect value for delay in second(s)
				return Result(flowvr::plugd::Result::ERROR,
						"Incorrect delay parameter (correct value from 0 to 1000)");
			}
		}
		delete lsleep;
	}
	initInputs(0); // no input here

	//only one outputmessagequeue for this filter
	initOutputs(1);
	outputs[0]->setName("out");
	outputs[0]->msgtype = Message::FULL;

	//give the Stamplist to the outputmessage queue
	outputs[0]->newStampSpecification(dispatcher);

	running = true;

	start();

	return result;
}

void FrequencyFilter::doStart(plugd::Dispatcher* dispatcher)
{
	Filter::doStart(dispatcher);
	ipc::ScopedMTLock locker(messageQueueLock(), "FrequencyFilter.doStart");

	signalStart.notify();
}

/// Main thread function.
int FrequencyFilter::run()
{
	while (running) // running is a variable from thread scope
	{
		if (!isStarted()) // this is a variable from daemon scope,
			          // indicating that this filter was started
		{
			ipc::ScopedMTLock locker(messageQueueLock(), "FrequencyFilter.run");

			signalStart.wait(messageQueueLock()); // can this be a race?
			if (sleepTime > 0)
				sleep(sleepTime); // looks like introduced for a side-effect

		}
		else
		{
			// we are putting a FULL message, as this leaves the option
			// to use this filter as a trigger filter for full-message type
			// ports (not only for stamped inputs...) and mask out the message body
			// using a connection stamps.
			// but we do not allocate memory for that, at least no more than the
			// information needed to record in the shm manager
			MessageWrite msg;
			msg.data = alloc(0);

			outputs[0]->put(msg, threadDispatcher);

			// waiting :
			timespec treq;
			double period = (1.0 / freqHz);
			treq.tv_sec = (time_t) (period);
			treq.tv_nsec = (long) ((period - treq.tv_sec) * 1000000000);

			// waste some time
			nanosleep(&treq, NULL);
		}

	} // loop end

	threadDispatcher->close();

	return 0;
}

flowvr::plugd::Result FrequencyFilter::close(flowvr::xml::DOMElement* xmlRoot,
		flowvr::plugd::Dispatcher* dispatcher)
{
	running = false;
	return BaseObject::close(xmlRoot, dispatcher);
}

flowvr::plugd::GenClass<FrequencyFilter> FrequencyFilterClass(
		"flowvr.plugins.FrequencyFilter", // name
		"", // description
		&flowvr::plugins::FilterClass);

Class* FrequencyFilter::getClass() const
{
	return &FrequencyFilterClass;
}

} // namespace plugins

} // namespace flowvr
