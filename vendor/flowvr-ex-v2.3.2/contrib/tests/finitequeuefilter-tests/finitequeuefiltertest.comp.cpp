/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Application Library                         *
 *                                                                 *
 *-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
 * INRIA                                                           *
 * ALL RIGHTS RESERVED.	                                          *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Thomas Arcila,                                               *
 *    Jean-Denis Lesage.                                           *
 *    Clement Menier,                                              *
 *    Bruno Raffin                                                 *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 *  Contact :                              *
 *                                                                 *
 ******************************************************************/

#include "finitequeuefiltertest.comp.h"

// needed for the GENCLASS macro
#include "flowvr/app/core/genclass.h"

#include <flowvr/app/components/filterfinitequeue.comp.h>
#include <flowvr/app/components/filterfrequency.comp.h>
#include <flowvr/app/components/connection.comp.h>

// example specific component includes
#include "metamodulereceiver.comp.h"
#include "metamodulesender.comp.h"


using namespace flowvr::app;

namespace FiniteFilterQueueTest
{
	// Required to enable the dynamic loading of this component
	GENCLASS(FiniteFilterQueueTest)

	FiniteFilterQueueTest::FiniteFilterQueueTest(const std::string &id_)
	: Composite(id_)
	{
		setInfo("Create a sender/receiver scenario under different conditions");
	}


	void FiniteFilterQueueTest::execute()
	{
		// ####################################################################
		// COMPONENTS
		// ####################################################################

		// in this example we have only two parties, running independently
		// namely: the 'sender' and the 'receiver'. The messages passed between them
		// do not have significant content, i.e. structure, it is just to show the
		// flow of messages on the network and the behavior of the filter
		MetaModuleSender   *metamodulesender   = addObject<MetaModuleSender>("sender");
		MetaModuleReceiver *metamodulereceiver = addObject<MetaModuleReceiver>("receiver");


		// the finite queue filter, we will plug this between the sender and
		// the receiver later on
		flowvr::app::FilterFiniteQueue *pfq = addObject<flowvr::app::FilterFiniteQueue> ("finite-queue");

		// in this example, the sender and receiver are modeled as totally independent.
		// for that, we use frequency filters (part of flowvr base) to activate
		// them each with a different frequency (user/parameter defined)
		// we prefix them with the tag "drive-*" to indicate that activation-characteristic
		flowvr::app::FilterFrequency *driveSender = addObject<flowvr::app::FilterFrequency> ("drive-sender");
		flowvr::app::FilterFrequency *driveRecver = addObject<flowvr::app::FilterFrequency> ("drive-receiver");


		// ####################################################################
		// CONNECTIONS
		// ####################################################################
		Connection *cons2fq = addObject<Connection> ("cS+fq");
		Connection *fq2Rcv  = addObject<Connection> ("fq+Rcv");
		ConnectionStamps *Rcv2Fq = addObject<ConnectionStamps> ("Rcv+fq");
		ConnectionStamps *drive2Sender   = addObject<ConnectionStamps> ("drive2Sender");
		ConnectionStamps *drive2Receiver = addObject<ConnectionStamps> ("driver2Receiver");


		// ####################################################################
		// LINKS
		// ####################################################################

		// ok, lets link the "application" first (sender over finiteQueue to
		// receiver, and the trigger queue from the receiver to the finiteQueue)

		// sender.out -> finiteQueue.in (using cS+fq)
		link(metamodulesender->getPort("out"), cons2fq->getPort("in"));
		link(cons2fq->getPort("out"), pfq->getPort("in"));


		// note: we create a cycle here between the finiteQueue and the receiving module
		// we eliminate that in the module by putting a token into the network
		// before we start waiting. Remember: THIS IS JUST AN EXAMPLE

		// finiteQueue.out -> receiver.in (using fq+Rcv)
		link(pfq->getPort("out"), fq2Rcv->getPort("in"));
		link(fq2Rcv->getPort("out"), metamodulereceiver->getPort("in"));

		// receiver.trigger -> finiteQueue.trigger (using Rcv+fq)
		link(metamodulereceiver->getPort("trigger"), Rcv2Fq->getPort("in"));
		link(Rcv2Fq->getPort("out"), pfq->getPort("trigger"));


		// now some part of the activation pattern
		// driverSender.out -> sender.beginIt
		link(driveSender->getPort("out"), drive2Sender->getPort("in"));
		link(drive2Sender->getPort("out"), metamodulesender->getPort("beginIt"));

		// driverReceiver.out -> receiver.beginIt
		link(driveRecver->getPort("out"), drive2Receiver->getPort("in"));
		link(drive2Receiver->getPort("out"), metamodulereceiver->getPort("beginIt"));
	}

	// factory method for this component
	flowvr::app::Component* FiniteFilterQueueTest::create() const
	{
		return new FiniteFilterQueueTest(this->getId());
	}

}
