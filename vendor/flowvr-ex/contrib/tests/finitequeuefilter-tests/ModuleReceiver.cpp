/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2007/2010 by                                      *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*******************************************************************
*                                                                 *
*  Contact :                                                      *
*                                                                 *
 ******************************************************************/

#include <flowvr/module.h>
#include <flowvr/utils/cmdline.h>
#include <iostream>


int main(int argc, char **argv)
{
	// command line parsing stuff from ftl
	flowvr::utils::CmdLine line;
	flowvr::utils::Option<bool> bBlockingIn("blocking-in", 'b',
			                      "switch blocking state (true: blocking, false: non blocking); \
			                       default: true (blocking)", false);

	flowvr::utils::Option<int> nRequest("request", 'r', "number of messages to request on each trigger", 1, true);

	bool bError=false;
	if(line.parse(argc,argv, &bError) == false)
	{
		std::cout << line.help() << std::endl;
		return 0;
	}

	if(bError)
	{
		std::cerr << line.help() << std::endl;
		return -1;
	}

	// output the parameters given
	std::cout << line.getValueString() << std::endl;

	if(nRequest.value() == 0)
	{
		std::cerr << "Requested 0 packets on trigger." << std::endl;
		return -1;
	}


	// we have to create a trigger stamp for this module to work
	// should be clear from other examples or the flowvr manual
	// how that works and what this is for
	flowvr::StampList triggerStamps;
	flowvr::StampInfo triggerP("trigger",flowvr::TypeInt::create()); // send # of messages to forward
	triggerStamps.add( &triggerP );


	// this is our input port, we set the blocking flag accordingly to the
	// user argument given
	flowvr::InputPort  messages("in", NULL, false, !bBlockingIn.value());

	// we just output the number of messages we want to receive.
	// this is basically an empty message with a trigger stamp
	// associated to it.
	flowvr::OutputPort trigger("trigger", &triggerStamps);

	// we need this for initialization
	std::vector<flowvr::Port*> ports;
	ports.push_back(&messages);
	ports.push_back(&trigger);

	flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);

	// check whether the module was created
	if(!flowvr)
	{
		std::cerr << "could not create flowvr moduleAPI" << std::endl;
		return -1; // no, bail out
	}

	// we recycle the same message, just changing the stamps attached to it (see below)
	flowvr::MessagePut msgTrigger;
	msgTrigger.stamps.write(triggerP,1); // for now: just forward one message when it arrives.
	msgTrigger.data = flowvr->alloc(0);  // empty message, allocate 0 bytes

	// we put this message to the network here, as we know that we are
	// attached in a cycle with the finite queue filter. This is in principle
	// ok, but normally we should not assume any property of the surrounding
	// network.
	flowvr->put(&trigger, msgTrigger); // put to the network

	int nIteration = 0;
	int nNumberOfEmptyPolls = 0;
	int nNumMessages = 0;

	while(flowvr->wait()) // wait for a new message
	{
		flowvr::Message m;
		flowvr->get( &messages, m ); // get it

		if(m.valid()) // in case the user specified the input port to be non-blocking,
			          // invalid message can arrive due to sampling of the ports
					  // in the regulator.
		{
			// valid message
			if( m.data.getSize() == sizeof(int)) // Coarsely check: we expect an int (iteration number of the sender)
			{
				nIteration = *m.data.getRead<int>(); // read as int
				++nNumMessages; // count the number of read messages for the receiver
			}

			// now we have to ask for more messages, we overwrite the message value
			// for that in the stamp, we leave the remainder of the message attached.
			// note that we put this request ONLY when we have received a valid message
			// otherwise, we might spam the finiteQueue trigger port with requests.
			msgTrigger.stamps.write(triggerP,nRequest.value());
			flowvr->put(&trigger, msgTrigger);
		}
		else
		{
			++nNumberOfEmptyPolls; // this was an "empty" or invalid poll,
			                       // for the statistics, we record this
		}
	}

	// output, just for the sake of it ;)
	// the number of messages received:
	std::cout << "Received  [" << nNumMessages << "] messages." << std::endl;


	// in case the user wanted a non-blocking port, it might be interesting
	// to see how many empty polls we had.
	if(!bBlockingIn.value())
		std::cout << "Processed [" << nNumberOfEmptyPolls << "] empty polls." << std::endl;

	return 0;
}
