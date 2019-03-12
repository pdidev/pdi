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

int main(int argc, char **argv)
{
	// this is a very simple sender module, it just outputs an empty
	// packet of size int to the network when it is told to do so

	flowvr::OutputPort messages("out");
	std::vector<flowvr::Port*> ports;
	ports.push_back(&messages);

	flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);

	if(!flowvr)
	{
		std::cerr << "Could not create flowvr moduleAPI" << std::endl;
		return -1;
	}


	int nIteration = 0; // we start with iteration 0

	// wait for activation time
	while(flowvr->wait())
	{
		// ok
		flowvr::MessageWrite m;
		m.data = flowvr->alloc(sizeof(int)); // allocate buffer
		*m.data.getWrite<int>() = nIteration++; // write current iteration number
		flowvr->put( &messages, m ); // and put...
	}

	return 0;
}
