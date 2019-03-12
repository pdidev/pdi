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
*  Original Contributors:  
*    Helene Coullon,                                              *
*    Sebastien Limet,                                             *
*    Sophie Robert,                                               *
*    Emmanuel Melin,                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/plugins/flowvr.plugins.FilterRank.cpp               *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Helene Coullon <helene.coullon@univ-orleans.fr>     *
*                                                                 *
******************************************************************/

#include "flowvr/daemon.h"
#include "flowvr/plugins/filter.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <iostream>
#include <sstream>
#include <unistd.h>
#include<string>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// \brief Sends messages 0..nb-1 to out0..out(nb-1). Then transmits
/// messages to the output port selected by the 'StampRank' stamp on
/// the 'order' port.  
///
/// This  filter opens  two input  ports <b>in</b> and <b>order</b> and  several output
/// ports <b>out0,out1,...</b>.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of outputs to produce\</nb\>
///
/// <b>Input ports:</b>
/// - <b>order</b> 
/// - <b>in</b>
///
/// <b>Output Ports:</b>
/// -  <b>out<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1

class FilterRank : public Filter
{
public:
	int nbPorts;
  	int number;

	enum {
		IDPORT_IN=0,
		IDPORT_ORDER,
		NBPORTS
	};

	FilterRank(std::string objID);
	virtual ~FilterRank();

	virtual flowvr::plugd::Class* getClass();
	virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);
	virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);
	virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

protected:
	virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

};

FilterRank::FilterRank(std::string objID)
: Filter(objID), nbPorts(0),number(0)
{
}

FilterRank::~FilterRank()
{
}

flowvr::plugd::Result FilterRank::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Filter::init(xmlRoot,dispatcher);
	if (result.error()) return result;

	xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
	if (lnb->getLength()<1) return flowvr::plugd::Result(flowvr::plugd::Result::ERROR,"No nb parameter");
	std::string nb = lnb->item(0)->getTextContent();
	nbPorts = atoi(nb.c_str());
	delete lnb;

	initInputs(2);
	inputs[IDPORT_IN]->setName("in");
	inputs[IDPORT_ORDER]->setName("order");

	initOutputs(nbPorts);
  
	char buf[16];
	for(int i = 0; i < nbPorts; i++)
	{
		sprintf(buf,"out%d",i);
		outputs[i]->setName(buf);
		outputs[i]->msgtype=Message::FULL;
	}
	return result;
}

void FilterRank::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
	if (mqid == IDPORT_IN)
		std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
	else
		std::cout << objectID()<<": new order "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
	sendPendingOrders(dispatcher);
}

void FilterRank::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
	if (mqid == IDPORT_IN)
	{ // forward specification to out port
#ifdef DEBUG
		std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif

		for (unsigned int i=0 ; i < nbPorts ; i++)
		{
			outputs[i]->stamps=inputs[mqid]->getStampList();
			outputs[i]->newStampSpecification(dispatcher);
		}

		sendPendingOrders(dispatcher);
	}
}

void FilterRank::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
	
	if (!inputs[IDPORT_IN]->stampsReceived()) return; // still waiting for stamps specification

	//int it,itin;
	for (;;)
	{
		Message msg;
		int rank,num=0;
		std::string srcStr="";

		if (!inputs[IDPORT_IN]->empty() && inputs[IDPORT_IN]->frontMsg().valid())
		{
			msg = inputs[IDPORT_IN]->frontMsg();
		}
		else
		{
#ifdef DEBUG
			std::cout<<objectID()<<": waiting message "<<std::endl;
#endif
			return; // missing message;
		}

		msg.stamps.read(inputs[IDPORT_IN]->getStampList().num,num);

		if(number<nbPorts && number>=0)
		{
			// put incoming message on port number
			MessagePut newmsg;
			newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());
			newmsg.data=msg.data;

			outputs[number]->put(newmsg,dispatcher);
	
 			number++;
			inputs[IDPORT_IN]->eraseFront();
		}
		else
		{
        		//give a message to the output port corresponding to the rank stamp
        		if (!inputs[IDPORT_ORDER]->frontMsg().valid())
        		{
#ifdef DEBUG
				std::cout<<objectID()<<": waiting orders"<<std::endl;
#endif
	  			return;
        		}

        		msg = inputs[IDPORT_ORDER]->frontMsg();
			msg.stamps.read(*inputs[IDPORT_ORDER]->getStampList()["StampRank"],rank);

        		msg = inputs[IDPORT_IN]->frontMsg();
#ifdef DEBUG
        		std::cout<<objectID()<<": sending message "<<number<<" to proc "<<rank<<std::endl;
#endif
        
			MessagePut newmsg;
			newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());
			newmsg.data=msg.data;

			outputs[rank]->put(newmsg,dispatcher);

			inputs[IDPORT_IN]->eraseFront();
			inputs[IDPORT_ORDER]->eraseFront();

			number++;
		}
	}
}

flowvr::plugd::GenClass<FilterRank> FilterRankClass("flowvr.plugins.FilterRank","");

Class* FilterRank::getClass()
{
	return &FilterRankClass;
}

} // namespace plugd

} // namespace flowvr
