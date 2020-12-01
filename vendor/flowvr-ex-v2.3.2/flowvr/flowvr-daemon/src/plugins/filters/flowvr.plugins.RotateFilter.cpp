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
* File: src/plugins/flowvr.plugins.RotateFilter.cpp               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;


/// RotateFilter. Transmit messages on each output port at turn.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of output ports \</nb\>
///
/// <b>Input ports:</b>
/// -  <b>in</b>
///
/// <b>Output Ports:</b>
/// - <b>out0</b> ... <b>out(nb-1)</b>


class RotateFilter : public Filter
{
 public:

  RotateFilter(const std::string &objID);

  virtual ~RotateFilter();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  int nboutputs;
  std::vector<int> numout;
  int numouttouse;

protected:
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);
  
};

using namespace flowvr::xml;

/// Constructor.
RotateFilter::RotateFilter(const std::string &objID)
  : Filter(objID)
{
   numouttouse=0;
}

RotateFilter::~RotateFilter()
{
}

flowvr::plugd::Result RotateFilter::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;
  
  nboutputs = 1;
  // nb parameter reading
  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string strnb = lnb->item(0)->getTextContent();
  nboutputs = atoi(strnb.c_str());
  delete lnb;
  if  ( nboutputs < 1 )
  {
    // incorrect value for number of outputs
    return Result(flowvr::plugd::Result::ERROR,"Incorrect nb parameter");
  }
  numout.resize(nboutputs);


  //initialization of the input message queue
  initInputs(1);
  inputs[0]->setName("in");


  //initialization of the OMQ
  initOutputs(numout.size());
  char buf[16];
  for (unsigned int  i=0 ; i < numout.size() ; i++)
    {
      numout[i]=0;
      sprintf(buf,"out%d",i);
      outputs[i]->setName(buf);
      outputs[i]->msgtype = Message::FULL;
    }
  
  return result;
}

void RotateFilter::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  if (mqid == 0)
    std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  sendPendingOrders(dispatcher);
}

void RotateFilter::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == 0)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif
    
    //forwarding stamplist specification to the OMQ
    for (unsigned int i=0 ; i < numout.size() ; i++)
      {
	outputs[i]->stamps=inputs[mqid]->getStampList();
	outputs[i]->newStampSpecification(dispatcher);
      }

    sendPendingOrders(dispatcher);
  }
}


void RotateFilter::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION

  if (!inputs[0]->stampsReceived()) return; // still waiting for stamps specification

  char buf[16];
  int it;
  for (;;)
  {
    Message msg;
    int num;
    {
      if (!inputs[0]->frontMsg().valid())
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting orders"<<std::endl;
#endif
	return;
      }

      msg.stamps.read(inputs[0]->getStampList().num,num);
      
      msg.stamps.read(inputs[0]->getStampList().it,it);
      
      if (!inputs[0]->empty()&& inputs[0]->frontMsg().valid())
        msg = inputs[0]->frontMsg();
      else
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting message "<<it<<std::endl;
#endif
        return; // missing message;
      }
      inputs[0]->eraseFront();
    }
#ifdef DEBUG
    std::cout<<"filter "<<objectID()<<" send message "<<num<<" to output : "<<numouttouse<<std::endl;
#endif
    
    MessagePut newmsg;
    newmsg.stamps.clone(msg.stamps,&inputs[0]->getStampList());
    newmsg.stamps.write(inputs[0]->getStampList().source,objectID()+buf);
    
    newmsg.data=msg.data;

    newmsg.stamps.write(inputs[0]->getStampList().num,numout[numouttouse]);
    outputs[numouttouse]->put(newmsg,dispatcher,numout[numouttouse]);    

    numout[numouttouse]++;
	
    numouttouse =  (numouttouse+1) % nboutputs;
  }
}

flowvr::plugd::GenClass<RotateFilter> RotateFilterClass("flowvr.plugins.RotateFilter", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* RotateFilter::getClass() const
{
  return &RotateFilterClass;
}

} // namespace plugins

} // namespace flowvr
