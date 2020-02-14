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
* File: ./src/plugins/filters/flowvr.plugins.UnrotateFilter.cpp   *
*                                                                 *
* Contacts:                                                       *
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

/// RotateFilter. Reorders messages dispatched by a RotateFilter.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of input ports \</nb\>
///
/// <b>Input ports:</b>
/// -  <b>in0</b>...<b>in(nb-1)</b>
///
/// <b>Output Ports:</b>
/// - <b>out</b> </b>

class UnrotateFilter : public Filter
{
 public:

  UnrotateFilter(const std::string & objID);

  virtual ~UnrotateFilter();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  int nbinputs;
  int numout;
  int noinputwaited;

protected:
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

};

using namespace flowvr::xml;

/// Constructor.
UnrotateFilter::UnrotateFilter(const std::string &objID)
  : Filter(objID), numout(0)
{
}

UnrotateFilter::~UnrotateFilter()
{
}

flowvr::plugd::Result UnrotateFilter::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;
  
  nbinputs = 1;
  noinputwaited = 0;
  // nb parameter reading
  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string strnb = lnb->item(0)->getTextContent();
  nbinputs = atoi(strnb.c_str());
  delete lnb;
  if  ( nbinputs < 1 )
  {
    // incorrect value for number of outputs
    return Result(flowvr::plugd::Result::ERROR,"Incorrect nb parameter");
  }


  //initialization of the output message queue
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::FULL;

  //initialization of the IMQ
  initInputs(nbinputs);
  char buf[16];
  for (int i=0 ; i < nbinputs ; i++)
    {
      sprintf(buf,"in%d",i);
      inputs[i]->setName(buf);
    }
  
  return result;
}

void UnrotateFilter::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
    std::cout << objectID()<<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  sendPendingOrders(dispatcher);
}

void UnrotateFilter::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == 0)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif
    
    //forwarding stamplist specification to the OMQ
	outputs[0]->stamps=inputs[mqid]->getStampList();
	outputs[0]->newStampSpecification(dispatcher);

    sendPendingOrders(dispatcher);
  }
}


void UnrotateFilter::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
 
  //noinputwaited=0; // number of the next input to be read

  if (!inputs[noinputwaited]->stampsReceived()) return; // still waiting for stamps specification

  int it = 0;
  for (;;)
  {
    Message msg;
    int num;
    {
      if (!inputs[noinputwaited]->frontMsg().valid())
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting orders"<<std::endl;
#endif
	return;
      }
      
      msg.stamps.read(inputs[noinputwaited]->getStampList().num,num);
      
      msg.stamps.read(inputs[noinputwaited]->getStampList().it,it);
      
      if (!inputs[noinputwaited]->empty()&& inputs[noinputwaited]->frontMsg().valid())
        msg = inputs[noinputwaited]->frontMsg();
      else
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting message "<<it<<std::endl;
#endif
        return; // missing message;
      }
      inputs[noinputwaited]->eraseFront();

    }
#ifdef DEBUG
    std::cout<<objectID()<<": sending message "<<num<<std::endl;
#endif
    
    MessagePut newmsg;
    newmsg.stamps.clone(msg.stamps,&inputs[noinputwaited]->getStampList());
    newmsg.stamps.write(inputs[noinputwaited]->getStampList().source,objectID());
    newmsg.data=msg.data;
    newmsg.stamps.write(inputs[noinputwaited]->getStampList().num,numout);
    outputs[0]->put(newmsg,dispatcher,numout);
    numout++;
	
    noinputwaited =  (noinputwaited+1) % nbinputs;
  }
}

flowvr::plugd::GenClass<UnrotateFilter> UnrotateFilterClass("flowvr.plugins.UnrotateFilter", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* UnrotateFilter::getClass() const
{
  return &UnrotateFilterClass;
}

} // namespace plugins

} // namespace flowvr
