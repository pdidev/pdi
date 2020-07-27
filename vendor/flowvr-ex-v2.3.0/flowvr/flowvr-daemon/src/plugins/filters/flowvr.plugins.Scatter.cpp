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
* File: src/plugins/flowvr.plugins.Scatter.cpp                    *
*                                                                 *
* Contacts:                                                       *
*  02/06/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief Splits incoming messages, and send slices on each output
/// port.
///
/// This  filter opens  one input  port <b>in</b>  and  several output
/// ports <b>out0,out1,...</b>.  The received messages should contains
/// an  array of  elements whose  size is  defined  at initialization.
/// When a new  message is available a message is  sent on each output
/// port containing a part of the data of the input message.
///
/// <b>Init parameters:</b>
/// -  \<nb\>number of outputs to produce\</nb\>
/// -  \<elementsize\>size of the elements in the data\</elementsize\>
///
/// <b>Input ports:</b>
/// - <b>in</b>
///
/// <b>Output Ports:</b>
/// -  <b>out<i>#</i></b> with <i>#</i> from 0 to <i>nb</i>-1

class Scatter : public Filter
{
public:

  int nbPorts;
  int sizeOfElem;

  enum {
    IDPORT_IN=0,
    NBPORTS
  };

  Scatter(const std::string &objID);

  virtual ~Scatter();

  virtual flowvr::plugd::Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

};

Scatter::Scatter(const std::string &objID)
    : Filter(objID), nbPorts(0), sizeOfElem(1)
{
}

Scatter::~Scatter()
{
}

flowvr::plugd::Result Scatter::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot,dispatcher);
  if (result.error()) return result;

  xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
  if (lnb->getLength()<1) return flowvr::plugd::Result(flowvr::plugd::Result::ERROR,"No nb parameter");
  std::string nb = lnb->item(0)->getTextContent();
  nbPorts = atoi(nb.c_str());
  delete lnb;

  xml::DOMNodeList* les = xmlRoot->getElementsByTagName("elementsize");
  if (les->getLength()<1) return flowvr::plugd::Result(flowvr::plugd::Result::ERROR,"No elementsize parameter");
  std::string es = les->item(0)->getTextContent();
  sizeOfElem = atoi(es.c_str());
  delete les;

  if (sizeOfElem <= 0) {
    std::cerr << objectID() << ": wrong size of element = " << sizeOfElem << std::endl;
    return Result(Result::ERROR, "Wrong size of element");
  }

  initInputs(1);
  inputs[IDPORT_IN]->setName("in");

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

void Scatter::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << objectID() <<": new input, port id"<<mqid<<", queue size "<<inputs[mqid]->size()<<", message num "<<msgnum<<std::endl;
#endif

  while ( inputs[IDPORT_IN]->frontMsg().valid() )
  {
    const Message& msg = inputs[IDPORT_IN]->frontMsg();

    int nbElems = msg.data.getSize()/sizeOfElem;
    for(int i = 0; i < nbPorts; i++)
    {
      int p0 = ((nbElems*i)/nbPorts)*sizeOfElem;
      int p1;
      if (i==nbPorts-1) p1 = msg.data.getSize();
      else p1 = ((nbElems*(i+1))/nbPorts)*sizeOfElem;
      MessagePut newmsg;
      newmsg.data = Buffer(msg.data, p0, p1-p0);
      newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());

      outputs[i]->put(newmsg, dispatcher);
#ifdef DEBUG
      std::cout << "Scatter sending message " << i << " size=" << newmsg.data.getSize()
                << " on port out" << i << std::endl;
#endif
    }
    inputs[IDPORT_IN]->eraseFront();
  }
}

void Scatter::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification to "<< nbPorts << " outputs "<<std::endl;
#endif
    for(int i = 0; i < nbPorts; i++)
    {
      outputs[i]->stamps = inputs[IDPORT_IN]->getStampList();
      outputs[i]->newStampSpecification(dispatcher);
    }
  }
}

flowvr::plugd::GenClass<Scatter> ScatterClass("flowvr.plugins.Scatter", // name
					      "" // description
					      );

Class* Scatter::getClass() const
{
  return &ScatterClass;
}

} // namespace plugd

} // namespace flowvr
