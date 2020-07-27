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
* File: src/plugins/flowvr.plugins.MaskScatter.cpp                *
*                                                                 *
* Contacts:                                                       *
*  12/02/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// \brief A filter which scatter a message in N messages according to
/// a base element size and a mask stamps dynamically specifying where
/// to send each message.
///
/// This  filter opens  one input  port <b>in</b>  and  several output
/// ports <b>out0,out1,...</b>.  The received messages should contain
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

class MaskScatter : public Filter
{
public:

  int nbPorts;
  int sizeOfElem;
  int maskSize;
  StampInfo* stampmask;
  int* mask;

  enum {
    IDPORT_IN=0,
    NBPORTS
  };

  MaskScatter(std::string objID);

  virtual ~MaskScatter();

  virtual flowvr::plugd::Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);
  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

};

MaskScatter::MaskScatter(std::string objID)
: Filter(objID), nbPorts(0), sizeOfElem(1), maskSize(0), stampmask(NULL), mask(NULL)
{
}

MaskScatter::~MaskScatter()
{
}

flowvr::plugd::Result MaskScatter::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
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
  if (sizeOfElem<=0) sizeOfElem=1;

  maskSize = nbPorts / 32;
  mask = new int[maskSize];

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

void MaskScatter::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input, port id"<<mqid<<", queue size "<<inputs[mqid]->size()<<", message num "<<msgnum<<std::endl;
#endif

  while ( inputs[IDPORT_IN]->frontMsg().valid() )
  {
    const Message& msg = inputs[IDPORT_IN]->frontMsg();

    if (stampmask!=NULL)
    {
      for (int i=0; i < maskSize; i++)
	msg.stamps.read((*stampmask)[i],mask[i]);
    }
    else
      for (int i=0; i < maskSize; i++)
	mask[i]=-1;

    int ndest=0;
    for (int i=0; i < nbPorts; i++)
      if (mask[i>>5]&(1<<(i&31)))
	++ndest;

    if (ndest>0)
    {
      int nbElems = msg.data.getSize()/sizeOfElem;
      int n = 0;
      for(int i = 0; i < nbPorts; i++)
      {
	if (mask[i>>5]&(1<<(i&31)))
	{
	  int p0 = ((nbElems*n)/ndest)*sizeOfElem;
	  int p1;
	  if (n==ndest-1) p1 = msg.data.getSize();
	  else p1 = ((nbElems*(n+1))/ndest)*sizeOfElem;
	  MessagePut newmsg;
	  newmsg.data = Buffer(msg.data, p0, p1-p0);
	  newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());

	  outputs[i]->put(newmsg, dispatcher);
#ifdef DEBUG
	  std::cout << "MaskScatter sending message " << i << " size=" << newmsg.data.getSize()
		    << " on port out" << i << std::endl;
#endif
	  ++n;
	}
      }
    }
    inputs[IDPORT_IN]->eraseFront();
  }
}

void MaskScatter::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
    stampmask = inputs[IDPORT_IN]->getStampList()["mask"];
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

flowvr::plugd::GenClass<MaskScatter> MaskScatterClass("flowvr.plugins.MaskScatter", // name
					      "" // description
					      );

Class* MaskScatter::getClass() const
{
  return &MaskScatterClass;
}

} // namespace plugd

} // namespace flowvr
