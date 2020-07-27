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
* File: src/plugins/flowvr.plugins.MinIt.cpp                      *
*                                                                 *
* Contacts:                                                       *
*  01/16/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
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

/// MinIt.
///
/// Send the minimum it of the last message on N inputs.
/// Useful to ensure outputs buffers of a module are not full

class MinIt : public Filter
{
 public:

  MinIt(const std::string &objID);

  virtual ~MinIt();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);

  /// Create an ActionHandler for batch mode action execution.
  virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot);

protected:

  //StampList stamps;
  int numout;
  //string sourceout;

  int* maxIt; // array of max it of each input
  int minIt; // current min it

  virtual void doStart(plugd::Dispatcher* dispatcher);
  virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
};

using namespace flowvr::xml;

/// Constructor.
MinIt::MinIt(const std::string &objID)
  : Filter(objID), numout(0), maxIt(NULL), minIt(0)
{
}

MinIt::~MinIt()
{
  if (maxIt!=NULL) delete[] maxIt;
}

flowvr::plugd::Result MinIt::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  initInputs(0);
  minIt=0;
  numout = 0;
  //sourceout = objectID()+":out";


  //one outputmessage queue
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::STAMPS;
  outputs[0]->newStampSpecification(dispatcher,0);


  return result;
}

/// Create an ActionHandler for batch mode action execution.
flowvr::plugd::ActionHandler* MinIt::createAction(DOMElement* xmlRoot)
{
  std::string portName = xmlRoot->getTextContent();
  int i;
  for (i=0;i<nbInputs;i++)
  {
    if (inputs[i]->getName() == portName)
      return inputs[i]->createAction(xmlRoot);
  }
  //return NULL;
  // create a new port with this name
  i = addInputs(1);
  inputs[i]->setName(portName);

  // update maxIt
  int* last = maxIt;
  maxIt = new int[nbInputs];
  if (last!=NULL)
  {
    for (int k=0;k<i;k++) maxIt[k]=last[k];
    delete[] last;
  }
  maxIt[i]=0;

  return inputs[i]->createAction(xmlRoot);
}

void MinIt::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{
#ifdef DEBUG
  std::cout << name()<<": new input"<<mqid<<" "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif
  int it;
  if (msg.stamps.read(inputs[mqid]->getStampList().it,it))
  {
    bool send = false;
    {
      //ipc::ScopedMTLock locker(globalLock,"newMessageNotification");
      int prevIt = maxIt[mqid];
      if (it>maxIt[mqid]) maxIt[mqid]=it;
      if (prevIt==minIt)
      {
	// minIt might have changed
	minIt = maxIt[0];
	for (int i=1;i<nbInputs;i++)
	  if (maxIt[i]<minIt) minIt=maxIt[i];
	if (minIt>prevIt && isStarted()) // send a new message
	  send = true;
      }
    }
    if (send)
      sendPendingMessages(dispatcher);
  }
  int num;
  msg.stamps.read(inputs[mqid]->getStampList().num,num);
  inputs[mqid]->setFront(num+1);
}

void MinIt::doStart(plugd::Dispatcher* dispatcher)
{
  Filter::doStart(dispatcher);
  { // out
    //StampListSpecification stampsSpec;
    //MessageWrite msgspec;
    //msgspec.stamps.write(stampsSpec.source,sourceout);
    //msgspec.stamps.write(stampsSpec.num,-1);
    //msgspec.stamps.write(stampsSpec.it,0);
    //xml::DOMNode* xmlspec = stamps.generateXML();
    //msgspec.stamps.write(stampsSpec.spec,xml::DOMWriter::toString(xmlspec));
    //delete xmlspec;
    //dispatcher->process(msgspec);
  }
  sendPendingMessages(dispatcher);
}

void MinIt::sendPendingMessages(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION
  MessageWrite m;
  {
    //ipc::ScopedMTLock locker(globalLock,"sendPendingMessages");
    // set stamps and send the message
#ifdef DEBUG
    std::cout<<name()<<": sending message "<<numout<<" it="<<minIt<<std::endl;
#endif
    //m.stamps.write(stamps.source,sourceout);
    //m.stamps.write(stamps.num,numout);
    m.stamps.write(outputs[0]->stamps.it,minIt);    
    outputs[0]->put(m,dispatcher,numout);
  } 
  {
    //ipc::ScopedMTLock locker(globalLock,"sendPendingMessages");
    ++numout;
  }
  //dispatcher->process(m);
 
}

flowvr::plugd::GenClass<MinIt> MinItClass("flowvr.plugins.MinIt", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* MinIt::getClass() const
{
  return &MinItClass;
}

} // namespace plugins

} // namespace flowvr
