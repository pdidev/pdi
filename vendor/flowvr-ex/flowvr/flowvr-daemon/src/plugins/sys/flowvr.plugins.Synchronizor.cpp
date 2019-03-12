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
* File: src/plugins/flowvr.plugins.Synchronizor.cpp               *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/synchronizor.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include <iostream>
#include <sstream>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::xml;

/// Constructor.
Synchronizor::Synchronizor(const std::string &objID)
  : BaseObject(objID), advance(1), nbOrder(0)
{
}

Synchronizor::~Synchronizor()
{
}

flowvr::plugd::Result Synchronizor::init(DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = BaseObject::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  DOMNodeList* nodeadvance = xmlRoot->getElementsByTagName("advance");
  if (nodeadvance!=NULL && nodeadvance->item(0)!=NULL)
  {
    std::string val = nodeadvance->item(0)->getTextContent();
    advance = atoi(val.c_str());
    if (flowvr::daemon::verboseLevel>=1)
      std::cout << objectID()<<": advance="<<advance<<std::endl;
  }
  else
    advance = 1; // we need to send at least one data before waiting for the end of the first iteration.
  if (nodeadvance!=NULL) delete nodeadvance;

  return result;
}

void Synchronizor::doStart(plugd::Dispatcher* dispatcher)
{
  BaseObject::doStart(dispatcher);
  {
    ipc::ScopedMTLock locker(messageQueueLock(),"Synchronizor.doStart");
    nbOrder=0;
  }
}


flowvr::plugd::AbsClass<Synchronizor> SynchronizorClass("flowvr.plugins.Synchronizor", // name
							"" // description
							);

plugd::Class* Synchronizor::getClass() const
{
  return &SynchronizorClass;
}

} // namespace plugins

} // namespace flowvr
