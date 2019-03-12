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
#include "flowvr/plugins/threadedsynchronizor.h"
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
ThreadedSynchronizor::ThreadedSynchronizor(const std::string &objID)
  : Synchronizor(objID)
{
}

ThreadedSynchronizor::~ThreadedSynchronizor()
{
}


void ThreadedSynchronizor::wait(int mqid)
{
  while(!inputs[mqid]->frontMsg().valid()) 
    inputs[mqid]->signalStart.wait(messageQueueLock());
}


void ThreadedSynchronizor::wait(int mqid, int msgnum) 
{ 
  while(!inputs[mqid]->get(msgnum).valid()) 
    inputs[mqid]->signalStart.wait(messageQueueLock());
}


void ThreadedSynchronizor::wait_all()
{
  for (int i=0;i<nbInputs;i++)
    {
      wait(i);
    }
}


void ThreadedSynchronizor::wait_any()
{
  global_signal.wait(messageQueueLock());
}
  
void ThreadedSynchronizor::newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher)
    {
      inputs[mqid]->signalStart.notify();
      global_signal.notify();
    }

flowvr::plugd::AbsClass<ThreadedSynchronizor> ThreadedSynchronizorClass("flowvr.plugins.ThreadedSynchronizor", // name
							"" // description
							);

plugd::Class* ThreadedSynchronizor::getClass() const
{
  return &ThreadedSynchronizorClass;
}

} // namespace plugins

} // namespace flowvr
