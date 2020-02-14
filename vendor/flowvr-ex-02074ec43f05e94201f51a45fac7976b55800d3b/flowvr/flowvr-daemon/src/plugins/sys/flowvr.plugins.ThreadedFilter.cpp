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
* File: src/plugins/flowvr.plugins.ThreadedFilter.cpp             *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/threadedfilter.h"
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
ThreadedFilter::ThreadedFilter(const std::string &objID)
  : Filter(objID)
{
}

ThreadedFilter::~ThreadedFilter()
{
}


void ThreadedFilter::wait(int mqid)
{
  while(!inputs[mqid]->frontMsg().valid()) 
    inputs[mqid]->signalStart.wait(messageQueueLock());
}


void ThreadedFilter::wait(int mqid, int msgnum) 
{ 
  while(!inputs[mqid]->get(msgnum).valid()) 
    inputs[mqid]->signalStart.wait(messageQueueLock());
}


void ThreadedFilter::wait_all()
{
  for (int i=0;i<nbInputs;i++)
    {
      wait(i);
    }
}


void ThreadedFilter::wait_any()
{
  global_signal.wait(messageQueueLock());
}
  
void ThreadedFilter::newMessageNotification(int mqid, int msgnum, const Message& msg, plugd::Dispatcher* dispatcher)
    {
      inputs[mqid]->signalStart.notify();
      global_signal.notify();
    }



flowvr::plugd::AbsClass<ThreadedFilter> ThreadedFilterClass("flowvr.plugins.ThreadedFilter", // name
							"" // description
							);

plugd::Class* ThreadedFilter::getClass() const
{
  return &ThreadedFilterClass;
}

} // namespace plugins

} // namespace flowvr
