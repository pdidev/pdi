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
* File: src/plugins/flowvr.plugins.MaxFrequencySynchronizor.cpp   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/threadedsynchronizor.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include <fstream>
#include <iostream>
#include <sstream>
#include <unistd.h>
#include <sys/time.h>
#include <stdio.h>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// Max Frequency Synchronizor.
///
/// \brief This synchronizor enables to set the max frequency
/// of module activation (via the module port beginIt).
///
/// When the synchronizor is launched, it sends a first message.
/// Then, the synchronizer waits for endIt signals to send more messages.
/// After, it sends a new message when it receives
/// an endIt signal, and  it waits the necessary time to
/// respect the frequency indicated.
/// The stamps'it is the value read in the endIt signal.
///
/// <b>Init parameters:</b>
/// -  \<freq\>frequency (from 1 to 1000000 Hertz)\</nb\> Default Value = 1000000
/// -  \<alpha\>alpha: low  threshold for QoS
/// -  \<beta\>beta: high treshold for QoS 
///
/// <b>Input ports:</b>
/// -  <b>endIt</b>
/// -  <b>freq: receives a new max freq (float from 1 to 1000000 Hertz) </b>
///
/// <b>Output Ports:</b>
/// - <b>out</b>        
/// 

class MaxFrequencySynchronizor : public ThreadedSynchronizor
{
 public:

     ipc::MTLock timer;
     ipc::MTSignal signaltimer;

  MaxFrequencySynchronizor(const std::string &objID);

  virtual ~MaxFrequencySynchronizor();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

/// Main thread function. Should be implemented by subclasses.
  virtual int run(); 
 
  enum {
    IDPORT_ENDIT=0,
NBPORTS
  };


  float freqHz;
  bool running;

  struct timeval timeLastSignalSend;
  struct timezone tz;

  StampList stamps;

  flowvr::plugd::Dispatcher* threadDispatcher;

  virtual flowvr::plugd::Result close(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

};

using namespace flowvr::xml;

/// Constructor.
MaxFrequencySynchronizor::MaxFrequencySynchronizor(const std::string &objID)
  : ThreadedSynchronizor(objID), running(false), freqHz(1.0f)
{
  threadDispatcher=NULL;
}

MaxFrequencySynchronizor::~MaxFrequencySynchronizor()
{
}

flowvr::plugd::Result MaxFrequencySynchronizor::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Synchronizor::init(xmlRoot, dispatcher);
  if (result.error()) return result;
  
	threadDispatcher = dispatcher->threadCopy();
	
  freqHz = 1000000.0f;
  // freq parameter reading ...
  xml::DOMNodeList* lfreq = xmlRoot->getElementsByTagName("freq");

  if (lfreq != NULL && lfreq->getLength()>=1)
  {
  	std::string fr = lfreq->item(0)->getTextContent();
	if (!(fr.empty()))
	  freqHz = atof(fr.c_str());
	  delete lfreq;
  }
  if (freqHz > 1000000.0f || freqHz < 1.0f )
  {
    // incorrect value for frequency in hertz
    return Result(flowvr::plugd::Result::ERROR,"Incorrect frequency parameter");
  }


  initInputs(NBPORTS);
  inputs[IDPORT_ENDIT]->setName("endIt");
	
  //only one outputmessagequeue for this filter
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype=Message::STAMPS;

  //give the Stamplist to the outputmessage queue    
  outputs[0]->newStampSpecification(dispatcher);

  running = true;

  start();
  
  return result;
}

int MaxFrequencySynchronizor::run()
{
    struct timeval oldtv;
       // waiting time evaluation 
      long long waitTime = 0;
      long long diffTime = 0;
    long long period = (long long) (1000000.0f / freqHz);
  while ( running )  // stopping condition ? 
    {
      // time of this sending 
      MessageWrite msg;      
      outputs[0]->put(msg,threadDispatcher);
    gettimeofday(&oldtv, NULL);

      ipc::ScopedMTLock locker(messageQueueLock(),"MaxFrequencySynchronizor.run");
      
      // wait a new message on the input message queue "endit"
      wait(IDPORT_ENDIT);
      inputs[IDPORT_ENDIT]->eraseFront();
    
     
      struct timeval tv;
      gettimeofday(&tv, &tz);
    diffTime=(tv.tv_sec-oldtv.tv_sec) * 1000000L + tv.tv_usec-oldtv.tv_usec;  		

      waitTime =  period - diffTime;
 /*     tv.tv_usec += waitTime;
      int sec = tv.tv_usec / 1000000L;
      tv.tv_sec += sec;
      tv.tv_usec -= 1000000L*sec;
      struct timespec ts;
      TIMEVAL_TO_TIMESPEC(&tv, &ts);*/
      if ( waitTime > 0 )
	{	
//#ifdef DEBUG
//	  std::cout<<name()<<": waiting for "<< waitTime <<" usecs."<<std::endl;
//#endif
/*            timer.lock();
            signaltimer.timedwait(timer, &ts);
            timer.unlock();
            struct timeval tv2;
//            std::cout << "tv = " << tv.tv_sec << " / " << tv.tv_usec << " tv2 = " << tv2.tv_sec << " / " << tv2.tv_usec << " Diff before = " << (tv2.tv_sec - oldtv.tv_sec)*1000000L + (tv2.tv_usec - oldtv.tv_usec) << " Period = " << period <<  " waitTime = " << waitTime << std::endl;*/
	  usleep(waitTime);  
	}

	


    }
	
#ifdef DEBUG
  std::cout<< objectID() << ": MaxFrequencySynchronizor thread now stopped. " << std::endl;
#endif
  
  threadDispatcher->close();
	
  return 0;
}

flowvr::plugd::Result MaxFrequencySynchronizor::close(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  ipc::ScopedMTLock locker(messageQueueLock(),"newMessageNotification");
  running = false;
 
  return BaseObject::close(xmlRoot,dispatcher);

}



flowvr::plugd::GenClass<MaxFrequencySynchronizor> MaxFrequencySynchronizorClass("flowvr.plugins.MaxFrequencySynchronizor", // name
								    "", // description
								    &flowvr::plugins::ThreadedSynchronizorClass
								    );

Class* MaxFrequencySynchronizor::getClass() const
{
  return &MaxFrequencySynchronizorClass;
}

} // namespace plugins

} // namespace flowvr
