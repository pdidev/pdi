/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Daemon and Base Plugins                     *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (EA 4022) ALL RIGHTS RESERVED.                                  *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING-LIB file for further information.                       *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Sebastien Limet                                              *
 *    Sophie Robert                                                *
 * 	  Yoann Kernoa												   *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: src/plugins/flowvr.plugins.VariableFrequencySynchronizor.cpp *
 *                                                                 *
 * Contacts:                                                       *
 *     22/07/2009 Y.Kernoa  <yoann.kernoa@etu.univ-orleans.fr>     *
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

/// Variable Frequency Synchronizor.
///
/// \brief This synchronizor enables to set the frequency
/// of module activation (via the module port beginIt).
///
/// When the synchronizor is launched, it sends a first message.
/// Then, the synchronizer waits for endIt signals to send more messages.
/// After, it sends a new message when it receives
/// an endIt signal, and  it waits the necessary time to
/// respect the frequency indicated.
/// The stamps'it is the value read in the endIt signal.
/// Moreover, the frequency can be modified by the port 'freq' during all the
/// time the application is running. The type of message on this port can be NEW for 
/// a new frequency, MIN to set the lower bound of the synchronizor or MAX to set the 
/// upper bound.
///
/// <b>Init parameters:</b>
/// -  frequency (from 1 to 1000000 Hertz);  Default Value = 1000000
/// -  alpha: low  threshold for QoS
/// -  beta: high treshold for QoS
///
/// <b>Input ports:</b>
/// -  freq
/// -  endIt
/// -  freq: receives a new max freq (float from 1 to 1000000 Hertz)
///
/// <b>Output Ports:</b>
/// - out
/// 

class VariableFrequencySynchronizor : public ThreadedSynchronizor
{
 public:
  VariableFrequencySynchronizor(const std::string &objID);
  
  virtual Class* getClass() const;
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);
  virtual flowvr::plugd::Result close(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  /**
   * implements the Thread::run() function, doing the bulky workload of this filter.
   */
  virtual int run(); 
 
 
  enum
  {
    IDPORT_ENDIT=0, /**< id of the ENDIT port (input) */
    IDPORT_FREQ,    /**< id of the frequency port (input) */
	NBPORTS         /**< mark the number of input ports */
  };


  float freqHz;  /**< frequency in Hertz (current frequency between freqMin and freqMax) */
  float freqMin; /**< freq. min (lower bound) */
  float freqMax; /**< freq. max (upper bound) */
  

  static const float MIN;   /**< the minimal frequency */

  bool running; /**< state running flag */
  
  flowvr::StampInfo *StampTypeFreq; /**< to obtain type of msg on inputPort IDPORT_FREQ (NEW,MIN,MAX freq.) */
  flowvr::StampInfo *StampValueFreq;/**< to obtain value of msg on inputPort IDPORT_FREQ */

  struct timeval timeLastSignalSend; /**< to save the last time a msg has been send on output port */
  struct timezone tz;

  StampList stamps;

  flowvr::plugd::Dispatcher* threadDispatcher;
  
  ipc::MTLock   timer;
  ipc::MTSignal signaltimer;

};

/// declare static const here
const float VariableFrequencySynchronizor::MIN=1.0f;


using namespace flowvr::xml;

/// Constructor.
/// Initializes running to false, the current frequency to 1.0Hz
VariableFrequencySynchronizor::VariableFrequencySynchronizor(const std::string &objID)
  : ThreadedSynchronizor(objID), running(false),
    freqHz(1.0f),
    freqMin(0.0f),
    freqMax(0.0f),
    threadDispatcher(NULL),
    stamps()
{
	memset(&tz,0, sizeof(tz));
	memset(&timeLastSignalSend, 0, sizeof(timeval));
}


/// Init method
flowvr::plugd::Result VariableFrequencySynchronizor::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  // default value
  freqMin = MIN; // 1 Hertz
  freqMax = 1000000.0f; // 1 Giga Hertz
	
	
  flowvr::plugd::Result result = Synchronizor::init(xmlRoot, dispatcher);
  if (result.error()) return result;
  
	threadDispatcher = dispatcher->threadCopy();
	
  freqHz = freqMax;
  // freq parameter reading ...
  xml::DOMNodeList* lfreq = xmlRoot->getElementsByTagName("freq");

  if (lfreq != NULL && lfreq->getLength()>=1)
  {
  	std::string fr = lfreq->item(0)->getTextContent();
	if (!(fr.empty()))
	  freqHz = atof(fr.c_str());
	  delete lfreq;
  }
  if (freqHz > freqMax || freqHz < freqMin )
  {
    // incorrect value for frequency in hertz
    return Result(flowvr::plugd::Result::ERROR,"Incorrect frequency parameter");
  }


  initInputs(NBPORTS);
  inputs[IDPORT_ENDIT]->setName("endIt");
  inputs[IDPORT_FREQ]->setName("freq");
  
  // Stamp declaration
  StampTypeFreq  = new flowvr::StampInfo("typeFreq", flowvr::TypeString::create()); // type : NEW, MIN, MAX
  StampValueFreq = new flowvr::StampInfo("valueFreq", flowvr::TypeString::create());// value : in Hz
  // Associate stamp with input port "Freq"
  stamps.add(StampTypeFreq);
  stamps.add(StampValueFreq);
	
	
  //only one outputmessagequeue for this filter
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::STAMPS;

  //give the stamplist to the outputmessage queue
  outputs[0]->newStampSpecification(dispatcher);

  running = true;

  start(); // call start of the thread super class
  
  return result;
}


/// method run() to launch the thread
int VariableFrequencySynchronizor::run()
{
    struct timeval oldtv;
	// waiting time evaluation
	long long waitTime = 0;
	long long diffTime = 0;
	long long period = (long long) (1.0f / freqHz) * 1000000.0f;
    
  while ( running )  // stopping condition ? 
    {
      // time of this sending --> first msg sent
      MessageWrite msg;      
      outputs[0]->put(msg,threadDispatcher);
    	
      gettimeofday(&oldtv, NULL); // save current time before to receive a new msg on input port

      ipc::ScopedMTLock locker(messageQueueLock(),"VariableFrequencySynchronizor.run");
      
      
      // Wait a new message on the input message queues "endit" or "freq"
      //wait_any();
      wait(IDPORT_ENDIT); //Some messages are lost with wait_any() since the end of april 2012. Updates in the Linux kernel?
      
      
      // If EndIt port received a msg -> as MaxFrequencySynchronizor
      if ( !inputs[IDPORT_ENDIT]->empty())
      {
      	inputs[IDPORT_ENDIT]->eraseFront();// supprimer le msg reçu
    
      	struct timeval tv;
      	gettimeofday(&tv, &tz);
    	diffTime=(tv.tv_sec-oldtv.tv_sec) * 1000000L + tv.tv_usec-oldtv.tv_usec;  // time in seconde -> micro-seconde thanks to (*1000000)		

      	waitTime =  period - diffTime; // time to wait before to send a new msg on output port
 
      	if ( waitTime > 0 ) // si temps a attendre positif, on attend, sinon, on doit renvoyer un message
		{	
		  
#ifdef DEBUG			
		  std::cout<<name()<<": waiting for "<< waitTime <<" usecs."<<std::endl;	
#endif
		
		  usleep(waitTime);  // wait time (in micro-sec)
		}
     }
     
     // If Freq port received a msg --> a new frequency must be used and affected in 'freqHz' (after check)
     // and the variable 'period' must be updated
     if( !inputs[IDPORT_FREQ]->empty())
     {     	
     	Message msg = inputs[IDPORT_FREQ]->frontMsg();
     	    	
     	// Depending on the type of msg
     	// 'typeFreq' Must be NEW (for new frequency), MIN (for lower bound) or MAX (for upper bound)
     	// 'valueFreq' must be a value between 0.1 and ... Hertz
     	std::string typeFreq;
     	std::string valueFreq;
     	float tmpFreq; // valeur temporaire
     	
     	msg.stamps.read(*StampTypeFreq , typeFreq); // on récupère la nature du msg
     	msg.stamps.read(*StampValueFreq , valueFreq);// on récuère  la valeur de la fréquence
     	
     	tmpFreq = atof(valueFreq.c_str()); // convert string to float
     	
     	if (typeFreq == "NEW")
     	{
     		if (!(valueFreq.empty())) // si valeur non vide
     		{ 
     			// si valeur dans les bornes, on utilise la nouvelle fréquence
     			if ( tmpFreq >= freqMin and tmpFreq <= freqMax ) 
  				{
    				freqHz = tmpFreq;
    				// update variable "period"
					period = (long long) 1000000.0f * ((double)(1.0f / freqHz));
					#ifdef DEBUG	
     					std::cout << name() << ": New frequency of " << freqHz << " Hertz - Time to wait in micro-seconds :" << period << std::endl;
					#endif
  				}			
			}
		}
		else 
		if (typeFreq == "MIN")
		{
     		if (!(valueFreq.empty()))
     		{ 
     			// on vérifie que min >= 1 Hertz
     			if ( tmpFreq >= MIN) 
  				{
    				freqMin = tmpFreq;
    				
    				#ifdef DEBUG	
     					std::cout << name() << ": Min frequency of " << freqMin << " Hertz " << std::endl;
					#endif
    				
    				// If the new lower bound is upper then current frequency, we increase current frequency
    				if(freqHz <freqMin)
    				{
    					freqHz = freqMin;
    					period = (long long) 1000000.0f * ((double)(1.0f / freqHz));
    					
    					#ifdef DEBUG	
	     					std::cout << name() << ": Update frequency of " << freqHz << " Hertz - Time to wait in micro-seconds :" << period << std::endl;
						#endif
    				}
  				}			
			}
		}
		else 
		if( typeFreq == "MAX")
		{
			if (!(valueFreq.empty()))
     		{ 
     			// on vérifie que max >= 1 Hertz and max > min
     			if ( tmpFreq >= MIN and tmpFreq > freqMin) 
  				{
    				freqMax = tmpFreq;
    				#ifdef DEBUG	
     					std::cout << name() << ": Max frequency of " << freqMax << " Hertz" << std::endl;
					#endif
					
					// If the new upper bound is lower then current frequency, we decrease current frequency
    				if(freqHz > freqMax)
    				{
    					freqHz = freqMax;
    					period = (long long) 1000000.0f * ((double)(1.0f / freqHz));
    				
    					#ifdef DEBUG	
    						std::cout << name() << ": Update frequency of " << freqHz << " Hertz - Time to wait in micro-seconds :" << period << std::endl;
    					#endif
    				}
  				}			
			}
		}
     	/// else, do nothing
    	    	
     	inputs[IDPORT_FREQ]->eraseFront(); // delete message received on input message queue 'IDPORT_FREQ'
     	
	 }// end of : IDPORT_FREQ
   }// end WHILE
	
	
#ifdef DEBUG
  std::cout<< objectID() << ": VariableFrequencySynchronizor thread now stopped. " << std::endl;
#endif
  
  threadDispatcher->close();
	
  return 0;
}

flowvr::plugd::Result VariableFrequencySynchronizor::close(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  ipc::ScopedMTLock locker(messageQueueLock(),"newMessageNotification");
  running = false;
 
  return BaseObject::close(xmlRoot,dispatcher);

}


// register plugin class (see genclass.h)
flowvr::plugd::GenClass<VariableFrequencySynchronizor> VariableFrequencySynchronizorClass("flowvr.plugins.VariableFrequencySynchronizor", // name
								    "", // description
								    &flowvr::plugins::ThreadedSynchronizorClass
								    );

Class* VariableFrequencySynchronizor::getClass() const
{
  return &VariableFrequencySynchronizorClass;
}

} // namespace plugins

} // namespace flowvr
