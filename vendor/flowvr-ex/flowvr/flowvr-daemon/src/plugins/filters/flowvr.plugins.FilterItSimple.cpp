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
*    Sebastien Limet,                                             *
*    Sophie Robert,                                               *
* 	 Yoann Kernoa                                                 *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/plugins/flowvr.plugins.FilterItSimple.cpp             *
*                                                                 *
* Contacts:                                                       *
*  22/07/2009 Yoann Kernoa <yoann.kernoa@etu.univ-orleans.fr>     *
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

#include <deque> //add by Y.Kernoa - MAY 26 2009


namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

/// \brief A filter which chooses the message corresponding to the it
/// stamp or received orders. Deprecated, is this filter used
/// anywhere?
///
/// <b>Init parameters:</b> none.
///
/// <b>Input ports:</b>
/// -  <b>in</b>: Messages to be filtered.
/// -  <b>order</b>: Filtering orders (from a synchronizer such as flowvr::plugins::GreedySynchronizor).
///
/// <b>Output Ports:</b>
/// - <b>out</b>: Filtered messages.
/// - <b>stat</b>: Statistic about filtering


/* Y.Kernoa - 26 Mai 2009
*  Modification of "FilterIt" to avoid overflow of the input message queue
*   when we use a Greedy
*
*  Modifications :
* 	Double buffers (writer/reader) which "swap" when a new message is available on the input message queue ("writer" buffer)
* 	If buffers are empty and we receive a order message, we send an empty message as usual
*/

#define SIZE_MAX 256 // see MessageQueue -> method "doIt"-> test "if(queue.size > 256) cout<<"Warning long message queue..."


class FilterItSimple : public Filter
{
 public:

  FilterItSimple(const std::string &objID);

  virtual ~FilterItSimple();

  virtual Class* getClass() const;

  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

  virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
  virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

  enum {
    IDPORT_IN=0,
    IDPORT_ORDER,
    NBPORTS
  };

  int numout;

protected:
  virtual void sendPendingOrders(plugd::Dispatcher* dispatcher);

  TypedTrace<int> traceout;
  
  // to manage double buffers (swap...)
  virtual void manageDoubleBuffers();
  // buffer reader
  typedef std::deque<Message> Queue;
  Queue bufferReader;
  
  // to manage order message queue (avoid overflow)
  virtual void manageOrderBuffer();
};



using namespace flowvr::xml;

/// Constructor.
FilterItSimple::FilterItSimple(std::string objID)
  : Filter(objID), numout(0), traceout(TypedTrace<int>("out")), bufferReader() // add here by Y.KERNOA
{
}

FilterItSimple::~FilterItSimple()
{
}

/// Manage double buffers - swap 'input message queue (writer buffer) with 'reader buffer'
void FilterItSimple::manageDoubleBuffers()
{
	// when this method is called, we have either a message on the IDPORT_IN or the ORDER_PORT

	// message on the IN_PORT
	if(!inputs[IDPORT_IN]->empty())
 	{
 		// at the beginning, when 2 buffers are empties, we  can't swap buffers but copy "writer" buffer into "reader" buffer
	 	if (bufferReader.empty())
		{
#ifdef DEBUG
			std::cout<<" Copy input message queue (size of "<<inputs[IDPORT_IN]->size()<<") on 'Reader' buffer and clean up input message queue from : "<<objectID()<<std::endl;
#endif
			bufferReader.push_front(inputs[IDPORT_IN]->frontMsg());
			
			while(!inputs[IDPORT_IN]->empty())
			{
				inputs[IDPORT_IN]->eraseFront(); //clean up buffer in order to receive another message
			}
		}
		else // swap and clean up "writer" buffer
		{
#ifdef DEBUG
			std::cout<<" Swap buffers (input message queue - size of "<<inputs[IDPORT_IN]->size()<<") and clean up input message queue from : "<<objectID()<<std::endl;
#endif
	 		// when a message arrived, it is stored in the "Writer" buffer (input message queue)
 			bufferReader.swap(inputs[IDPORT_IN]->getMessageBuffer()); // We must now swap "Reader" buffer and "Writer" buffer
 			
 			while(!inputs[IDPORT_IN]->empty())
			{
				inputs[IDPORT_IN]->eraseFront(); //clean up buffer in order to receive another message
			}
 		}
	}
 	// else message on the ORDER_PORT
 }

// if order message queue > SIZE_MAX => get last order and clean up order message queue
void FilterItSimple::manageOrderBuffer()
{
	if(inputs[IDPORT_ORDER]->size() > SIZE_MAX)
	{
		while(inputs[IDPORT_ORDER]->size() > 1)
			inputs[IDPORT_ORDER]->eraseBack();
	}
}

flowvr::plugd::Result FilterItSimple::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
{
  flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
  if (result.error()) return result;

  //initialization of the input message queue
  initInputs(NBPORTS);
  inputs[IDPORT_IN]->setName("in");
  inputs[IDPORT_ORDER]->setName("order");


  //initialization of the OMQ "out"
  initOutputs(1);
  outputs[0]->setName("out");
  outputs[0]->msgtype = Message::FULL;

  outputTraces.push_back(&traceout);

  return result;
}


void FilterItSimple::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
{

#ifdef DEBUG
  if (mqid == IDPORT_IN)
    std::cout << objectID() <<": new input "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
  else
    std::cout << objectID()<<": new order "<<msgnum<<" queue size "<<inputs[mqid]->size()<<std::endl;
#endif

  sendPendingOrders(dispatcher);
}

void FilterItSimple::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
{
  if (mqid == IDPORT_IN)
  { // forward specification to out port
#ifdef DEBUG
    std::cout << objectID()<<": forwarding stamps specification"<<std::endl;
#endif

    //forwarding stamplist specification to the OMQ
    outputs[0]->stamps = inputs[mqid]->getStampList();
    outputs[0]->newStampSpecification(dispatcher);

    sendPendingOrders(dispatcher);
  }
}

void FilterItSimple::sendPendingOrders(plugd::Dispatcher* dispatcher)
{ // MAIN FILTER FUNCTION

//~ #define DEBUG

  if (!inputs[IDPORT_IN]->stampsReceived()) return; // still waiting for stamps specification

  int it = 0 ,itin = 0;
  for (;;)
  {   	
  	// add by Y.kernoa - MAY 26 2009 **********************************		
	manageDoubleBuffers(); // swap buffers (writer/reader)
	//~ manageOrderBuffer(); // avoid overflow
    // ****************************************************************
	
    Message msg;
    int num = 0 , numin = 0;
    { 
		
      if (!inputs[IDPORT_ORDER]->frontMsg().valid())
      {
#ifdef DEBUG
	std::cout<<objectID()<<": waiting orders "<<std::endl;
#endif
		return;
      }


      msg = inputs[IDPORT_ORDER]->frontMsg();

      msg.stamps.read(inputs[IDPORT_ORDER]->getStampList().num,num);
      msg.stamps.read(inputs[IDPORT_ORDER]->getStampList().it,it);
      
      bool scratch = (it<0);
      // ???????
      it = (it<0?-it:it)-10;
      itin = -10;
      //~ 
      //~ while(!inputs[IDPORT_IN]->empty()
	    //~ && inputs[IDPORT_IN]->frontMsg().valid()
	    //~ && inputs[IDPORT_IN]->frontMsg().stamps.read(inputs[IDPORT_IN]->getStampList().it,itin)
	    //~ && itin<it)
        //~ inputs[IDPORT_IN]->eraseFront();

		// tant que ( message disponible file d'entrée &&
		//            message valide &&
		//            numéro d'itération message > 0 &&
		//            numéro d'itération message < numéro de l'itération du message Order )
		// Alors -> supprimer message
	  

      if (scratch)
      {
#ifdef DEBUG
		std::cout<<objectID()<<": scratch order message"<<it<<std::endl;
#endif
		inputs[IDPORT_ORDER]->eraseFront();
		continue; // do not send any message, just clean the queue
      }

 	  // **************************************************************
 	  if(!bufferReader.empty())
 	  {
 	  	if (bufferReader.front().valid())
      	{
	      	bufferReader.front().stamps.read(inputs[IDPORT_IN]->getStampList().num,numin);
      		bufferReader.front().stamps.read(inputs[IDPORT_IN]->getStampList().it, itin); // obtain the message's iteration number
        	//~ std::cout<<"it order :"<<it<<" / it Msg :"<<itin<<" ----> from :"<<objectID()<<std::endl;
      	 	//~ std::cout<<"num order :"<<num<<" / num Msg :"<<numin<<std::endl;
      	}
      	else // delete all - message order and bufferReader . Then, wait a new message
      	{
#ifdef DEBUG
		std::cout<<objectID()<<": invalid message n° ("<<it<<") - waiting another message "<<std::endl;
#endif
  			//~ inputs[IDPORT_ORDER]->eraseFront(); // modification ICI (03 juin 2009)
      		bufferReader.clear();
			continue; // do not send any message, just clean the queue
	  	}
	  }
      // **************************************************************
      
      if ( itin >= it ) // the last arrived message is sent
      {
     	//~ msg = inputs[IDPORT_IN]->frontMsg();
	    msg = bufferReader.front();
      }
      else
      {
#ifdef DEBUG
        std::cout << objectID() << " : we received an order, but do not have any message to forward : send an empty message, contaning '0' " << std::endl;
#endif
        BufferPool poolout;
        BufferWrite data = poolout.alloc(Allocator::getAllocator(),sizeof(char));
        memcpy(data.writeAccess(),"0",sizeof(char));
        MessagePut newmsg;
        newmsg.data = data;
        	
        outputs[0]->put(newmsg,dispatcher);
		inputs[IDPORT_ORDER]->eraseFront();
	    num = numout++;
		return;
      }

      inputs[IDPORT_ORDER]->eraseFront();
      num = numout++;
    }

    
#ifdef DEBUG
    std::cout<<objectID()<<": sending message "<<numin<<std::endl;
    //~ std::cout<<objectID()<<": sending message "<<num<<std::endl;
#endif
    MessagePut newmsg;
    newmsg.stamps.clone(msg.stamps,&inputs[IDPORT_IN]->getStampList());
    newmsg.data=msg.data;
	
    traceout.write(it);
    
    // send message
    outputs[0]->put(newmsg,dispatcher,num);
  }
}

flowvr::plugd::GenClass<FilterItSimple> FilterItClass("flowvr.plugins.FilterItSimple", // name
						"", // description
						&flowvr::plugins::FilterClass
						);

Class* FilterItSimple::getClass() const
{
  return &FilterItClass;
}

} // namespace plugins

} // namespace flowvr
