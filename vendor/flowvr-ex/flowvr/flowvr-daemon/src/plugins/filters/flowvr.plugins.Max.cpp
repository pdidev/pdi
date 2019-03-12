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
 * File: src/plugins/flowvr.plugins.Max.cpp                      *
 *                                                                 *
 * Contacts:                                                       *
 *  01/16/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/ipc/mtlock.h"
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


     /// \brief transmits the max it of all messages on the input
     /// ports.

      class Max : public Filter
      {
	 public:

	    Max(const std::string &objID);

	    virtual ~Max();

	    virtual Class* getClass() const;

	    virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher);

	    virtual void newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher);
	    virtual void newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher);

	    unsigned int NBPORTS;
	    bool stampsSpecified;
	    int lastNum; ///< number of messages sent

	 protected:

	    std::string stampname;
	    StampInfo** stamp;

	    BufferPool poolout;

	    virtual void sendPendingMessages(plugd::Dispatcher* dispatcher);
	    ipc::MTLock lockSend; 

      };

      using namespace flowvr::xml;

      /// Constructor.
      Max::Max(const std::string &objID)
	 : Filter(objID), NBPORTS(0), stampsSpecified(false), lastNum(0), stamp(NULL), lockSend()
      {
      }

      Max::~Max()
      {
	 if (stamp!=NULL) delete[] stamp;
      }

      flowvr::plugd::Result Max::init(flowvr::xml::DOMElement* xmlRoot, flowvr::plugd::Dispatcher* dispatcher)
      {
	 flowvr::plugd::Result result = Filter::init(xmlRoot, dispatcher);
	 if (result.error()) return result;

	 xml::DOMNodeList* lnb = xmlRoot->getElementsByTagName("nb");
	 if (lnb->getLength()<1) return Result(flowvr::plugd::Result::ERROR,"No nb parameter");
	 std::string nb = lnb->item(0)->getTextContent();
	 NBPORTS = atoi(nb.c_str());
	 delete lnb;

	 initInputs(NBPORTS);

	 char buf[16];
	 for (unsigned int i=0;i<NBPORTS;i++)
	 {
	    sprintf(buf,"in%d",i);
	    inputs[i]->setName(buf);
	    inputs[i]->setCouldBeDisconnected(); //This port can be disconnected
	 }

	 //only one outputmessagequeue for this filter
	 initOutputs(1);
	 outputs[0]->setName("out");
	 outputs[0]->msgtype=Message::STAMPS;


	 return result;
      }

      void Max::newMessageNotification(int mqid, int msgnum, const Message& msg, Dispatcher* dispatcher)
      {
#ifdef DEBUG
	 std::cout << name()<<": new input"<<mqid<<" "<<msgnum<<std::endl;
#endif
	lockSend.lock(); 
	 sendPendingMessages(dispatcher);
	lockSend.unlock(); 
      }

      void Max::newStampListSpecification(int mqid, const Message& msg, Dispatcher* dispatcher)
      {
	 if (stamp != NULL)
	 {
	    stamp[mqid] = inputs[mqid]->getStampList()[stampname];
	    if (stamp[mqid] == NULL)
	       std::cerr << objectID() << ":in"<<mqid<<" ERROR stamp "<<stampname<<" not found."<<std::endl;
#ifdef DEBUG
	    else
	       std::cout << objectID() << ":in"<<mqid<<": stamp "<<stampname<<" @ "<<stamp[mqid]->getOffset()<<std::endl;
#endif
	 }
	 if (mqid == 0 && !stampsSpecified)
	 { // forward specification to out port
#ifdef DEBUG
	    std::cout << name()<<": forwarding stamps specification"<<std::endl;
#endif

	    //give the Stamplist to the outputmessage queue    
	    outputs[0]->stamps  = inputs[0]->getStampList();
	    outputs[0]->newStampSpecification(dispatcher);

	    stampsSpecified = true;
	 }
	 //  sendPendingMessages(dispatcher);
      }

      void Max::sendPendingMessages(plugd::Dispatcher* dispatcher)
      {
	 int max = -1;
	 for(unsigned int i = 0; i != NBPORTS; ++i)
	 {
	    if(!inputs[i]->empty())
	    {
		  Message m = inputs[i]->frontMsg();
		  int readdata = 0;
		  m.stamps.read(inputs[i]->getStampList().it, readdata);
		  if(max < readdata)
		  {
		     max = readdata;
		  }
	    }
	    else
	    {
	       return;
	    }
	 }
	 MessagePut mresult;
	 mresult.stamps.clone(inputs[0]->frontMsg().stamps, &inputs[0]->getStampList());  
	 // I must remove a front message on each port
	 for(unsigned int i = 0; i != NBPORTS; ++i)
	 {
	    inputs[i]->eraseFront();
	 }
	 mresult.stamps.write(outputs[0]->stamps.it, max);
//	 mresult.stamps.write(outputs[0]->stamps.num, lastNum);

	 outputs[0]->put(mresult, dispatcher, lastNum++);
      
	}

      flowvr::plugd::GenClass<Max> MaxClass("flowvr.plugins.Max", // name
	    "", // description
	    &flowvr::plugins::FilterClass
	    );

      Class* Max::getClass() const
      {
	 return &MaxClass;
      }

   } // namespace plugins

} // namespace flowvr
