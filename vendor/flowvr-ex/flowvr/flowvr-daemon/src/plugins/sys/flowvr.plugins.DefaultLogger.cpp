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
* File: src/plugins/flowvr.plugd.DefaultLogger.cpp                *
*                                                                 *
* Contacts:                                                       *
*  05/16/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/thread.h>

#include "flowvr/daemon.h"
#include "flowvr/daemondata.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugd/dispatcher.h"

#include "flowvr/plugins/logger.h"
#include "flowvr/bufferpool.h"

#include "flowvr/stamp.h"
#include "flowvr/trace.h"
#include <iostream>
#include <sstream>
#include <queue>
#include <sys/time.h>
#include <unistd.h>

namespace flowvr
{

namespace plugins
{

class DefaultLogger : public Logger
                    , public flowvr::Thread
{
public:

  BufferWrite bufInfo;
  flowvr::MPLogInfo* logInfo;
  flowvr::plugd::Dispatcher* threadDispatcher;
  BufferPool pool;
  int bufferSize;
  timespec period;
  timespec nexttime;
  bool newParameters;
  bool stop;
  int num;

  /// Constructor.
  DefaultLogger(std::string objID)
    : Logger(objID)
  {
    bufferSize = 1024;
    period.tv_sec=1;
    period.tv_nsec=0;
    newParameters = false;
    stop = false;
    num = 0;
  }

  virtual flowvr::plugd::Class* getClass() const;

  /// Initialization. Returns a XML document containing the result.
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher)
  {
    flowvr::plugd::Result result = Logger::init(xmlRoot, dispatcher);
    if (result.error()) return result;

    bufInfo = alloc(sizeof(flowvr::MPLogInfo));
    logInfo = bufInfo.getWrite<MPLogInfo>(0);
    logInfo->init();
    newParameters = true;
    result = setParameters(xmlRoot,true);
    threadDispatcher = dispatcher->threadCopy();
    start();
    return result;
  }

  /// Execute an action in immediate mode.
  virtual flowvr::plugd::Result doAction(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher)
  {
    flowvr::ipc::ScopedMPLock locker(logInfo->lock,"DefaultLogger::doAction");
    xml::DOMElement* child = xmlRoot->FirstChildElement();
    if (child!=NULL && !strcmp(child->getNodeName(),"time"))
    {
      Trace::cycle_t cycle;
//      struct timeval t;
//      gettimeofday(&t,NULL);
	  Trace::readCycle(&cycle);
      xml::DOMElement* xml = (xml::DOMElement*)child->Clone(); //new xml::DOMElement("time");
      xml->SetAttribute("sec",cycle.tv_sec);
      xml->SetAttribute("usec",cycle.tv_usec);
/*      std::ostringstream str;
      str << cycle;
      xml->SetAttribute("cycle",str.str());*/
      return flowvr::plugd::Result(flowvr::plugd::Result::OK,xml);
    }
    else if (child!=NULL && !strcmp(child->getNodeName(),"ping"))
    {
      xml::DOMElement reply("reply");
      Trace::cycle_t cycle;
/*      struct timeval t;
      gettimeofday(&t,NULL);*/
	  Trace::readCycle(&cycle);
      reply.SetAttribute("sec",cycle.tv_sec);
      reply.SetAttribute("usec",cycle.tv_usec);
/*      char str[32];
      sprintf(str,"%lld",cycle);
      reply.SetAttribute("cycle",str);*/
      child->InsertEndChild(reply);
//       child->SetAttribute("replyhost",hostName());
      return flowvr::plugd::Result(flowvr::plugd::Result::OK,child);
    }
    else if (child!=NULL && !strcmp(child->getNodeName(),"start"))
    {
      return flowvr::plugd::Result(flowvr::plugd::Result::OK,child);
    }
    else
    {
      // default action: set parameters
      flowvr::plugd::Result result = setParameters(xmlRoot);
      std::cout << "LOG: action notify"<<std::endl;
      logInfo->bufferFull.notify();
      return result;
    }
  }

  /// Create an ActionHandler for batch mode action execution.
  virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot)
  { // this object doesn't implement any action
    return NULL;
  }

  virtual int run()
  {
    flowvr::ipc::ScopedMPLock locker(logInfo->lock,"DefaultLogger::run");
    //logInfo->
    while (!stop)
    {
      int nbBufferToSend = 1;
      int nbBufferToCreate = 1;

      if (logInfo->nbBuffer<1)
      {
        nbBufferToSend = 0;
        nbBufferToCreate = 0;
      }

      if (newParameters)
      {
        if (bufferSize != pool.getCurrentBufferSize())
        {
          nbBufferToSend = logInfo->nbBuffer;
          nbBufferToCreate = pool.getMaxBuffer()-1;
        }
        else
        {
          if (pool.getMaxBuffer()-1 < logInfo->nbBuffer)
          {
            nbBufferToSend = logInfo->nbBuffer - (pool.getMaxBuffer()-1);
            nbBufferToCreate = 0;
          }
          else if (pool.getMaxBuffer()-1 > logInfo->nbBuffer)
          {
            nbBufferToCreate += pool.getMaxBuffer()-1 - logInfo->nbBuffer;
          }
	  pool.setMaxBuffer(logInfo->nbBuffer+1);
        }
      }

      int first = logInfo->firstBuffer;

      for (int i=0;i<nbBufferToCreate;i++)
      {
    	  ///@todo check memory handling here
        BufferWrite newBuf = pool.alloc(getAllocator(),bufferSize,true);
		if(!newBuf.valid())
		{
		  std::cerr << "WARNING: Logger buffer allocation failed."<<std::endl;
		  newBuf = pool.alloc(getAllocator(),bufferSize,false);
		}
		newBuf.getWrite<MPLogBuffer>(0)->init(bufferSize);
		logInfo->buffers[(first+logInfo->nbBuffer+i)&(MPLogInfo::MAX_BUFFER-1)] = newBuf;
      }

      if (nbBufferToCreate!=nbBufferToSend)
        logInfo->nbBuffer+=nbBufferToCreate-nbBufferToSend;
      logInfo->firstBuffer=(first+nbBufferToSend)&(MPLogInfo::MAX_BUFFER-1);

      //std::cout << "before sending" << std::endl;
      for (int i=0;i<nbBufferToSend; i++)
      {
          //std::cout << "Buffer " << i << std::endl;
        BufferWrite tosend = logInfo->buffers[(first+i)&(MPLogInfo::MAX_BUFFER-1)];
        size_t size = tosend.getWrite<MPLogBuffer>(0)->nextPos.exchange_and_add(tosend.getSize());
        if (size>tosend.getSize()) size = tosend.getSize();
        size_t pos = sizeof(MPLogBuffer);
        int nyield = 0;
        logInfo->buffers[(first+i)&(MPLogInfo::MAX_BUFFER-1)].clear();
        while (pos<size)
        {
        //std::cout << "pos = " << pos << " toSend size = " << tosend.getSize() <<  " size = " << size << std::endl;
          unsigned int val = *tosend.getRead<unsigned int>(pos);
          //std::cout << "val = "<< val << std::endl;
          if (val == 0)
          {
              break;
          }
          else if (val == 1)
            break;
          else
          {
            pos += val;//*4;
            nyield = 0;
          }
        }
        MessageWrite msg;
        if (pos>size)
          std::cerr << "LOG: Buffer size error: "<<pos<<"!="<<size<<std::endl;
        else
          msg.data = BufferWrite(tosend,sizeof(MPLogBuffer),pos-sizeof(MPLogBuffer));
        if (msg.data.getSize()>0)
        {
          static StampList stamplist;
          if (num == 0)
          { // endIt
            StampListSpecification stamps;
            MessageWrite msgspec;
            msgspec.stamps.write(stamps.source,objectID()+":log");
            msgspec.stamps.write(stamps.num,-1);
            msgspec.stamps.write(stamps.it,0);
            xml::DOMNode* xmlspec = stamplist.generateXML();
            msgspec.stamps.write(stamps.spec,xml::DOMWriter::toString(xmlspec));
            delete xmlspec;
            ///@todo check memory handling here
            msgspec.data = alloc(0);
            threadDispatcher->process(msgspec);
          }

          msg.stamps.write(stamplist.source,objectID()+":log");
          msg.stamps.write(stamplist.it,num);
          msg.stamps.write(stamplist.num,num);
          num++;
	  //std::cout << "LOG: put msg "<<num-1<<" size="<<msg.data.getSize()<<std::endl;
          threadDispatcher->process(msg);
        }
      }
      {
	struct timeval now;
	gettimeofday(&now,NULL);
	if ((now.tv_sec > nexttime.tv_sec) || (now.tv_sec==nexttime.tv_sec && now.tv_usec*1000 >= nexttime.tv_nsec))
	{
	  std::cout << "LOG: reset nexttime"<<std::endl;
	  nexttime.tv_sec = now.tv_sec;
	  nexttime.tv_nsec = now.tv_usec*1000;
	}
      }
      timespec t = nexttime;
      if (logInfo->bufferFull.timedwait(logInfo->lock,&t))
      {
          period.tv_sec = 10;
          period.tv_nsec = 0;
        nexttime.tv_sec+=period.tv_sec;
        nexttime.tv_nsec+=period.tv_nsec;
        if (nexttime.tv_nsec>1000000000)
        {
          nexttime.tv_sec++;
          nexttime.tv_nsec-=1000000000;
        }
      }
    }

    std::cout << "receive stop!" << std::endl;
    return 0;
  }

  virtual BufferWrite getLogInfo()
  {
    return bufInfo;
  }

protected:

  flowvr::plugd::Result setParameters(flowvr::xml::DOMElement* xmlRoot, bool first=false)
  {

    xml::DOMNodeList* lbuffer = xmlRoot->getElementsByTagName("buffer_nb");
    if (lbuffer->getLength()>0)
    {
      const char* nb = ((flowvr::xml::DOMElement*)lbuffer->item(0))->GetText();
      if (nb!=NULL)
      {
        pool.setMaxBuffer(atoi(nb));
        newParameters = true;
      }
      lbuffer = xmlRoot->getElementsByTagName("buffer_size");
      const char* size = ((flowvr::xml::DOMElement*)lbuffer->item(0))->GetText();
      if (size!=NULL)
      {
        bufferSize = atoi(size);
        newParameters = true;
      }
    }
    delete lbuffer;

    xml::DOMNodeList* lperiod = xmlRoot->getElementsByTagName("period");
    if (lperiod->getLength()>0)
    {
      std::string sp = lperiod->item(0)->getTextContent();
      double p = atof(sp.c_str());
      period.tv_sec = (int) p;
      period.tv_nsec = (int)((p-period.tv_sec)*1000000000);
    }
    delete lperiod;

    if (first)
    {
      struct timeval now;
      gettimeofday(&now,NULL);
      nexttime.tv_sec = now.tv_sec;
      nexttime.tv_nsec = now.tv_usec*1000;
    }

    xml::DOMNodeList* lstart = xmlRoot->getElementsByTagName("start");
    if (lstart->getLength()>0)
    {
      std::string sp = lstart->item(0)->getTextContent();
      double p = atof(sp.c_str());
      nexttime.tv_sec += (int) p;
      nexttime.tv_nsec += (int)((p-nexttime.tv_sec)*1000000000);
    }
    delete lstart;
//       return flowvr::plugd::Result(flowvr::plugd::Result::OK,"OK");
      return flowvr::plugd::Result(flowvr::plugd::Result::OK,xmlRoot);
  }

};

flowvr::plugd::GenClass<DefaultLogger> DefaultLoggerClass
(
 "flowvr.plugins.DefaultLogger", // name
 "", // description
 "flowvr.plugins.Logger" // base
);

flowvr::plugd::Class* DefaultLogger::getClass() const
{
  return &DefaultLoggerClass;
}

} // namespace plugins

} // namespace flowvr
