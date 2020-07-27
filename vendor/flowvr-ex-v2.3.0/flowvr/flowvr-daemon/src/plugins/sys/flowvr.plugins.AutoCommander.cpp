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
* File: src/plugins/flowvr.plugins.AutoCommander.cpp              *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugd/routingtable.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/plugins/commander.h"
#include "flowvr/plugins/regulator.h"
#include <iostream>

#include <list>

namespace flowvr
{

namespace plugins
{

using namespace flowvr::plugd;

class AutoCommander : public flowvr::plugins::Commander
{
public:

  class MyModule
  {
  public:
    Regulator* regulator;
    Object* minItFilter;
    Object* it2StartFilter;
    int nbConnect;
    MyModule() : regulator(NULL), minItFilter(NULL), it2StartFilter(NULL), nbConnect(0) {}
  };

  std::list<MyModule> myModules;

  AutoCommander(std::string objID)
    : Commander(objID)
  {
  }
  virtual ~AutoCommander()
  {
  }

  virtual Class* getClass() const;

  /// Initialization. Returns a XML document containing the result.
  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher)
  {
    flowvr::plugd::Result result = Commander::init(xmlRoot, dispatcher);
    return result;
  }

  int connect(RoutingTable* routes, Object* source, const std::string& sport, Object* dest, const std::string& dport,Message::Type msgtype=Message::FULL)
  {
    flowvr::xml::DOMElement xml("port");
    xml.LinkEndChild(new flowvr::xml::DOMText(dport));
    ActionHandler* action = dest->createAction(&xml);
    if (action==NULL) return false;
    std::string sourceid = source->objectID()+":"+sport;
    std::string destid = dest->objectID()+":"+dport;
    static int lastnum = 0;
    char id[24];
    ++lastnum;
    sprintf(id,"/Connect%d",lastnum);
    //string id = "/Connect"+sourceid+destid;
    if (flowvr::daemon::verboseLevel>=1)
      std::cout<<"Connecting "<<sourceid<<" to "<<destid<<std::endl;
    if (commandsOutput!=NULL)
      (*commandsOutput)<< "<addroute id=\""<<id+1<<"\">"
		       <<"<source id=\""<<source->objectID().substr(1)<<"\" port=\""<<sport<<"\""<<(msgtype==Message::STAMPS?" messagetype=\"stamps\"":"")<<"/>"
		       <<"<action id=\""<<dest->objectID().substr(1)<<"\">"<<flowvr::xml::DOMWriter::toString(&xml)<<"</action>"
		       <<"</addroute>"<<std::endl;
    if (!routes->addRoute(id,
			  sourceid,
			  msgtype,
			  new Action(action)))
      return 0;
    return lastnum;
  }

  void start(Object* obj)
  {
    xml::DOMElement root("action");
    root.SetAttribute("id",obj->objectID().substr(1));
    root.LinkEndChild(new xml::DOMElement("start"));
    if (commandsOutput!=NULL)
      (*commandsOutput)<<flowvr::xml::DOMWriter::toString(&root)<<std::endl;
    std::string s;
    s = obj->doAction(&root,threadDispatcher).toString();
    if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
  }

  /// Create an automatic connection
  bool autoconnect(RoutingTable* routes, MyModule* sourcemod, MyModule* destmod, const std::string& portName)
  {
    int cid;
    if (!(cid=connect(routes,sourcemod->regulator,portName,destmod->regulator,portName)))
      return false;
    // add synchronizors to set max buffers
    char buf[24];
    sprintf(buf,"/Connect%d",cid);
    std::string id = buf;
    //string id = "/Connect"+sourcemod->regulator->objectID()+":"+portName+destmod->regulator->objectID()+":"+portName;
    if (commandsOutput!=NULL)
      (*commandsOutput)<< "<addobject id=\""<<id.substr(1)<<"/Buf\" class=\"flowvr.plugins.MaxBuffer\">"
		       <<"<nb>10</nb></addobject>"<<std::endl;

    Class* maxBufferClass = Class::find("flowvr.plugins.MaxBuffer");
    if (maxBufferClass==NULL)
      return true; // abort synchronizers creation
    Object* maxBufferFilter = maxBufferClass->construct(id+"/Buf");
    {
      xml::DOMElement* root = new xml::DOMElement("connect");
      xml::DOMElement* nb = new xml::DOMElement("nb");
      nb->LinkEndChild(new xml::DOMText("10"));
      root->LinkEndChild(nb);
      Result res = maxBufferFilter->init(root,threadDispatcher);
      if (flowvr::daemon::verboseLevel>=1)
	std::cout << res.toString() << std::endl;
      if (res.error()) return true;
      delete root;
    }
    if (!connect(routes,destmod->regulator,"endIt",maxBufferFilter,"signal")) return true;
    if (!connect(routes,sourcemod->regulator,portName,maxBufferFilter,"portIt")) return true;
    if (sourcemod->minItFilter==NULL)
    {
      if (commandsOutput!=NULL)
      {
	(*commandsOutput)<< "<addobject id=\"minIt"<<sourcemod->regulator->objectID()
			 <<"\" class=\"flowvr.plugins.MinIt\"/>"<<std::endl;
	(*commandsOutput)<< "<addobject id=\"it2Start"<<sourcemod->regulator->objectID()
			 <<"\" class=\"flowvr.plugins.It2Start\"/>"<<std::endl;
      }
      Class* minItClass = Class::find("flowvr.plugins.MinIt");
      if (minItClass==NULL)
	return true; // abort synchronizers creation
      Class* it2StartClass = Class::find("flowvr.plugins.It2Start");
      if (it2StartClass==NULL)
	return true; // abort synchronizers creation
      xml::DOMElement root("addsync");
      sourcemod->minItFilter = minItClass->construct("/minIt"+sourcemod->regulator->objectID());
      std::string s;
      s = sourcemod->minItFilter->init(&root,threadDispatcher).toString();
      if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
      sourcemod->it2StartFilter = it2StartClass->construct("/it2Start"+sourcemod->regulator->objectID());
      s = sourcemod->it2StartFilter->init(&root,threadDispatcher).toString();
      if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
      if (!connect(routes,sourcemod->minItFilter,"out",sourcemod->it2StartFilter,"it",Message::STAMPS)) return true;
      if (!connect(routes,sourcemod->it2StartFilter,"out",sourcemod->regulator,"beginIt",Message::STAMPS)) return true;
    }

    sprintf(buf,"in%d",sourcemod->nbConnect); ++sourcemod->nbConnect;
    connect(routes,maxBufferFilter,"out",sourcemod->minItFilter,buf,Message::STAMPS);
    start(maxBufferFilter);
    return true;
  }

  virtual void processCmdModInit(flowvr::MPDaemonHeader::Cmd& cmd)
  {
    /// Well, this method has the worst-award...

    BufferWrite moduleBuf = cmd.arg;

    Commander::processCmdModInit(cmd);

    MPModuleDescription* moduleDesc = moduleBuf.getWrite<MPModuleDescription>();
    if (moduleDesc->flags&MPModuleDescription::CONTROLLER)
    {
      std::string moduleName = (std::string)moduleDesc->parent+"/"+(std::string)moduleDesc->name;
      {
	Object* old = Object::find(moduleName);
	if (old!=NULL)
	{
	  std::cout << "Removing old object "<<moduleName<<std::endl;
	  old->unregister();
	  std::string s = old->close(NULL,threadDispatcher).toString();
	  if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
	}
      }
      if (commandsOutput!=NULL)
      {
	(*commandsOutput) << "<addobject class=\""+RegulatorClass.name() << "\" id=\""
			  << moduleName.substr(1) << "\"/>" << std::endl;
      }
      Regulator* regulator = RegulatorClass.constructT(moduleName);
      if (regulator!=NULL)
      {
        std::string s = regulator->init(NULL,threadDispatcher).toString();
	if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
	std::cout << "AutoCommander: starting controller "<<regulator->objectID()<<std::endl;
	start(regulator);
      }
    }
    else if (moduleDesc->parent.empty())
    {
      std::string moduleName = (std::string)moduleDesc->parent+"/"+(std::string)moduleDesc->name;
      std::cout << "AutoCommander: init new module "<<moduleName<<std::endl;
      {
	Object* old = Object::find(moduleName);
	if (old!=NULL)
	{
	  std::cout << "Removing old object "<<moduleName<<std::endl;
	  old->unregister();
	  std::string s = old->close(NULL,threadDispatcher).toString();
	  if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;
	}
      }
      if (commandsOutput!=NULL)
	(*commandsOutput) << "<addobject class=\""+RegulatorClass.name() << "\" id=\""
			  << moduleName.substr(1) << "\"/>" << std::endl;
      Regulator* regulator = RegulatorClass.constructT(moduleName);

      std::list<MyModule*> tostart;

      if (regulator!=NULL)
      {
	std::string s;
        s = regulator->init(NULL,threadDispatcher).toString();
	if (flowvr::daemon::verboseLevel>=1) std::cout << s << std::endl;

	myModules.push_back(MyModule());
	MyModule* newModule=&*myModules.rbegin();
	newModule->regulator = regulator;
	newModule->minItFilter = NULL;
	newModule->it2StartFilter = NULL;

	/// auto add connections
	RoutingTable* routes = threadDispatcher->getRoutingTable();
	if (routes!=NULL)
	{
	  std::list<MyModule>::iterator it;

	  for (it = myModules.begin(); it != myModules.end(); ++it)
		{
			Regulator* dest = it->regulator;
			MPModuleDescription *hd = dest->getModuleDescription();
            if (dest->isStarted() == false)
              {
                for (int pOut = 0; pOut < hd->nbPorts; pOut++)
                {
					if (hd->ports[pOut].flags & MPPort::OUTPUT)
					{
						std::string portName = hd->ports[pOut].name;

						for (int pIn = 0; pIn < dest->nbInputs; pIn++)
							if (dest->inputs[pIn]->getName() == portName && dest->inputs[pIn]->getNbActions() == 0)
							{
								std::cout
										<< "AutoCommander: connecting "
										<< regulator->objectID()
										<< ':' << portName
										<< " to "
										<< dest->objectID() << ':'
										<< portName << std::endl;
								if (!autoconnect(routes, newModule,	&*it, portName))
									std::cerr
											<< "Connection Failed"
											<< std::endl;
								else
									// start receiver
									tostart.push_back(&*it);
							} //if empty input port with same name
					} // if output port
                    else
                    {
                        if (hd->ports[pOut].flags & MPPort::INPUT)
                        {
                            std::string portName = hd->ports[pOut].name;
                            MPModuleDescription *dest_hd = dest->getModuleDescription();

                            for (int pIn = 0; pIn < dest_hd->nbPorts; pIn++)
                                if ((std::string) dest_hd->ports[pIn].name == portName
                                        && (dest_hd->ports[pIn].flags & MPPort::OUTPUT))
                                {
                                    std::cout
                                            << "AutoCommander: connecting "
                                            << dest->objectID() << ':'
                                            << portName << " to "
                                            << regulator->objectID()
                                            << ':' << portName
                                            << std::endl;
                                    if (!autoconnect(routes, &*it,
                                                     newModule, portName))
                                        std::cerr
                                                << "Connection Failed"
                                                << std::endl;
                                }
                        } // if input port
                    }// if (hd->ports[pOut].flags & MPPort::OUTPUT)
                }//end for (int pOut = 0; pOut < hd->nbPorts; pOut++)
            }// (dest->isStarted() == false)
      } // for myModules
    } // routing table ok

	// start any module ready
	xml::DOMElement root("addobject");
	root.LinkEndChild(new xml::DOMElement("start"));
	tostart.push_back(newModule);
	while (!tostart.empty())
	{
	  MyModule* mod = tostart.back();
	  tostart.pop_back();

	  if (mod->regulator->isStarted())
		  continue; // already started

	  // check if all input ports are connected
	  bool ready=true;
	  Regulator::MPPortQueue *ports = mod->regulator->getPorts();

	  for (int i=0;i<mod->regulator->nbInputs;i++)
	    if (ports[i].port != NULL && mod->regulator->inputs[i]->getNbActions()==0)
	      ready=false;

	  if (!ready) continue; // still waiting for connections

	  // START!
	  std::cout << "AutoCommander: starting "<<mod->regulator->objectID()<<std::endl;
	  start(mod->regulator);

	  if (mod->it2StartFilter!=NULL)
	    start(mod->it2StartFilter);

	  if (mod->minItFilter!=NULL)
	    start(mod->minItFilter);
	}
      }
    }
  }
};

flowvr::plugd::GenClass<AutoCommander> AutoCommanderClass("flowvr.plugins.AutoCommander", // name
					       "", // description
					       "flowvr.plugins.Commander" // base class
					       );

Class* AutoCommander::getClass() const
{
  return &AutoCommanderClass;
}

} // namespace plugins

} // namespace flowvr
