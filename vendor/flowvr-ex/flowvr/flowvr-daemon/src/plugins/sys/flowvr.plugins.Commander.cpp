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
 * File: src/plugins/flowvr.plugins.Commander.cpp                  *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include <flowvr/plugd/object.h>
#include <flowvr/thread.h>

#include "flowvr/plugins/commander.h"
#include "flowvr/plugd/genclass.h"
#include "flowvr/plugd/routingtable.h"
#include "flowvr/plugins/regulator.h"
#include "flowvr/mem/daemonsharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/stamp.h"
#include "flowvr/xml.h"
#include <iostream>
#include <fstream>

namespace flowvr
{

namespace plugins
{

static inline const char* notNULL(const char* s)
{
	return (s == NULL) ? "" : s;
}

static std::string sourcePort(const std::string& sid)
{
	return std::string(sid, sid.find_last_of(":") + 1);
}

static std::string sourceObject(const std::string& sid)
{
	return std::string(sid, 0, sid.find_last_of(":"));
}

static std::string destFromReply(std::string& reply)
{
	if (reply.size() > 2 && reply[0] == '/')
	{
		std::string::size_type pos = reply.find('/', 1);
		if (pos != std::string::npos)
			--pos;
		return reply.substr(1, pos);
	}
	else
		return std::string("");
}


/// Constructor.
Commander::Commander(std::string objID) :
	Object(objID), shm(NULL), header(NULL), threadDispatcher(NULL),
			commandsOutput(NULL), closeOutput(false)
{
}

Commander::~Commander()
{
}

/// Initialization. Returns a XML document containing the result.
plugd::Result Commander::init(flowvr::xml::DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{
	plugd::Result result = Object::init(xmlRoot, dispatcher);
	if (result.error())
		return result;

	xml::DOMNodeList* lmemid = xmlRoot->getElementsByTagName("memid");

	if (lmemid->getLength() < 1)
		return plugd::Result(flowvr::plugd::Result::ERROR, "No memid parameter");

	std::string smemid = lmemid->item(0)->getTextContent();
	int mem_id = atoi(smemid.c_str());
	delete lmemid;

	xml::DOMNodeList* lout = xmlRoot->getElementsByTagName("out");
	if (lout->getLength() >= 1)
	{
		std::string fname = lout->item(0)->getTextContent();
		if (fname.size() == 0)
			commandsOutput = &std::cout;
		else
		{
			commandsOutput = new std::ofstream(fname.c_str());
			closeOutput = true;
		}
	}
	delete lout;

	threadDispatcher = dispatcher->threadCopy();
	shm = flowvr::mem::SharedMemoryManager::instance()->openMemoryArea(mem_id);

	if (shm != NULL)
		header = shm->getWrite<MPDaemonHeader> (shm->readHeader());

	if (header != NULL)
		if (start())
			detach();


	return result;
}



void Commander::processCmdModInit(flowvr::MPDaemonHeader::Cmd& cmd)
{
	BufferWrite moduleBuf = cmd.arg;

	flowvr::daemon::registerNewModule(moduleBuf);
	const MPModuleDescription* desc =
			moduleBuf.getRead<MPModuleDescription> (0);

	std::string reply = std::string(desc->parent);
	if (!reply.empty())
	{
		xml::DOMElement xml("news");
		xml.SetAttribute("id", "newmodule");
		xml.SetAttribute("status", "OK");
		xml.LinkEndChild(Regulator::moduleXmlDescription(desc));

		StampListControl stamps;
		MessageWrite msg;
		std::string xmltext = xml::DOMWriter::toString(&xml);

		msg.stamps.write(stamps.source, objectID());
		msg.stamps.write(stamps.it, 0);
		msg.stamps.write(stamps.num, 0);
		std::string dest = destFromReply(reply);
		msg.stamps.write(stamps.dest, dest);
		msg.stamps.write(stamps.reply, reply);

		msg.data = alloc(xmltext.length());
		memcpy(msg.data.writeAccess(), xmltext.c_str(), xmltext.length());

		threadDispatcher->processControlMessage(msg);
	}

	cmd.arg.clear();
}

void Commander::processCmdModExit(flowvr::MPDaemonHeader::Cmd& cmd)
{
	BufferWrite moduleBuf = cmd.arg;
	const MPModuleDescription* desc =
			moduleBuf.getRead<MPModuleDescription> (0);
        assert(desc);
	std::string reply = std::string(desc->parent);
	if (!reply.empty())
	{
		xml::DOMElement xml("news");
		xml.SetAttribute("id", "purgemodule");
		xml.SetAttribute("status", "OK");
		xml.LinkEndChild(Regulator::moduleXmlDescription(desc));

		StampListControl stamps;
		MessageWrite msg;
		std::string xmltext = xml::DOMWriter::toString(&xml);

		msg.stamps.write(stamps.source, objectID());
		msg.stamps.write(stamps.it, 0);
		msg.stamps.write(stamps.num, 0);
		std::string dest = destFromReply(reply);
		msg.stamps.write(stamps.dest, dest);
		msg.stamps.write(stamps.reply, reply);


		msg.data = alloc(xmltext.length());
		memcpy(msg.data.writeAccess(), xmltext.c_str(), xmltext.length());

		threadDispatcher->processControlMessage(msg);
	}

	cmd.arg.clear();
}

void Commander::processCmdAbort(flowvr::MPDaemonHeader::Cmd& cmd)
{
	BufferWrite moduleBuf = cmd.arg;
	const MPModuleDescription* desc =
			moduleBuf.getRead<MPModuleDescription> (0);
        assert(desc);
	std::string reply = std::string(desc->parent);
	if (!reply.empty())
	{
		xml::DOMElement xml("news");
		xml.SetAttribute("id", "abort");
		xml.SetAttribute("status", "OK");
		xml.LinkEndChild(Regulator::moduleXmlDescription(desc));

		StampListControl stamps;
		MessageWrite msg;
		std::string xmltext = xml::DOMWriter::toString(&xml);

		msg.stamps.write(stamps.source, objectID());
		msg.stamps.write(stamps.it, 0);
		msg.stamps.write(stamps.num, 0);
		std::string dest = destFromReply(reply);
		msg.stamps.write(stamps.dest, dest);
		msg.stamps.write(stamps.reply, reply);


		msg.data = alloc(xmltext.length());
		memcpy(msg.data.writeAccess(), xmltext.c_str(), xmltext.length());

		threadDispatcher->processControlMessage(msg);
	}

	cmd.arg.clear();
}

int Commander::run()
{
	bool bLeave = false;
	while(!bLeave)
	{
		int cmdnum = header->cmdChan.beginRead();
		flowvr::MPDaemonHeader::Cmd& cmd = header->cmdTable[cmdnum];
		switch (cmd.id)
		{
		case flowvr::MPDaemonHeader::Cmd::MODINIT:
			processCmdModInit(cmd);
			break;
		case flowvr::MPDaemonHeader::Cmd::MODEXIT:
			processCmdModExit(cmd);
			break;
		case flowvr::MPDaemonHeader::Cmd::SHMLIST:
			mem::DaemonSharedMemoryManager::processCmdShmList( cmd.arg );
			break;
		case flowvr::MPDaemonHeader::Cmd::SHMNEW:
			mem::DaemonSharedMemoryManager::processCmdShmNew( cmd.arg );
			break;
		case flowvr::MPDaemonHeader::Cmd::ABORT:
			processCmdAbort(cmd);
			break;
		case flowvr::MPDaemonHeader::Cmd::SHUTDOWN:
			bLeave = true; // leave loop
			break;
		}
		// Now the read has been done, we clear the now unused MPBuffer
		// This is important for any new SharedMemoryManager to be able to send
		//  their MPInterface (to the daemon) during their initialization.
		header->cmdTable[cmdnum].arg.clear();
		header->cmdChan.endRead();
	}

	threadDispatcher->close();

	return 0;
}


flowvr::plugd::ActionHandler* Commander::createAction(
		flowvr::xml::DOMElement* xmlRoot)
{
	xml::DOMElement* child = xmlRoot->FirstChildElement();
	if (child != NULL && !strcmp(child->getNodeName(), "control"))
	{
		ControlMessageHandler* handler = new ControlMessageHandler(this);
		handler->init();
		return handler;
	}
	return NULL;
}

Commander::ControlMessageHandler::ControlMessageHandler(Commander* _parent)
  : lock("Commander::ControlMessageHandler.lock")
  , signal("Commander::ControlMessageHandler.signal")
  , parent(_parent)
{
	stop = false;
}

void Commander::ControlMessageHandler::init()
{
	dispatcher = parent->threadDispatcher->threadCopy();
	if (start())
		detach();
}

Commander::ControlMessageHandler::~ControlMessageHandler()
{
}

/// Execute the action processing the specified message.
/// The dispatcher parameter is used to dispatch any generated message
void Commander::ControlMessageHandler::doIt(const Message& msg,
		plugd::Dispatcher* dispatcher)
{
	ipc::ScopedMTLock locker(lock, "doIt");
	queue.push_back(msg);
	signal.notify();
}

/// Remove this ActionHandler.
/// This will destroy this instance if ActionHandlers are dynamically
/// instanciated or it will remove one reference.
void Commander::ControlMessageHandler::remove()
{
	stop = true;
	signal.notifyAll();
}

bool Commander::ControlMessageHandler::waitNewMessage(Message& msg)
{
	ipc::ScopedMTLock locker(lock, "doIt");
	while (!stop && queue.empty())
		signal.wait(lock);
	if (!queue.empty())
	{
		msg = queue.front();
		queue.pop_front();
	}
	return stop;
}

int Commander::ControlMessageHandler::run()
{
	Message msg;
	while (!waitNewMessage(msg))
		processControlMessage(msg);
	delete this;
	return 0;
}

using namespace xml;

/// Determine the command to do and process it
plugd::Result Commander::ControlMessageHandler::processCommand(
		xml::DOMElement* root, const std::string&reply)
{
	if (!strcmp(root->getNodeName(), "addobject"))
	{
		return processAddObject(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "delobject"))
	{
		return processDelObject(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "addroute"))
	{
		return processAddRoute(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "delroute"))
	{
		return processDelRoute(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "getroutecount"))
	{
		return processGetRouteCount(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "action"))
	{
		return processAction(root, reply);
	}
	else if (!strcmp(root->getNodeName(), "group"))
	{
		return processGroupAction(root, reply);
	}
	else
		return plugd::Result(plugd::Result::ERROR, "Unknown command");
}

/// Process a control message from a local controller (or from the network)
void Commander::ControlMessageHandler::processControlMessage(
		const flowvr::Message& msg)
{
	static StampListControl stamps;
	std::string source;
	if (!msg.stamps.read(stamps.source, source))
		return;

	std::string dest;
	if (!msg.stamps.read(stamps.dest, dest))
		return;

	std::string reply;
	if (!msg.stamps.read(stamps.reply, reply))
		return;

	int it;
	if (!msg.stamps.read(stamps.it, it))
		return;

	if (flowvr::daemon::verboseLevel >= 1)
		std::cout << "Commander: Processing command " << it << " from "
				<< source << " for " << dest << " on " << reply << std::endl;

	std::string text((const char*) msg.data.readAccess(),
			((const char*) msg.data.readAccess()) + msg.data.getSize());

	DOMParser parser;
	plugd::Result res;
	if (parser.parseString(text.c_str()))
	{
		res.setStatus(plugd::Result::ERROR);
		DOMElement* desc = new DOMElement("xmlparsingerror");
		desc->SetAttribute("id", parser.getDocument()->ErrorId());
		desc->LinkEndChild(new DOMText(parser.getDocument()->ErrorDesc()));
		res.setXML(desc);
	}
	else
	{
		DOMElement* root = parser.getDocument()->RootElement();
		if (!strcmp(root->getNodeName(), "news") || !strcmp(
				root->getNodeName(), "result"))
		{
			processReply(msg, reply);
			return; // no result of action to transmit
		}

		if (parent->commandsOutput != NULL)
			(*parent->commandsOutput) << flowvr::xml::DOMWriter::toString(root) << std::endl;

		res = processCommand(root, reply);
	}
	// send the result back
	MessageWrite resmsg;
	resmsg.stamps.write(stamps.source, dest);
	resmsg.stamps.write(stamps.it, it);
	resmsg.stamps.write(stamps.num, 0);
	dest = destFromReply(reply);
	resmsg.stamps.write(stamps.dest, dest);
	resmsg.stamps.write(stamps.reply, reply);

	DOMElement* root = new DOMElement("result");
	root->SetAttribute("id", it);
	switch (res.getStatus())
	{
	case plugd::Result::OK:
		root->SetAttribute("status", "OK");
		break;
	case plugd::Result::ERROR:
		root->SetAttribute("status", "ERROR");
		break;
	}
	if (res.getXML() != NULL)
	{
		root->LinkEndChild(res.getXML());
		res.setXML(NULL);
	}
	std::string resulttext = DOMWriter::toString(root);
	resmsg.data = parent->alloc(resulttext.length());
	memcpy(resmsg.data.writeAccess(), resulttext.c_str(), resulttext.length());

	if (flowvr::daemon::verboseLevel >= 1)
		std::cout << "Sending " << resulttext << " to " << dest << std::endl;

	Message newmsg = resmsg;
	dispatcher->processControlMessage(newmsg);
}



plugd::Result Commander::ControlMessageHandler::processGroupAction(
		xml::DOMElement* root, const std::string& reply)
{

	xml::DOMElement* child = root->FirstChildElement();
	std::vector<std::string> vecid;
	plugd::Result result;

	std::string id = notNULL(root->Attribute("id"));

	//set the id
	if (id.empty() || id.at(0) != '/')
	{
		id = reply + "/" + id;
	}

	//is the command applied on route ?
	if (strstr(child->getNodeName(), "route") != NULL)
	{
		//get the routing table
		plugd::RoutingTable* table = dispatcher->getRoutingTable();
		vecid = table->getListID(id);
	}
	//if not then this is a group object concerned  by this command
	else
	{
		//get the list of objects concerned by the command
		vecid = Object::getListObject(id);
	}

	if (flowvr::daemon::verboseLevel >= 1)
	{
		std::cout << "ID in group " << id << ":";
		for (unsigned int i = 0; i < vecid.size(); i++)
			std::cout << ' ' << vecid[i];
		std::cout << std::endl;
	}

	std::vector<std::string>::iterator vit = vecid.begin();
	xml::DOMNode * rep = new xml::DOMElement("groupresult");

	//go through the list and make the processcommand call on each id then construct the result for the group command
	while (vit != vecid.end())
	{
		std::string idobject = *vit;
		child->SetAttribute("id", idobject);

		plugd::Result res = processCommand(child, reply);

		DOMElement* root = new DOMElement("result");
		root->SetAttribute("id", idobject);
		switch (res.getStatus())
		{
		case plugd::Result::OK:
			root->SetAttribute("status", "OK");
			break;
		case plugd::Result::ERROR:
			root->SetAttribute("status", "ERROR");
			break;
		}
		if (res.getXML() != NULL)
		{
			root->LinkEndChild(res.getXML());
			res.setXML(NULL);
		}

		rep->LinkEndChild(root);

		vit++;
	}

	result.setXML(rep);
	return result;

}

plugd::Result Commander::ControlMessageHandler::processAddObject(
		xml::DOMElement* root, const std::string& reply)
{
	std::string id = notNULL(root->Attribute("id"));
	std::string classname = notNULL(root->Attribute("class"));
	if (id.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute id");

	if (classname.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute class");

	plugd::Class* objClass = plugd::Class::find(classname);
	if (objClass == NULL)
		return plugd::Result(plugd::Result::ERROR, "Invalid class " + classname);

	std::string objID = id;
	if (id.empty() || id.at(0) != '/')
		objID = reply + "/" + id;

	Object* obj = objClass->construct(objID);
	if (obj == NULL)
		return plugd::Result(plugd::Result::ERROR, "Object creation failed");

	return obj->init(root, dispatcher);
}

plugd::Result Commander::ControlMessageHandler::processDelObject(
		xml::DOMElement* root, const std::string& reply)
{
	std::string id = notNULL(root->Attribute("id"));
	if (id.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute id");
	std::string objID = id;
	if (id.empty() || id.at(0) != '/')
		objID = reply + "/" + id;
	Object* obj = Object::find(objID);
	if (obj == NULL)
		return plugd::Result(plugd::Result::ERROR, "Object " + objID
				+ " not found");

	return obj->close(root, dispatcher);
}

plugd::Result Commander::ControlMessageHandler::processAddRoute(
		xml::DOMElement* root, const std::string& reply)
{
	std::string id = notNULL(root->Attribute("id"));
	if (id.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute id");

	DOMNodeList* sources = root->getElementsByTagName("source");
	DOMNodeList* actions = root->getElementsByTagName("action");

	if (sources->getLength() != 1)
		return plugd::Result(plugd::Result::ERROR, "Need exactly one source");

	if (actions->getLength() != 1)
		return plugd::Result(plugd::Result::ERROR, "Need exactly one action");

	DOMElement* source = (DOMElement*) sources->item(0);
	DOMElement* action = (DOMElement*) actions->item(0);
	std::string sourceid = notNULL(source->Attribute("id"));
	std::string sourceport = notNULL(source->Attribute("port"));
	std::string destid = notNULL(action->Attribute("id"));
	Message::Type msgtype = Message::FULL;

	if (!strcmp("stamps", notNULL(source->Attribute("messagetype"))))
		msgtype = Message::STAMPS;

	bool stampsonly = (!strcmp("stamps", notNULL(action->Attribute(
			"messagetype"))));

	if (destid.empty() || destid.at(0) != '/')
		destid = reply + "/" + destid;

	if (id.empty() || id.at(0) != '/')
		id = reply + "/" + id;

	if (sourceid.empty() || sourceid.at(0) != '/')
		sourceid = reply + "/" + sourceid;

	Object* dest = Object::find(destid);
	if (dest == NULL)
		return plugd::Result(plugd::Result::ERROR, "Object " + destid + " not found");

	ActionHandler *handler = dest->createAction(action);
	if (handler == NULL)
		return plugd::Result(plugd::Result::ERROR, "Action creation failed (unknown port?)");

	std::string sourceall = sourceid + ":" + sourceport;
	if (flowvr::daemon::verboseLevel >= 1)
		std::cout << "Connecting " << sourceall << " to " << dest->objectID()
				<< ":" << action->getTextContent() << " with id=" << id
				<< ((stampsonly && msgtype == Message::FULL) ? " removing data" : "") << std::endl;


	plugd::RoutingTable* table = dispatcher->getRoutingTable();
	if (!table->addRoute(id, sourceall, msgtype, new plugd::Action(handler, (stampsonly ? plugd::Action::STAMPSONLY : 0))))
		return plugd::Result(plugd::Result::ERROR, "Connection Failed");

	return plugd::Result(plugd::Result::OK, root);
}

plugd::Result Commander::ControlMessageHandler::processDelRoute(
		xml::DOMElement* root, const std::string& reply)
{
	std::string id = notNULL(root->Attribute("id"));
	if (id.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute id");

	if (id.empty() || id.at(0) != '/')
		id = reply + "/" + id;

	plugd::RoutingTable* table = dispatcher->getRoutingTable();
	if (!table->removeRoute(id))
		return plugd::Result(plugd::Result::ERROR, "Route deletion failed");

	return plugd::Result(plugd::Result::OK, "Route deleted");
}

plugd::Result Commander::ControlMessageHandler::processGetRouteCount(
		xml::DOMElement* root, const std::string& reply)
{
	std::string id = notNULL(root->Attribute("id"));
	if (id.empty())
		return plugd::Result(plugd::Result::ERROR, "Missing attribute id");

	if (id.empty() || id.at(0) != '/')
		id = reply + "/" + id;

	plugd::RoutingTable* table = dispatcher->getRoutingTable();
	return table->getRouteCount(id);
}

plugd::Result Commander::ControlMessageHandler::processAction(
		xml::DOMElement* root, const std::string& reply)
{
	DOMElement* action = root;
	std::string destid = notNULL(action->Attribute("id"));

	if (destid.empty() || destid.at(0) != '/')
		destid = reply + "/" + destid;

	Object* dest = Object::find(destid);
	if (dest == NULL)
		return plugd::Result(plugd::Result::ERROR, "Object " + destid + " not found");

	return dest->doAction(action, dispatcher);
}


void Commander::ControlMessageHandler::processReply(const Message& msg,
		const std::string& reply)
{
	std::string objID = sourceObject(reply);
	std::string port = sourcePort(reply);
	//std::cout << "Forwarding reply to object "<<objID<<" port "<<port<<std::endl;
	Object* obj = Object::find(objID);

	if (obj == NULL)
		std::cerr << "Commander::processReply: object " << objID
				<< " not found." << std::endl;
	else if (!obj->getClass()->derive("flowvr.plugins.Regulator"))
		std::cerr << "Commander::processReply: object " << objID
				<< " not a regulator." << std::endl;
	else
	{
		Regulator* module = (Regulator*) obj;
		module->processReplyMessage(msg, port, dispatcher);
	}
}

flowvr::plugd::GenClass<Commander> CommanderClass("flowvr.plugins.Commander", // name
		"" // description
		);

plugd::Class* Commander::getClass() const
{
	return &CommanderClass;
}

} // namespace plugins

} // namespace flowvr
