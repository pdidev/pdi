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
 * File: src/plugins/flowvr.plugins.BaseObject.cpp                   *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/plugins/baseobject.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/plugd/outputmessagequeue.h"
#include "flowvr/plugins/logger.h"
#include <iostream>
#include <sstream>
#include <unistd.h>


namespace flowvr
{

namespace plugins
{

using namespace flowvr::xml;

/// Constructor.
BaseObject::BaseObject(const std::string &objID) :
	Object(objID), globalLock("BaseObject.lock"), started(false), nbInputs(0),
			nbOutputs(0), inputs(NULL), outputs(NULL)
{
}

void BaseObject::initInputs(int nb)
{
	nbInputs = nb;
	if (nbInputs > 0)
	{
		inputs = new MyMessageQueue*[nb];
		for (int i = 0; i < nbInputs; i++)
		{
			inputs[i] = new MyMessageQueue();
			inputs[i]->parent = this;
			inputs[i]->id = i;
		}
	}
}

void BaseObject::initOutputs(int nb)
{
	nbOutputs = nb;
	outputs = new plugd::OutputMessageQueue*[nb];
	for (int i = 0; i < nbOutputs; i++)
	{
		outputs[i] = new plugd::OutputMessageQueue(objectID());
	}
}

int BaseObject::addInputs(int nb)
{
	if (nb <= 0)
		return 0;
	int i0 = nbInputs;
	nbInputs += nb;
	MyMessageQueue** oldinputs = inputs;
	inputs = new MyMessageQueue*[nbInputs];
	if (oldinputs != NULL)
	{
		for (int i = 0; i < i0; i++)
			inputs[i] = oldinputs[i];
		delete[] oldinputs;
	}
	for (int i = i0; i < nbInputs; i++)
	{
		inputs[i] = new MyMessageQueue();
		inputs[i]->parent = this;
		inputs[i]->id = i;
	}
	return i0;
}

int BaseObject::addOutputs(int nb)
{
	if (nb <= 0)
		return 0;
	int i0 = nbOutputs;
	nbOutputs += nb;
	plugd::OutputMessageQueue** oldoutputs = outputs;
	outputs = new plugd::OutputMessageQueue*[nbOutputs];
	if (oldoutputs != NULL)
	{
		for (int i = 0; i < i0; i++)
			outputs[i] = oldoutputs[i];
		delete[] oldoutputs;
	}
	for (int i = i0; i < nbOutputs; i++)
	{
		outputs[i] = new plugd::OutputMessageQueue(objectID());
	}
	return i0;
}

/// Initialization. Returns a XML document containing the result.
flowvr::plugd::Result BaseObject::init(DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = Object::init(xmlRoot, dispatcher);
	if (result.error())
		return result;
	return result;
}

/// Execute an action in immediate mode.
flowvr::plugd::Result BaseObject::doAction(DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{
	xml::DOMElement* child = xmlRoot->FirstChildElement();
	if (child != NULL && !strcmp(child->getNodeName(), "start"))
	{
		bool needstart = false;
		{
			ipc::ScopedMTLock locker(globalLock, "start");
			if (!started)
			{
				started   = true;
				needstart = true;
			}
		}
		if (needstart)
		{
			//call start method on each outputmessagequeue
			for (int i = 0; i < nbOutputs; i++)
			{
				outputs[i]->start(dispatcher);
			}

			doStart(dispatcher);
			return plugd::Result(flowvr::plugd::Result::OK, xmlRoot);
		}
		else
			return plugd::Result(flowvr::plugd::Result::ERROR,
					"Already started");
	}
	//pause the object
	//modification Loick
	else if (child != NULL && !strcmp(child->getNodeName(), "pause"))
	{
		if (!started)
			return plugd::Result(flowvr::plugd::Result::ERROR, "Not started");

		started = false;

		DOMElement* resultnode = new DOMElement("paused");
		for (int i = 0; i < nbOutputs; i++)
		{
			DOMElement* node = new DOMElement("messagesent");
			node->SetAttribute("source", outputs[i]->getSource());
			node->SetAttribute("count", outputs[i]->pause());
			resultnode->LinkEndChild(node);
		}
		return plugd::Result(flowvr::plugd::Result::OK, resultnode);
	}

	else if (child != NULL && !strcmp(child->getNodeName(), "traceStart"))
	{
		std::string name = child->Attribute("name");

		// We are searching an input message queue that has the same name than the trace to start
		for (int i = 0; i < nbInputs; i++)
		{
			if (inputs[i]->getName() == name)
			{
				// read trace id and name of its logger, then find this logger
				int id = atoi(child->Attribute("id"));
				std::string logger = child->Attribute("logger");
				BufferWrite buf = ((flowvr::plugins::Logger*) findRelative(
						logger))->getLogInfo();
				// send command start to the trace
				if (!inputs[i]->trace.start(id, buf, child))
				{
					std::string errText = objectID() + ": Can't start trace ("
							+ name + ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}

				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}
		// We are searching an output message queue that has the same name than the trace to start
		for (int i = 0; i < nbOutputs; i++)
		{
			if (outputs[i]->getName() == name)
			{
				if (flowvr::daemon::verboseLevel >= 1)
					std::cout << "Trace : " << name << " found." << std::endl;
				// read trace id and name of its logger, then find this logger
				int id = atoi(child->Attribute("id"));
				std::string logger = child->Attribute("logger");
				BufferWrite buf = ((flowvr::plugins::Logger*) findRelative(
						logger))->getLogInfo();
				// send command start to the trace
				if (!outputs[i]->trace.start(id, buf, child))
				{
					std::string errText = objectID() + ": Can't start trace ("
							+ name + ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}
				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}
		// no corresponding traces find
		std::string errText = objectID() + ": Trace to start (" + name
				+ ") not found";
		return plugd::Result(flowvr::plugd::Result::ERROR, errText);
	}

	else if (child != NULL && !strcmp(child->getNodeName(), "traceStop"))
	{
		std::string name = child->Attribute("name");

		for (int i = 0; i < nbInputs; i++)
		{
			if (inputs[i]->getName() == name)
			{
				if (!inputs[i]->trace.stop(child))
				{
					std::string errText = objectID() + ": Can't stop trace ("
							+ name + ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}

				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}

		for (std::vector<Trace*>::iterator i = outputTraces.begin(); i
				!= outputTraces.end(); i++)
		{
			if ((*i)->getName() == name)
			{
				if (!(*i)->stop(child))
				{
					std::string errText = objectID() + ": Can't stop trace ("
							+ name + ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}

				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}

		// no corresponding traces find
		std::string errText = objectID() + ": Trace to stop (" + name
				+ ") not found";
		return plugd::Result(flowvr::plugd::Result::ERROR, errText);
	}
	else
		return plugd::Result(flowvr::plugd::Result::ERROR, "Unsupported action");
}

/// Create an ActionHandler for batch mode action execution.
flowvr::plugd::ActionHandler* BaseObject::createAction(DOMElement* xmlRoot)
{
	std::string portName = xmlRoot->getTextContent();
	for (int i = 0; i < nbInputs; i++)
	{
		if (inputs[i]->getName() == portName)
			return inputs[i]->createAction(xmlRoot);
	}
	return NULL;
}

BaseObject::~BaseObject()
{
	if (nbInputs > 0)
	{
		for (int i = 0; i < nbInputs; i++)
			delete inputs[i];
		delete[] inputs;
	}

	if (nbOutputs > 0)
	{
		for (int i = 0; i < nbOutputs; i++)
			delete outputs[i];
		delete[] outputs;
	}

}

bool BaseObject::isStarted() const
{
	return started;
}

/// Create a one-line description of the current state
std::string BaseObject::status() const
{
	std::ostringstream sout;
	sout << Object::status();
	ipc::ScopedMTLock locker(globalLock, "status");
	if (!started)
		sout << "    PAUSED";
	if (nbInputs > 0)
	{
		sout << "    IN:";
		for (int i = 0; i < nbInputs; i++)
		{
			MyMessageQueue* p = inputs[i];
			if (p == NULL)
				continue;
			if (p->isConnected())
			{
				sout << ' ' << p->getName() << ':' << p->frontNum();
				if (!p->empty())
				{
					// the bounds here are arbitrarily chosen
					int nNum = p->backNum() - p->frontNum();
					if (nNum == 0)
					{
						// ignore
						continue;
					}
					else if (nNum >= 1 and nNum < 100)
					{
						sout << "\033[0;32m\033[1m"; // green bold
					}
					else if (nNum >= 100 and nNum < 1000)
					{
						sout << "\033[1;33m\033[1m"; // yellow bold
					}
					else if (nNum >= 1000)
					{
						sout << "\033[0;44m\033[1m"; // blue bold
					}

					sout << "(" << nNum << ")" << "\033[0m"; // reset
				}
			}
			else
			{
				sout << ' ' << p->getName();
			}
		}
	}
	if (nbOutputs > 0)
	{
		sout << "    OUT:";
		for (int i = 0; i < nbOutputs; i++)
		{
			plugd::OutputMessageQueue* p = outputs[i];
			if (p == NULL)
				continue;
			sout << ' ' << p->getName() << ':' << p->getNextNum();
		}
	}
	return sout.str();
}

} // namespace plugins

} // namespace flowvr
