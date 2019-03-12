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
 * File: src/plugins/flowvr.plugins.Regulator.cpp                  *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/daemondata.h"
#include "flowvr/plugins/regulator.h"
#include "flowvr/plugins/logger.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/messagequeue.h"
#include "flowvr/moduleapi.h"
#include "flowvr/utils/hexdump.h"


#include <flowvr/mem/sharedmemoryarea.h>
#include <flowvr/mem/sharedmemorymanager.h>


#include <iostream>
#include <sstream>
#include <unistd.h>
#include <sys/time.h>

#include "flowvr/config.h"
#ifdef FLOWVR_HAVE_HWLOC
#include "hwloc.h"
#endif

namespace flowvr
{

namespace plugins
{

using namespace flowvr::xml;

/// Constructor.
Regulator::Regulator(const std::string &objID) :
	BaseObject(objID)
	, header(NULL)
	, threadDispatcher(NULL)
	, closed(false)
	, nbMessageToGo(0)
	, endWaitSignal("Regulator.endWaitSignal")
	, traceBeginIt(TypedTrace<int> ("beginIt"))
	, traceEndIt(TypedTrace<int> ("endIt"))
	, lastStatusTime(0.0)
	, lastStatusIt(0)
	, ports(NULL)
	, nbEventPorts(0)
{
}

Regulator::~Regulator()
{
	delete[] ports;
	delete threadDispatcher;
}

/// Initialization. Returns a XML document containing the result.
flowvr::plugd::Result Regulator::init(DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{
	flowvr::plugd::Result result = BaseObject::init(xmlRoot, dispatcher);
	if (result.error())
		return result;

	result.clear();

	std::string moduleName = name();
	std::string moduleParent = parentID();

	headerBuf = flowvr::daemon::getNewModule(moduleName, moduleParent);
	if (!headerBuf.valid())
		result = plugd::Result(flowvr::plugd::Result::ERROR, "Module "
				+ moduleParent + "/" + moduleName + " not found");
	else
	{
		header = headerBuf.getWrite<MPModuleDescription> (0);

		/// construct the input ports message queue

		nbInputs = 1; // first input port is system's beginIt

		// count the number of input ports (in total)
		for (int p = 0; p < header->nbPorts; p++)
		{
			if (header->ports[p].flags & MPPort::INPUT)
			{
				++nbInputs;
				if( header->ports[p].flags & MPPort::NONBLOCKING)
					++ nbEventPorts;
			}
		}

//		std::cerr << "# eventsource ports: " << nbEventPorts << std::endl;


		{
			initInputs(nbInputs);
			ports = new MPPortQueue[nbInputs];

			int i = 0;
			inputs[i]->setName("beginIt");
			ports[i].port = NULL;
			++i;
			for (int p = 0; p < header->nbPorts; p++)
				if (header->ports[p].flags & MPPort::INPUT)
				{
					MPPort* port = header->ports + p;
					inputs[i]->setName((std::string) port->name);
					ports[i].port = port;
					++i;
				}

			waitOnInput.resize(nbInputs, false);
		}

		///construct the outputmessage queue list

		nbOutputs = 1; // first output port is endIt

		for (int p = 0; p < header->nbPorts; p++)
			if (header->ports[p].flags & MPPort::OUTPUT)
				++nbOutputs;

		initOutputs(nbOutputs);

		int i = 0;

		{ // endIt
			outputs[i]->setName("endIt");
			outputs[i]->msgtype = Message::STAMPS; 
			outputs[i]->newStampSpecification(dispatcher);
		}

		for (int p = 0; p < header->nbPorts; p++)
		{
			if (header->ports[p].flags & MPPort::OUTPUT)
			{
				i++;
				outputs[i]->setName((std::string) header->ports[p].name);
				outputs[i]->msgtype =  Message::FULL;
				//give the Stamplist to the outputmessage queue
				DOMParser parser;
				std::string str = header->ports[p].stampList;
				parser.parseString(str.c_str());
				outputs[i]->stamps.updateFromXML(
						parser.getDocument()->RootElement());
				outputs[i]->newStampSpecification(dispatcher);
			}
		}

		result.setStatus(flowvr::plugd::Result::OK);
		result.setXML(getXmlDescription());

		threadDispatcher = dispatcher->threadCopy();
		if (start())
			detach();
	}

	outputTraces.push_back(&traceEndIt);
	outputTraces.push_back(&traceBeginIt);

	return result;
}

DOMNode* Regulator::buildPortDescription(const MPPort* p)
{
	DOMElement* node = new DOMElement("port");
	node->SetAttribute("id", (std::string) p->name);
	DOMElement* datatype = new DOMElement("datatype");
	node->LinkEndChild(datatype);
	datatype->LinkEndChild(new DOMText((std::string) p->dataType));

	std::istringstream instamps((std::string) p->stampList);
	DOMElement* stamplist = new DOMElement("stamplist");
	instamps >> *stamplist;
	node->LinkEndChild(stamplist);

	return node;
}

DOMNode* Regulator::moduleXmlDescription(const MPModuleDescription* header)
{
	DOMElement* result = new DOMElement("moduledescription");
	int i;
	result->SetAttribute("mid", header->name);

	std::string hostname = "127.0.0.1";
	// get hostname from daemon header
	flowvr::mem::SharedMemoryArea* shm =
			flowvr::mem::SharedMemoryManager::instance()->getMainMemoryArea();


	const MPDaemonHeader* dheader = (shm == NULL ? NULL :
									 shm->getRead<MPDaemonHeader> (shm->readHeader()));
	if (dheader != NULL)
		hostname = dheader->hostName;

	result->SetAttribute("host", hostname);

	DOMElement* input = new DOMElement("input");
	result->LinkEndChild(input);

	// add beginIt port
	{
		DOMElement* node = new DOMElement("port");
                node->SetAttribute("id", (std::string) "beginIt");
                // Bruno: beginit is a stamps port only.
                //              DOMElement* datatype = new DOMElement("datatype");
                //              node->LinkEndChild(datatype);
		// standard stamps
		StampList stamps;
		DOMNode* stamplist = stamps.generateXML();
		node->LinkEndChild(stamplist);
		input->LinkEndChild(node);
	}
	for (i = 0; i < header->nbPorts; i++)
		if (header->ports[i].flags & MPPort::INPUT)
			input->LinkEndChild(buildPortDescription(header->ports + i));

	DOMElement* output = new DOMElement("output");
	result->LinkEndChild(output);

	// add endIt port
	{
		DOMElement* node = new DOMElement("port");
		node->SetAttribute("id", (std::string) "endIt");
		DOMElement* datatype = new DOMElement("datatype");
		node->LinkEndChild(datatype);
		// standard stamps
		StampList stamps;
		DOMNode* stamplist = stamps.generateXML();
		node->LinkEndChild(stamplist);
		output->LinkEndChild(node);
	}
	for (i = 0; i < header->nbPorts; i++)
		if (header->ports[i].flags & MPPort::OUTPUT)
		{
			output->LinkEndChild(buildPortDescription(header->ports + i));
		}

	std::istringstream intraces((std::string) header->traceDesc);
	DOMElement* tracelist = new DOMElement("tracelist");
	intraces >> *tracelist;
	result->LinkEndChild(tracelist);

	return result;
}

xml::DOMNode* Regulator::getXmlDescription()
{
	return moduleXmlDescription(header);
}

void Regulator::processCmdClose(flowvr::MPModuleDescription::Cmd& cmd)
{
	header->status = StatusError;
	cmd.arg.clear();
}

void Regulator::processCmdInvalid(flowvr::MPModuleDescription::Cmd& cmd)
{
	std::cerr << "Regulator " << objectID() << ": Invalid command " << cmd.id
			<< std::endl;
	cmd.arg.clear();
}

void Regulator::processCmdWait(flowvr::MPModuleDescription::Cmd& cmd)
{
	if (header->status == StatusInit)
	{
		header->iteration = 0;
	}
	else if (header->status == StatusRun)
	{
		header->iteration++;
	}
	else
	{ // ERROR
		header->status = StatusError;
		return;
	}

	cmd.arg.clear();
	cmd.id = MPModuleDescription::Cmd::INVALID;

	if (header->iteration > 0)
	{   // we are in state StatusRun
		// creation of the message
		MessageWrite newmsg;
		newmsg.stamps.write(outputs[0]->stamps.it, header->iteration - 1);

		traceEndIt.write(header->iteration - 1);

		//give the message to the endIt OMQ
		outputs[0]->put(newmsg, threadDispatcher);
	}

	{
		ipc::ScopedMTLock locker(messageQueueLock(), "wait");

		header->status = StatusWait;
		for (int p = 0; p < header->nbPorts; p++)
			header->ports[p].current.clear();


		// initialize the expected number of messages to come in
		// with all the inputs available.
		int nbUserInputs = nbInputs-1;

		int nbBlockingUserPorts = nbUserInputs - nbEventPorts;

		nbMessageToGo = nbBlockingUserPorts + 1; // +1 for beginIt


		// special cases: to be honest... unclear to me...
		if(header->flags & MPModuleDescription::CONTROLLER )
		{
			if( nbInputs > 1 ) // controllers with inputs just wait for 1 message?
				nbMessageToGo = 1;

			if( nbOutputs > 1 )
				nbMessageToGo = 0; // controllers with outputs wait for no message?
		}


		for (int i = 0; i < nbInputs; i++)
		{
			waitOnInput[i] = true;
			Message m = inputs[i]->frontMsg();
			if (m.stamps.valid())
			{
				waitOnInput[i] = false;
				inputs[i]->eraseFront();
				endWaitOnInput(i, m);
			}
			else
			{
				if (inputs[i]->getNbActions() == 0
					&& !(header->flags & MPModuleDescription::CONTROLLER))
				{ // we don't wait for messages on inputs with no actions
				  // does that mean, we do not wait on unconnected inputs?
					waitOnInput[i] = false;
					MPPort* port = (ports[i].port ? ports[i].port : NULL);
					if(port)
					{
						bool bMsgOnEventPort = (port->flags & MPPort::NONBLOCKING) ? true : false;
						if(!bMsgOnEventPort)
							--nbMessageToGo; // only decrease in case of message on blocking port
					}
					else
						--nbMessageToGo;
				}
			}
		}

		// this condition seems to be a special purpose
		// implementation for controllers again?
		// otherwise: normal modules decrease nbMessageToGo to 0
		// and switch to EndWait (StatusRun) as a side effect
		// in endWaitForInput()

		if (nbMessageToGo == 0 && header->status == StatusWait)
		{
			// check for new stampList specifications
			{
				for (int i = 0; i < nbInputs; i++)
				{
					if ((ports[i].port != NULL) && !ports[i].stampsSpecif.empty())
					{
						if (flowvr::daemon::verboseLevel >= 1)
							std::cout << objectID() << ": "
									<< inputs[i]->getName()
									<< " stampList updated @ "
									<< header->iteration << std::endl;


						ports[i].port->stampListUpdateIt = header->iteration;
						ports[i].port->stampList = ports[i].stampsSpecif;
						ports[i].stampsSpecif.clear();
					}
				}
			}

			header->status = StatusRun;

			header->actions[header->actionChan.beginSend()].id
					= MPModuleDescription::Action::ENDWAIT;
			header->actionChan.endSend();
		}
		else
		{
			// wait for someone else to finish
			if (flowvr::daemon::verboseLevel >= 2)
			{
				if (nbMessageToGo == 0)
					std::cout << objectID() << ": not waiting." << std::endl;
				else
				{
					std::cout << objectID() << ": waiting for";
					for (int i = 0; i < nbInputs; i++)
						if (waitOnInput[i])
							std::cout << " " << inputs[i]->getName();
					std::cout << std::endl;
				}
			}

			while (header->status == StatusWait)
				// this will release global lock and re-acquire
				// once the wait() returns (hopefully)
				endWaitSignal.wait(messageQueueLock());
		}
	}
}

void Regulator::endWaitOnInput(int mqid, const Message& msg)
{
	MPPort* port = ports[mqid].port;

	if (port != NULL && port->current.stamps.empty())
		port->current = msg; // this forwards the message to the consumer

	if (nbMessageToGo > 0) // globally, this module needs more messages
	{
		// nbInputs-1 is the number of user ports (-1 for beginIt)
		if( ((nbInputs-1) && ((nbInputs-1) - nbEventPorts) == 0) ) // all ports are optional
		{
			// this is a special case:
			// there are user ports (e.g.: not only beginIt)
			// but all ports specified are non-blocking
			// see, whether beginIt() is connected
			// @todo is this the official way to do that?
			if( inputs[0]->getNbActions() == 0)
			{
				nbMessageToGo = 0; // clear the way...
			}
			else
			{
				// beginIt is connected, so just continue as if this was
				// a normal message drop
				if(!port)
					--nbMessageToGo;
				else
				{
					// at least one mandatory input
					bool bMsgOnEventPort = (port->flags & MPPort::NONBLOCKING) ? true : false;

					if( !bMsgOnEventPort ) // it's a message on a mandatory port
						--nbMessageToGo;   // one new message in a FIFO port, count-down				//--nbMessageToGo; // we assume this is a call from an incoming message
				}
			}
		}
		else
		{
			if(!port)
				--nbMessageToGo;
			else
			{
				// at least one mandatory input
				bool bMsgOnEventPort = (port->flags & MPPort::NONBLOCKING) ? true : false;

				if( !bMsgOnEventPort ) // it's a message on a mandatory port
					--nbMessageToGo;   // one new message in a FIFO port, count-down
			}
		}

		if (nbMessageToGo == 0) // all received for this module
		{
			{
				// check for new stampList specifications
				for (int i = 0; i < nbInputs; i++)
				{
					if ( (ports[i].port != NULL) && !ports[i].stampsSpecif.empty())
					{
						if (flowvr::daemon::verboseLevel >= 1)
							std::cout << objectID() << ": "
									<< inputs[i]->getName()
									<< " stampList updated @ "
									<< header->iteration << std::endl;
						ports[i].port->stampListUpdateIt = header->iteration;
						ports[i].port->stampList = ports[i].stampsSpecif;
						ports[i].stampsSpecif.clear();
					}
				}
			}
			header->status = StatusRun;
			header->actions[header->actionChan.beginSend()].id
					= MPModuleDescription::Action::ENDWAIT; // beginSend() is blocking

			header->actionChan.endSend();

			endWaitSignal.notifyAll();
		}
#ifdef DEBUG
		else
			std::cout << objectID()
			<< ": waiting for "
			<< nbMessageToGo
			<< " message(s)."
			<< std::endl;
#endif
	}
}

void Regulator::newMessageNotification(int mqid, int msgnum,
		const Message& msg, plugd::Dispatcher* dispatcher)
{
	// new message on beginIt
	if (mqid == 0)
	{
		int nummsg;
		msg.stamps.read(inputs[0]->getStampList().num, nummsg);
		traceBeginIt.write(nummsg);
	}

	// @todo: this seems to be a helper function...
	// the msg pump is sleeping (i.e. waiting) and it
	// still waits for inputs, so call endWaitOnInput()
	// upon the incoming of a new message.
	if (header->status == StatusWait
			&& msgnum == inputs[mqid]->frontNum()
			&& waitOnInput[mqid])
	{
		// we checked the state flag for StatusWait.
		// although this is flag polling, we assume that the regulator
		// thread is not modifying or reading the state vector right
		// now, so we mark this port as dirty
		waitOnInput[mqid] = false;

		// and do some of the work required, e.g. remove the msg from the
		// wait queue
		inputs[mqid]->eraseFront();

		// call the bulk function endWaitOnInput(),
		// we assume that the regulator thread is safely in its
		// barrier, which will be released at the end of the method
		endWaitOnInput(mqid, msg); // may release a barrier, but will not block
	}
}

void Regulator::newStampListSpecification(int mqid, const Message& msg,
		plugd::Dispatcher* dispatcher)
{
	StampListSpecification specif;
	std::string xml;
	if (msg.stamps.read(specif.spec, xml))
		ports[mqid].stampsSpecif = xml;
}

void Regulator::processCmdPut(flowvr::MPModuleDescription::Cmd& cmd)
{
	MessagePut newmsg = cmd.arg;
	cmd.arg.clear();
	if (!newmsg.stamps.valid())
		std::cerr << "Invalid stamps" << std::endl;
	if (!newmsg.data.valid())
		std::cerr << "Invalid data from " << objectID() << std::endl;
	if (!(header->flags & MPModuleDescription::CONTROLLER))
	{
        std::string source;
        if ( this->nbOutputs > 0 ) {
            // get the source of the message in its stamps
            newmsg.stamps.read( outputs[0]->stamps.source, source );
            // find the right OMQ to process the message
            for ( int i = 0  ;  i < this->nbOutputs  ;  i++ ) {
                if ( outputs[i]->getSource() == source ) {
                    outputs[i]->put( newmsg, threadDispatcher ); // to the OMQ
                    return;
                }
            }
        }
        static ipc::MTAtomicInt error_sent( 0 );
        if ( error_sent.compare_and_swap( 0, 1 ) )
            fprintf( stderr, "Error: Regulator: message source '%s' unknown. Some output port is probably registred in appy, yet not in your module code.", source.c_str() );
	}
	else
	{ // Control message: special handler
		threadDispatcher->processControlMessage(newmsg);
	}
}

void Regulator::processCmdStamp(flowvr::MPModuleDescription::Cmd& cmd)
{
	MessagePut newmsg = cmd.arg;
	cmd.arg.clear();
	if (!newmsg.data.valid())
		std::cerr << "Invalid stamplist from " << objectID() << std::endl;
	if (!(header->flags & MPModuleDescription::CONTROLLER))
	{
        if ( this->nbOutputs > 0 ) {
            std::string source, name;
            // get the source of the message in its stamps
            newmsg.stamps.read( outputs[0]->stamps.source, source );
            // keep the port name alone
            source = std::string( source, source.find_last_of(":")+1 );
            // find the port of which to change the stamplist
            for ( int p=0/*port*/, o=0/*output*/; p < header->nbPorts; p++ )
            {
                if ( header->ports[p].flags & MPPort::OUTPUT ) {
                    o++; // o==0 is for 'endit'
                    name = header->ports[p].name;
                    if ( name == source )
                    {
                        // save xml spec inside the MPPort MP-descriptor
                        header->ports[p].stampList = newmsg.data.getRead<char>();
                        // save xml spec inside the local MPPortQueue descriptor
                        ports[p].stampsSpecif = header->ports[p].stampList;
                        //give the Stamplist to the outputmessage queue itself
                        DOMParser parser;
                        parser.parseString( ports[p].stampsSpecif.c_str() );
                        outputs[o]->stamps.updateFromXML(
                                parser.getDocument()->RootElement());
                        outputs[o]->newStampSpecification(threadDispatcher);
                        return;
                    }
                }
            }
        }
        static ipc::MTAtomicInt error_sent( 0 );
        if ( error_sent.compare_and_swap( 0, 1 ) )
            fprintf( stderr, "Error: Regulator: processCmdStamp" );
	}
}

void Regulator::processCmdCpuset(flowvr::MPModuleDescription::Cmd& cmd) {
#ifndef FLOWVR_HAVE_HWLOC
    std::cout << "FlowVR didn't find hwloc at configuration time\n";
#else
    BufferWrite data;
    data.Buffer::operator=( cmd.arg.data );
    cmd.arg.clear();
    if ( ! data.valid()  &&  0 < data.getSize(0) ) {
		std::cerr << "Invalid core number from " << objectID() << std::endl;
    } else if ( !(header->flags & MPModuleDescription::CONTROLLER) ) {
        /* Allocate and initialize objects. */
        hwloc_topology_t topology;
        hwloc_topology_init( & topology );
        hwloc_topology_load( topology );
        hwloc_cpuset_t cpuset = hwloc_bitmap_alloc();
        /* deserialize cpuset */
        data.getWrite< char >()[ data.getSize(0)-1 ] = '\0'; // just in case
        const char * const str = data.getRead<char>();
        fprintf( stderr, "cpuset before treatment '%s'\n", str);
        if ( hwloc_bitmap_sscanf( cpuset, str ) ) {
            const int error = errno;
            fprintf( stderr, "Couldn't scanf cpuset '%s': %s\n", str, strerror(error));
        } else {
            /* Get ONE logical processor (in case it is SMT/hyperthreaded). */
            hwloc_bitmap_singlify( cpuset );
            /* And try to bind ourself there. */
            if ( hwloc_set_cpubind( topology, cpuset, 0 ) ) {
                const int error = errno;
                fprintf( stderr, "Couldn't bind to cpuset %s: %s\n", str, strerror(error));
            /* And some memory binding */
            } else if ( -1 == hwloc_set_area_membind( topology, 
                header,sizeof(header),  cpuset, HWLOC_MEMBIND_DEFAULT, 0 ) ){
                const int error = errno;
                fprintf( stderr, "Couldn't bind MPModuleDescription to cpuset %s: %s\n", str, strerror(error));
            }
            fprintf( stderr, "Bound regulator to cpuset '%s'\n", str);
        }
        hwloc_bitmap_free( cpuset );
        hwloc_topology_destroy(topology);
	}
#endif
}

void Regulator::processReplyMessage(const Message& msg, const std::string& port,
		plugd::Dispatcher* dispatcher)
{
	if (!(header->flags & MPModuleDescription::CONTROLLER))
		return; // ignore replies if not a controller

	for (int i = 0; i < nbInputs; i++)
		if (inputs[i]->getName() == port)
		{
			static StampList stamplist;
			// Send the message in the queue: add the right num stamp
			StampsWrite newstamps;
			newstamps.clone(msg.stamps, &stamplist);
			int num = inputs[i]->frontNum() + inputs[i]->size();
			newstamps.write(stamplist.num, num);
			Message newmsg;
			newmsg.stamps = newstamps;
			newmsg.data = msg.data;
			inputs[i]->doIt(newmsg, dispatcher);
		}
}

flowvr::plugd::Result Regulator::close(flowvr::xml::DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{
	{
		//taking lock
		ipc::ScopedMTLock locker(messageQueueLock(), "close");

		header->status = StatusError;
		header->actions[header->actionChan.beginSend()].id
				= MPModuleDescription::Action::ENDWAIT;
		header->actionChan.endSend();

		closed = true;

		endWaitSignal.notifyAll();
	}

	return BaseObject::close(xmlRoot, dispatcher);

}

/// Main thread function. Should be implemented by subclasses.
int Regulator::run()
{
  MPModuleDescription::Cmd* cmdTable =
    headerBuf.getWrite<MPModuleDescription::Cmd>(header->cmdOffset);

	{
		ipc::ScopedMTLock locker(messageQueueLock(), "wait");
		while (!isStarted())
		{
			endWaitSignal.wait(messageQueueLock());
		}
	}

	while (!closed)
	{
		int cmdnum = header->cmdChan.beginRead();
		flowvr::MPModuleDescription::Cmd& cmd = cmdTable[cmdnum];
		switch (cmd.id)
		{
		case flowvr::MPModuleDescription::Cmd::CLOSE:
			processCmdClose(cmd);
			closed = true;
			break;
		case flowvr::MPModuleDescription::Cmd::WAIT:
			processCmdWait(cmd);
			break;
		case flowvr::MPModuleDescription::Cmd::PUT:
			processCmdPut(cmd);
			break;
		case flowvr::MPModuleDescription::Cmd::STAMP:
			processCmdStamp(cmd);
			break;
        case flowvr::MPModuleDescription::Cmd::CPUSET:
			processCmdCpuset(cmd);
			break;
		default:
			processCmdInvalid(cmd);
			closed = true;
			break;
		}
		cmd.id = MPModuleDescription::Cmd::INVALID;
		header->cmdChan.endRead();

		if (!isStarted())
		{
			ipc::ScopedMTLock locker(messageQueueLock(), "wait");

			//while the regulator is stopped and no stop occurs
			while ((!isStarted()) && (!closed))
			{
				endWaitSignal.wait(messageQueueLock());
			}
		}
	}
	// close on the regulator
	// this should not be called here... better
	// someone else will collect the resources of
	// this thread...
	close(NULL, threadDispatcher);

	threadDispatcher->close();

	header->clear();

	return 0;
}

void Regulator::doStart(plugd::Dispatcher* dispatcher)
{
	// Connections might have changed.
	for (int i = 1; i < nbInputs; i++)
		if (inputs[i]->isConnected())
			ports[i].port->flags |= MPPort::CONNECTED;
		else
			ports[i].port->flags &= ~MPPort::CONNECTED;

	{
		int i = 0;
		for (int p = 0; p < header->nbPorts; p++)
		{
			if (header->ports[p].flags & MPPort::OUTPUT)
			{
				i++;
				if (outputs[i]->isConnected())
					header->ports[p].flags |= MPPort::CONNECTED;
				else
					header->ports[p].flags &= ~MPPort::CONNECTED;
			}
		}
	}

	endWaitSignal.notifyAll();
}

/// Execute an action in immediate mode.
flowvr::plugd::Result Regulator::doAction(DOMElement* xmlRoot,
		plugd::Dispatcher* dispatcher)
{ // this object doesn't implement any action
	xml::DOMElement* child = xmlRoot->FirstChildElement();
	if (child != NULL && !strcmp(child->getNodeName(), "traceStart"))
	{
		std::string name = child->Attribute("name");

		for (std::vector<Trace*>::iterator i = outputTraces.begin(); i
				!= outputTraces.end(); i++)
		{
			if ((*i)->getName() == name)
			{
				int id = atoi(child->Attribute("id"));
				std::string logger = child->Attribute("logger");
				BufferWrite buf = ((flowvr::plugins::Logger*) findRelative(
						logger))->getLogInfo();

				if (!(*i)->start(id, buf, child))
				{
					std::string errText = objectID() + ": Can't start trace ("
							+ name + ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}

				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}

		std::string loggername = child->Attribute("logger");
		if (loggername.empty() || loggername.at(0) != '/')
			loggername = ((std::string) header->parent) + "/" + loggername;
		flowvr::plugins::Logger* logger =
				(flowvr::plugins::Logger*) Object::findRelative(loggername);
		if (logger == NULL)
		{
			std::string msg = "Logger " + loggername + "not found";
			return plugd::Result(flowvr::plugd::Result::ERROR, msg.c_str());
		}
		int actnum = header->actionChan.beginSend();
		header->actions[actnum].id = MPModuleDescription::Action::DOACTION;
		header->actions[actnum].xml = xml::DOMWriter::toString(child);
		header->actions[actnum].arg = logger->getLogInfo();
		header->actionChan.endSend(true);
		header->actions[actnum].arg.clear();
		if (!header->actions[actnum].xml.valid())
			return plugd::Result(flowvr::plugd::Result::ERROR,
					"Unsupported action");
		std::string text = header->actions[actnum].xml;
		if (text.length() == 0)
		{
			return plugd::Result(flowvr::plugd::Result::ERROR, "Unknown error");
		}
		header->actions[actnum].xml.clear();
		DOMParser parser;
		if (parser.parseString(text.c_str()))
		{ // send as text
			return plugd::Result(plugd::Result::ERROR, text);
		}
		child->SetAttribute("from", objectID());
		return plugd::Result(flowvr::plugd::Result::OK, child);
	}
	else if (child != NULL && !strcmp(child->getNodeName(), "traceStop"))
	{
		std::string name = child->Attribute("name");

		for (std::vector<Trace*>::iterator i = outputTraces.begin(); i
				!= outputTraces.end(); i++)
		{
			if ((*i)->getName() == name)
			{
				if (!(*i)->stop(child))
				{
					std::string errText = objectID() + ": Can't stop trace (" + name
							+ ")";
					return plugd::Result(flowvr::plugd::Result::ERROR, errText);
				}

				child->SetAttribute("from", objectID());
				return plugd::Result(flowvr::plugd::Result::OK, child);
			}
		}

		int actnum = header->actionChan.beginSend();
		header->actions[actnum].id = MPModuleDescription::Action::DOACTION;
		header->actions[actnum].xml = xml::DOMWriter::toString(child);
		header->actions[actnum].arg.clear();
		header->actionChan.endSend(true);
		if (!header->actions[actnum].xml.valid())
			return plugd::Result(flowvr::plugd::Result::ERROR,
					"Unsupported action");
		std::string text = header->actions[actnum].xml;
		header->actions[actnum].xml.clear();
		DOMParser parser;
		if (parser.parseString(text.c_str()))
		{ // send as text
			return plugd::Result(plugd::Result::ERROR, text);
		}
		child->SetAttribute("from", objectID());
		return plugd::Result(flowvr::plugd::Result::OK, child);
	}
	else
		return BaseObject::doAction(xmlRoot, dispatcher);
	//return std::string("<result status=\"error\">No action</result>");
}

/// Create a one-line description of the current state
std::string Regulator::status() const
{
	std::string s = BaseObject::status();
	std::string pre;
	std::string post;
	ipc::ScopedMTLock locker(messageQueueLock(), "status");
	if (header != NULL && header->status == StatusWait)
	{
		pre = "\e[7m";
		post = "\e[0m";
	}
	else if (header != NULL && header->status == StatusRun)
	{
		pre = "\e[1m";
		post = "\e[0m";
	}
	if (header != NULL)
	{
		static int sec0 = 0;
		struct timeval tv;
		gettimeofday(&tv, NULL);
		if (sec0 == 0)
			sec0 = tv.tv_sec;
		double t = (tv.tv_sec - sec0) + tv.tv_usec * 0.000001;
		if (lastStatusIt > 0 && lastStatusIt < header->iteration)
		{
			double ips = (header->iteration - lastStatusIt) / (t
					- lastStatusTime);
			char buf[16];
			snprintf(buf, sizeof(buf), "(%.1f it/s)", ips);
			pre += buf;
		}
		lastStatusIt = header->iteration;
		lastStatusTime = t;
	}
	return pre + s + post;
}

flowvr::plugd::GenClass<Regulator> RegulatorClass("flowvr.plugins.Regulator", // name
		"" // description
		);

plugd::Class* Regulator::getClass() const
{
	return &RegulatorClass;
}

} // namespace plugins

} // namespace flowvr
