/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                         Base Libraries                          *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA and                                                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (FRE 2490). ALL RIGHTS RESERVED.                                *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
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
 * File: src/moduleapimultiprocessimpl.cpp                         *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/moduleapimultiprocessimpl.h"
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/mem/sharedmemorybuffer.h"
#include "flowvr/daemondata.h"
#include "flowvr/message.h"
#include "flowvr/stamp.h"

#ifdef FLOWVR_HAVE_HWLOC
#include "hwloc.h"
#endif

#include <unistd.h>
#include <iostream>
#include <sstream>

namespace flowvr
{

// ModuleAPI Implementation using IPC (Shared memory)
using namespace mem;

/// Mandatory virtual destructor.
ModuleAPIMultiProcessImpl::~ModuleAPIMultiProcessImpl()
{
	delete descBuf;

//	if(sharedArea)
//	{
//		SharedMemoryManager* shmman = SharedMemoryManager::instance();
//		shmman->detachMemoryArea(sharedArea, 0);
//	}

    if ( Allocator::the() )
        Allocator::the()->detach();
	
}

/// Initialize the module.
///
/// This connects to the daemon and register the module.
/// The ports are also initialized.
/// Design note: a std::vector is used as a container as
/// the list of ports is small and not modified after init.
/// Design note 2: ports are always passed by pointers as
/// they contain private fields modified by ModuleAPI.
int ModuleAPIMultiProcessImpl::init(const std::vector<Port*>& ports,
		const std::vector<Trace*>& traces)
{
	// Init traces
	outputTraces = traces;

	// Add Wait traces
	outputTraces.push_back(&traceWaitBegin);
	outputTraces.push_back(&traceWaitEnd);

	// Add Ports traces

	for (unsigned int i = 0; i < ports.size(); i++)
	{
		Trace* t = getPortTrace(ports[i]);
		outputTraces.push_back(t);
	}

	SharedMemoryManager* shmman = SharedMemoryManager::instance();
	int areaId = 0;
	if (!daemonInfo.empty())
		areaId = atoi(daemonInfo.c_str());
	if ( NULL == shmman || areaId == 0)
		return 0; // areaId missing

	// assign the shared area (local to this instance)
	sharedArea = shmman->openMemoryArea(areaId);
	if (sharedArea == NULL)
	{
		std::cerr << "ModuleAPI-MP: Connection to daemon failed." << std::endl;
		return 0;
	}

	int headpos = sharedArea->readHeader();

	if (*sharedArea->getRead<unsigned> (headpos) == 0xDEADADDE)
	{
		std::cerr << "ModuleAPI-MP: Daemon not running." << std::endl;

//		sharedArea->detach((int) getpid(), 0);
//		shmman->releaseMemoryArea(sharedArea->areaId);
		return 0;
	}
    
//    std::cerr << "\tmoduleapumulproc : getpid == " << getpid() << std::endl;
//	sharedArea->attach((int) getpid());

	daemonHeader = sharedArea->getWrite<MPDaemonHeader> (headpos);

	std::cout << "ModuleAPI-MP: Connected to daemon "
			<< (std::string) daemonHeader->daemonName << std::endl;

	int nbports = ports.size();
	int nbmaxcmd = nbports + 1;
	
	// Create a new shared segment for the module and its regulator
	const void* shm_p = NULL;
	size_t shm_size = 0;
	if ( getenv("FLOWVR_MODULE_OWNSHMEM") ) {
		if ( shmman->getNewAreaFromDaemon() == -1 ) {
			std::cerr << "Couldn't allocate a new shm segment for hwbind\n";
		} else {
			mem::SharedMemoryArea *area = shmman->getCurrentMemoryArea();
			shm_p = area->getMappingRead();
			shm_size = area->getSize();
		}
	}

	(*descBuf) = shmman->allocBuffer(sizeof(MPModuleDescription) + nbports
			* sizeof(MPPort) + (nbmaxcmd) * sizeof(MPModuleDescription::Cmd));

	if (!(*descBuf).valid())
	{
		std::cerr << "ModuleAPI-MP: buffer allocation failed." << std::endl;
//		sharedArea->detach((int) getpid(), 0);
//		shmman->releaseMemoryArea(sharedArea->areaId);
		sharedArea = NULL; // reset dangling pointer
		return 0;
	}

	desc()->init(moduleName, nbmaxcmd, nbports);
	desc()->versionMaj = 0; // released, but still no versioning...
	desc()->versionMin = 0;
	desc()->parent = parentInfo;
	desc()->flags = flags;
	if (moduleName.empty())
	{
		std::cout << "ModuleAPI-MP: Registering as controller." << std::endl;
		desc()->flags |= MPModuleDescription::CONTROLLER;
	}
	desc()->status = StatusInit;
	desc()->iteration = -1;
	int nboutputs = 0;
	for (int p = 0; p < nbports; p++)
	{
//		std::string flags;
		Port* port = ports[p];
		setPortModule(port);
		setPortPrivate(port, (void*) (long) p); // save the port ID
		desc()->ports[p].name = port->name;
		desc()->ports[p].flags = 0;
		if (port->isInput())
		{
			desc()->ports[p].flags |= MPPort::INPUT;
//			flags += " INPUT";

			InputPort *pIn = dynamic_cast<InputPort*>(port);
			if(pIn->isNonBlockingPort())
			{
				desc()->ports[p].flags |= MPPort::NONBLOCKING;
//				flags += " #";
			}

			inputPorts.push_back(port);
		}
		if (port->isOutput())
		{
			desc()->ports[p].flags |= MPPort::OUTPUT;
			++nboutputs; // compute the number of output ports
//			flags += " OUTPUT";
			outputPorts.push_back(port);
		}
		xml::DOMNode* stamplist = port->stamps->generateXML();
		desc()->ports[p].stampList = xml::DOMWriter::toString(stamplist);
		delete stamplist;
	}

	// Create Traces specifications

	xml::DOMElement* tracesXml = new xml::DOMElement("tracelist");

	for (unsigned int i = 0; i < outputTraces.size(); i++)
	{
		tracesXml->LinkEndChild(outputTraces[i]->xmlDesc());
	}

	desc()->traceDesc = xml::DOMWriter::toString(tracesXml);
	delete tracesXml;

	{ // Send a MODINIT command to the daemon
		int cmdnum = daemonHeader->cmdChan.beginSend();
		daemonHeader->cmdTable[cmdnum].id = MPDaemonHeader::Cmd::MODINIT;
		daemonHeader->cmdTable[cmdnum].arg = (*descBuf);
		daemonHeader->cmdChan.endSend();
	}
	
	// do actual binding using hwloc
	// done afterward to perform memory and regulator binding at once
	if ( desc()->status != StatusError  &&  getenv("FLOWVR_MODULE_HWBIND") ) {
		this->hwbind( shm_p, shm_size );
	}

	return desc()->status;
}



void ModuleAPIMultiProcessImpl::hwbind( const void * shm_p, size_t shm_size ) {
#ifndef FLOWVR_HAVE_HWLOC
	std::cout << "FlowVR didn't find hwloc at configuration time\n";
#else
	// Allocate and initialize objects
	hwloc_topology_t topology;
	hwloc_topology_init( & topology );
	hwloc_topology_load( topology );
	hwloc_cpuset_t cpuset = hwloc_bitmap_alloc();
	// get current binding
	if ( hwloc_get_cpubind( topology, cpuset, 0 ) ) {
		const int err = errno;
		fprintf( stderr, "ModuleAPI: Couldn't get current bind: %s\n", strerror(err));
	} else {
		hwloc_bitmap_singlify(cpuset);
		// bind memory area
		if ( shm_p != NULL ) {
			if ( hwloc_set_area_membind( topology, shm_p,shm_size, cpuset,
										 HWLOC_MEMBIND_DEFAULT, 0 ) ) {
				const int err = errno;
				fprintf( stderr, "Couldn't bind MPModuleDescription: %s\n", strerror(err));
			}
		}
		// Now, we bind the newly-created regulator on the same core than the module
        char * str = NULL;
        if ( hwloc_bitmap_asprintf( &str, cpuset ) ) {
            this->cpuset( str );
            free(str);
        } else {
            const int err = errno;
            fprintf( stderr, "ModuleAPI: Couldn't print scpuset: %s\n", strerror(err));
        }
	}
	hwloc_bitmap_free( cpuset );
	hwloc_topology_destroy(topology);
#endif
}


/// Get the module ID.
std::string ModuleAPIMultiProcessImpl::getID() const
{
	std::string source = parentInfo + "/" + moduleName;
	return source;
}

/// Get a port source ID.
std::string ModuleAPIMultiProcessImpl::getPortID(Port* port) const
{
	std::string source = parentInfo + "/" + moduleName;
	source += ":" + port->name;
	return source;
}

/// Check if a port is connected (only valid after the first wait).
bool ModuleAPIMultiProcessImpl::isPortConnected(const Port* port) const
{
	int portId = (int) (long) getPortPrivate(port);
	return desc()->ports[portId].flags & MPPort::CONNECTED;
}

size_t ModuleAPIMultiProcessImpl::getNumberOfInputPorts() const
{
	return inputPorts.size() + 1; // at least beginIt will be there for modules
}

size_t ModuleAPIMultiProcessImpl::getNumberOfOutputPorts() const
{
	return outputPorts.size() + 1; // at least endIt will be there for modules
}


bool ModuleAPIMultiProcessImpl::isBoundByInports() const
{
	for(std::vector<Port*>::const_iterator cit = inputPorts.begin(); cit != inputPorts.end(); ++cit )
	{
		InputPort *inp = static_cast<InputPort*>( *cit );
		if( inp->isNonBlockingPort() )
				continue;    // non-blocking ports do not bind the module

		if( isPortConnected(inp) )
			return true; // one blocking, connected port is enough to bind the module
	}
	return false; // no, this module is not bound by inports
}


Allocator *ModuleAPIMultiProcessImpl::getAllocator() const
{
	return Allocator::getAllocator();
}

/// Alloc a shared memory buffer
BufferWrite ModuleAPIMultiProcessImpl::alloc(size_t size)
{
	return getAllocator()->alloc(size);
}

/// Alloc a shared memory buffer and initialize it with the specified std::string
BufferWrite ModuleAPIMultiProcessImpl::allocString(const std::string& str)
{
	return ((SharedMemoryManager*)getAllocator())->allocString(str);
}

/// Realloc a shared memory buffer.
bool ModuleAPIMultiProcessImpl::realloc(BufferWrite &buffer, size_t size, bool amortized)
{
	if (buffer.valid())
		return buffer.resize(size, amortized);
	else
	{
		buffer = alloc(size);
		return buffer.valid();
	}
}

/// Wait for the next iteration.
///
/// This is the most important method of ModuleAPI.
/// It updates the messages available on the input ports
/// according to the conditions externally defined.
/// The point is that the module's implementation have
/// no knowledge of what these conditions are.
int ModuleAPIMultiProcessImpl::wait()
{
	if (getStatus() != 0)
	{
		traceWaitBegin.write(desc()->iteration + 1);

		int cmdnum = desc()->cmdChan.beginSend();
		MPModuleDescription::Cmd* cmd = (*descBuf).getWrite<MPModuleDescription::Cmd> (desc()->cmdOffset) + cmdnum;

		if (cmd->id != MPModuleDescription::Cmd::INVALID)
			std::cout << "ModuleAPI-MP: WARNING: module command entry "
					<< cmdnum << " not cleared." << std::endl;

		cmd->id = MPModuleDescription::Cmd::WAIT;

		desc()->cmdChan.endSend();

		bool endwait = false;
		bool closed = false;
		do
		{
			// blocking operation! wait for message to proceed on
			// action channel
			int actnum = desc()->actionChan.beginRead();

			// ok, got back control, let's see what happened.
			switch (desc()->actions[actnum].id)
			{
			case MPModuleDescription::Action::ENDWAIT:
				endwait = true; // finish, we are done with the wait,
				// probably all mandatory inports have
				// a message now.
				break;
			case MPModuleDescription::Action::DOACTION:
				desc()->actions[actnum].xml = doAction(
						(std::string) desc()->actions[actnum].xml,
						(BufferWrite) desc()->actions[actnum].arg);
				break;
			case MPModuleDescription::Action::CLOSE:
				closed = true; // control closed us, leave loop.
				break;
			default:
				desc()->actions[actnum].xml.clear();
			}
			desc()->actionChan.endRead();
		} while (!endwait && !closed);

		if (closed)
			return 0; // indicate application end to the outside world

		// check for new stamplist
		for (int p = 0; p < (int) inputPorts.size(); p++)
		{
			MPPort* mpport = desc()->ports + (int) (long) getPortPrivate(
					inputPorts[p]);
			if ((mpport->stampListUpdateIt == desc()->iteration)
					&& (!mpport->stampList.empty()))
			{
				std::string xml = mpport->stampList;
				xml::DOMParser parse;
				if (parse.parseString(xml.c_str()) == 0)
				{
					inputPorts[p]->stamps->updateFromXML(
							parse.getDocument()->RootElement());
				}
			}
		}
		traceWaitEnd.write(desc()->iteration);
	}
	return getStatus();
}

int ModuleAPIMultiProcessImpl::get(InputPort* port, Message& message)
{
	if (desc()->status == StatusRun)
	{
		int portId = (int) (long) getPortPrivate(port);
		if( portId >= 0 ) // valid?
		{
			message = desc()->ports[portId].current;
			desc()->ports[portId].current.clear();
			int nummsg = -1;
			if (message.stamps.valid())
				message.stamps.read(port->stamps->num, nummsg);
			getPortTrace(port)->write(nummsg);
		}
		else
		{
			std::cerr << "calling module::get() on an unassigned port ["
					  << (port ? port->name : "<null-port>") << std::endl;
		}
	}
	else
	{
		message.clear();
	}
	return getStatus();
}

/// Send a new message to an output port.
///
/// Note: The message buffer should not be modified by the module
/// after calling this method.
int ModuleAPIMultiProcessImpl::put(OutputPort* port, MessagePut& message)
{
	if (!message.data.valid())
	{
		std::cerr
				<< "ERROR: flowvr::ModuleAPI::put called with a stamps-only message on port "
				<< port->name << std::endl;
		message.data = alloc(0);
	}

	if (desc()->status == StatusRun || desc()->status == StatusInit)
	{
		int portId = (int) (long) getPortPrivate(port);
		int nummsg = desc()->ports[portId].nextnum;
		getPortTrace(port)->write(nummsg);

		// First complete the system-defined stamps
		std::string source;
		source = parentInfo + "/" + moduleName;
		source += ":" + port->name;
		message.stamps.write(port->stamps->source, source);
		message.stamps.write(port->stamps->num, nummsg);
		message.stamps.write(port->stamps->it, desc()->iteration);
		++desc()->ports[portId].nextnum;

		int cmdnum = desc()->cmdChan.beginSend();
		MPModuleDescription::Cmd* cmd = (*descBuf).getWrite<
				MPModuleDescription::Cmd> (desc()->cmdOffset) + cmdnum;
		if (cmd->id != MPModuleDescription::Cmd::INVALID)
			std::cout << "ModuleAPI-MP: WARNING: module command entry "
					<< cmdnum << " not cleared." << std::endl;

		cmd->id = MPModuleDescription::Cmd::PUT;

		cmd->arg.stamps = message.stamps;
		cmd->arg.data = message.data;

		desc()->cmdChan.endSend();
	}
    else if (desc()->status == StatusAbort)
    {
        std::cout << "ModuleAPI-MP: WARNING: called ModuleAPIMultiProcessImpl::put() while in StatusAbort:"
                  << " put canceled" << std::endl;
    }
    else
	{
		std::cerr
				<< "ERROR: during ModuleAPIMultiProcessImpl::put() -- getStatus() reported ("
				<< getStatus() << ")" << std::endl
				<< "forgot to call init()? broken pipe? ending the application non-gently?"
				<< std::endl;
	}
	return getStatus();
}

int ModuleAPIMultiProcessImpl::put(OutputPort* port, MessageWrite& message)
{
	MessagePut msg(message);
	int result = put(port, msg);
	message.stamps = msg.stamps;
	return result;
}

/// Send a new message to an output port.
///
/// Note: The message buffer should not be modified by the module
/// after calling this method.
int ModuleAPIMultiProcessImpl::put(OutputPort* port, const StampList *stamplist)
{
	if ( ! stamplist ) {
		std::cerr
				<< "ERROR: flowvr::ModuleAPI::put called with a NULL stamplist on port "
				<< port->name << std::endl;
        return getStatus();
	}

	if (desc()->status == StatusRun || desc()->status == StatusInit)
	{
        MessageWrite message;
        
		const int portId = (int) (long) getPortPrivate(port);
		const int nummsg = desc()->ports[portId].nextnum;
		//getPortTrace(port)->write(nummsg);
		// First complete the system-defined stamps
		std::string source;
		source = parentInfo + "/" + moduleName;
		source += ":" + port->name;
		message.stamps.write(port->stamps->source, source);
		message.stamps.write(port->stamps->num, nummsg);
		message.stamps.write(port->stamps->it, desc()->iteration);
		//++desc()->ports[portId].nextnum;
        
        xml::DOMNode* const xmlspec = stamplist->generateXML();
        const std::string strspec( xml::DOMWriter::toString( xmlspec ) );
        message.data = alloc( strspec.size() );
        if ( message.data.valid() ) {
            // update local stamps
            port->stamps->updateFromXML( xmlspec );
            // forward the update to the connected ports
            memcpy( message.data.writeAccess(), strspec.data(), strspec.size() );
            const int cmdnum = desc()->cmdChan.beginSend();
            {
            MPModuleDescription::Cmd* cmd = (*descBuf).getWrite<
                    MPModuleDescription::Cmd> (desc()->cmdOffset) + cmdnum;
            cmd->id = MPModuleDescription::Cmd::STAMP;
            cmd->arg.stamps = message.stamps;
            cmd->arg.data = message.data;
            if (cmd->id != MPModuleDescription::Cmd::INVALID)
                std::cout << "ModuleAPI-MP: WARNING: module command entry "
                        << cmdnum << " not cleared." << std::endl;
            }
            desc()->cmdChan.endSend();
        } else {
            std::cerr << "ERROR: allocation failed during ModuleAPIMultiProcessImpl::put()" << std::endl;
        }     
        delete xmlspec;
	}
	else
	{
		std::cerr
				<< "ERROR: during ModuleAPIMultiProcessImpl::put() -- getStatus() reported ("
				<< getStatus() << ")" << std::endl
				<< "forgot to call init()? broken pipe? ending the application non-gently?"
				<< std::endl;
	}
	return getStatus();
}




int ModuleAPIMultiProcessImpl::cpuset( const char* cpuset )
{
	if (desc()->status == StatusRun || desc()->status == StatusInit)
	{
        MessageWrite message;
        message.data = alloc( 1 + strlen(cpuset) );
        if ( message.data.valid() ) {
            memcpy( message.data.writeAccess(), cpuset, message.data.getSize());
            // forward the update to the connected ports
            const int cmdnum = desc()->cmdChan.beginSend();
            {
            MPModuleDescription::Cmd* cmd = (*descBuf).getWrite<
                    MPModuleDescription::Cmd> (desc()->cmdOffset) + cmdnum;
            cmd->id = MPModuleDescription::Cmd::CPUSET;
            if (cmd->id != MPModuleDescription::Cmd::INVALID)
                std::cout << "ModuleAPI-MP: WARNING: module command entry "
                        << cmdnum << " not cleared." << std::endl;
            cmd->arg = message;
            }
            desc()->cmdChan.endSend();
        } else {
            std::cerr << "ERROR: allocation failed during ModuleAPIMultiProcessImpl::put()" << std::endl;
        }     
	}
	else
	{
		std::cerr
				<< "ERROR: during ModuleAPIMultiProcessImpl::cpuset() -- getStatus() reported ("
				<< getStatus() << ")" << std::endl
				<< "forgot to call init()? broken pipe? ending the application non-gently?"
				<< std::endl;
	}
	return getStatus();
}


/// Abort the FlowVR application
void ModuleAPIMultiProcessImpl::abort() {
    int cmdnum = daemonHeader->cmdChan.beginSend();
    daemonHeader->cmdTable[cmdnum].id = MPDaemonHeader::Cmd::ABORT;
    daemonHeader->cmdTable[cmdnum].arg = (*descBuf);
    daemonHeader->cmdChan.endSend();
    desc()->status = StatusAbort;
}



/// Explicitly close the current module.
int ModuleAPIMultiProcessImpl::close()
{
	if (desc() != NULL)
	{
		/*
		 * send CLOSE to the daemon to indicate that we want to end processing
		 * and resources on the daemon side can be elminated.
		 */
		int cmdnum = desc()->cmdChan.tryBeginSend();
		if (cmdnum >= 0) // got a valid slot?
		{
			MPModuleDescription::Cmd* cmd
			= (*descBuf).getWrite<MPModuleDescription::Cmd> (desc()->cmdOffset) + cmdnum;

			if (cmd->id != MPModuleDescription::Cmd::INVALID)
				std::cout << "ModuleAPI-MP: WARNING: module command entry "
						  << cmdnum << " not cleared." << std::endl;

			cmd->id = MPModuleDescription::Cmd::CLOSE;
			desc()->cmdChan.endSend();
		}

		// we are not too sure who called close()... so send the CLOSE command
		// on the action channel as well, just in case we are waiting with
		// another thread here.
		int actnum = desc()->actionChan.tryBeginSend();
		if (actnum >= 0)
		{
			desc()->actions[actnum].id = MPModuleDescription::Action::CLOSE;
			desc()->actionChan.endSend();
		}

		// send mod exit
		/// @todo is this the right place to do that?

		{ // Send a MODEXIT command to the daemon
			int cmdnum = daemonHeader->cmdChan.beginSend();
			daemonHeader->cmdTable[cmdnum].id = MPDaemonHeader::Cmd::MODEXIT;
			daemonHeader->cmdTable[cmdnum].arg = (*descBuf);
			daemonHeader->cmdChan.endSend();
		}

		desc()->status = StatusError;
	}
	return getStatus();
}

/// Return the current state of this module.
///
/// Each of the other methods also returns the current status after completion.
/// The exact meaning of this have to be defined...
int ModuleAPIMultiProcessImpl::getStatus() const
{
	if (desc() == NULL)
		return 0;
	else
		return desc()->status;
}

/// Return the local host name.
std::string ModuleAPIMultiProcessImpl::getHostName() const
{
	if (daemonHeader == NULL)
		return "unknown";
	else
		return daemonHeader->hostName;
}

/// Generate a unique 64 bits ID.
ID ModuleAPIMultiProcessImpl::generateID()
{
	if (daemonHeader == NULL)
	{
		unsigned int hostID = 0x00000001;
		static ipc::MTAtomicInt countID = ((int) getpid()) << 16;
		unsigned int count = countID.exchange_and_add(1);
		return (static_cast<ID> (hostID) << 32) | static_cast<ID> (count);
	}
	else
	{
		return daemonHeader->generateID();
	}
}

Port *ModuleAPIMultiProcessImpl::getPortByName( const std::string &strName ) const
{
	for( std::vector<Port*>::const_iterator cit = inputPorts.begin();
	     cit != inputPorts.end(); ++cit )
	{
		if( (*cit)->name == strName )
			return (*cit);
	}

	for( std::vector<Port*>::const_iterator ocit = outputPorts.begin();
	     ocit != outputPorts.end(); ++ocit )
	{
		if( (*ocit)->name == strName )
			return (*ocit);
	}

	return NULL;
}

Trace *ModuleAPIMultiProcessImpl::getTraceByName( const std::string &strName ) const
{
	for( std::vector<Trace*>::const_iterator cit = outputTraces.begin();
	     cit != outputTraces.end(); ++cit )
	{
		if( (*cit)->getName() == strName )
			return (*cit);
	}

	return NULL;
}

// protected:

ModuleAPIMultiProcessImpl::ModuleAPIMultiProcessImpl(const std::string& modulename,
		const std::string& parentinfo, const std::string& daemoninfo, int flags)
: moduleName(modulename)
, parentInfo(parentinfo)
, daemonInfo(daemoninfo)
, flags(flags)
, traceWaitBegin(TypedTrace<int> ("waitBegin"))
, traceWaitEnd(TypedTrace<int> ("waitEnd"))
, daemonHeader(NULL)
, sharedArea(NULL)
, descBuf( new BufferWrite )
{
	if( !SharedMemoryManager::the() )
		Allocator::setAllocator( new SharedMemoryManager );
	Allocator::the()->attach();
}

using namespace xml;

std::string ModuleAPIMultiProcessImpl::doAction(std::string xmlText,
		BufferWrite arg)
{
	DOMParser parser;
	if (parser.parseString(xmlText.c_str()))
	{
		DOMElement* desc = new DOMElement("xmlparsingerror");
		desc->SetAttribute("id", parser.getDocument()->ErrorId());
		desc->LinkEndChild(new DOMText(parser.getDocument()->ErrorDesc()));
		std::string result = "<result status=\"ERROR\">" + DOMWriter::toString(
				desc) + "</result>";
		delete desc;
		return result;
	}
	else
	{
		DOMElement* root = parser.getDocument()->RootElement();
		if (!strcmp(root->getNodeName(), "traceStart"))
		{
			std::string name = root->Attribute("name");
			int id = atoi(root->Attribute("id"));
			for (unsigned int i = 0; i < outputTraces.size(); i++)
			{
				if (name == outputTraces[i]->getName())
				{
					if (!outputTraces[i]->start(id, arg, root))
					{
						std::string errText =
								"<result status=\"ERROR\">Trace to start ("
										+ name + ") not found</result>";
						return errText;
					}
					else
					{
						DOMElement* desc = outputTraces[i]->xmlDesc();
						desc->SetAttribute("from", moduleName);
						std::string result = "<result status=\"OK\">"
								+ DOMWriter::toString(desc) + "</result>";
						delete desc;
						return result;
					}
				}
			}
			return std::string("<result status=\"ERROR\">Trace ") + name
					+ std::string(" not found</result>");
		}
		else if (!strcmp(root->getNodeName(), "traceStop"))
		{
			std::string name = root->Attribute("name");
			for (unsigned int i = 0; i < outputTraces.size(); i++)
			{
				if (name == outputTraces[i]->getName())
				{
					if (!outputTraces[i]->stop(root))
					{
						std::string errText =
								"<result status=\"ERROR\">Trace to stop ("
										+ name + ") not found</result>";
						return errText;
					}
					else
						return "<result status=\"OK\">Stopped</result>";
				}
			}
			return std::string("<result status=\"ERROR\">Trace ") + name
					+ std::string(" not found</result>");
		}
		else
		{
			return std::string("<result status=\"ERROR\">Action ")
					+ root->getNodeName() + std::string(
					" not supported</result>");
		}
	}
}

} // namespace flowvr
