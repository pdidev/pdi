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
 * File: src/moduleapifileimpl.cpp                                 *
 *                                                                 *
 * Contacts:                                                       *
 *  03/18/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/moduleapifileimpl.h"
#include "flowvr/message.h"
#include "flowvr/mem/memorymanager.h"

#include <unistd.h>
#include <iostream>
#include <sstream>

namespace flowvr
{

// ModuleAPI Implementation using Files

ModuleAPIFileImpl::ModuleAPIFileImpl(const std::string& modulename,
				     const std::string& parentinfo,
				     const std::string& daemoninfo,
				     int flags)
: moduleName(modulename), parentInfo(parentinfo), daemonInfo(daemoninfo),
			flags(flags), status(0), it(-1)
{
	if( !Allocator::the() )
		Allocator::setAllocator(new mem::MemoryManager);
	Allocator::the()->attach();
}

/// Mandatory virtual destructor.
ModuleAPIFileImpl::~ModuleAPIFileImpl()
{
	Allocator::the()->detach();
}

/// Initialize the module.
int ModuleAPIFileImpl::init(const std::vector<Port*>& ports, const std::vector<Trace*>& traces)
{

  std::string filedir(daemonInfo,5);
  if (!filedir.empty() && filedir.at(filedir.length()-1)!='/') filedir += '/';

  std::vector<std::string> args;
  // Enumerate all arguments in parentInfo
  {
    const char* delimiters = " \t\n";
    std::string::size_type n = parentInfo.length();
    std::string::size_type start = parentInfo.find_first_not_of(delimiters);
    while (start < n)
    {
      std::string::size_type stop = parentInfo.find_first_of(delimiters, start);
      if (stop > n) stop = n;
			args.push_back(parentInfo.substr(start, stop - start));
			start = parentInfo.find_first_not_of(delimiters, stop + 1);
		}
	}

	// Init ports private info and module
	for (unsigned int p = 0; p < ports.size(); p++)
	{
		setPortPrivate(ports[p], NULL);
		setPortModule(ports[p]);
	}

	bool raw = false;
	bool repeat = false;

	for (unsigned int i = 0; i < args.size(); i++)
	{
		if (args[i] == "-r")
		{
			repeat = true;
		}
		else if (args[i] == "-raw")
		{
			raw = true;
		}
		else
		{
			std::string::size_type eqpos = args[i].find('=');
			std::string portname;
			std::string filename;
			if (eqpos != std::string::npos)
			{
				portname.assign(args[i], 0, eqpos);
				filename.assign(args[i], eqpos + 1, args[i].length());
			}
			else
			{
				portname = args[i];
				filename = args[i];
			}

			if (filename.at(0) != '/')
				filename = filedir + filename;

			// find the corresponding module port
			unsigned int p = 0;
			while (p < ports.size() && ports[p]->name != portname)
				++p;
			if (p >= ports.size())
			{
				std::cerr << "WARNING: Port " << portname
						<< " associated with file " << filename
						<< " not used by the application." << std::endl;
			}
			else if (ports[p]->isInput())
			{
				std::cout << "Connecting input port " << portname
						<< " using file " << filename << std::endl;
				InputPortData* data = new InputPortData((InputPort*) ports[p],
						filename, repeat);
				setPortPrivate(data->port, data);
				inputPorts.push_back(data);
			}
			else if (ports[p]->isOutput())
			{
				std::cout << "Connecting output port " << portname
						<< " using file " << filename << std::endl;
				OutputPortData* data = new OutputPortData(
						(OutputPort*) ports[p], filename, repeat);
				if (!data->writer.init(data->port->stamps))
				{
					std::cerr << "Port " << portname << " init failed."
							<< std::endl;
					return StatusError;
				}
				setPortPrivate(data->port, data);
				outputPorts.push_back(data);
			}
			else
			{
				std::cerr << "Invalid port " << portname << std::endl;
			}
		}
	}

	// Display list of unconnected ports
	{
		std::string freeInputPorts;
		std::string freeOutputPorts;
		for (unsigned int p = 0; p < ports.size(); p++)
		{
			if (getPortPrivate(ports[p]) == NULL)
			{
				if (ports[p]->isInput())
				{
					freeInputPorts += ' ';
					freeInputPorts += ports[p]->name;
				}
				if (ports[p]->isOutput())
				{
					freeOutputPorts += ' ';
					freeOutputPorts += ports[p]->name;
				}
			}
		}
		if (!freeInputPorts.empty())
			std::cout << "Unconnected Input Ports:" << freeInputPorts
					<< std::endl;
		if (!freeInputPorts.empty())
			std::cout << "Unconnected Output Ports:" << freeOutputPorts
					<< std::endl;
	}
	status = StatusInit;
	return getStatus();
}

void ModuleAPIFileImpl::abort() {
    std::cerr << "ModuleAPI::abort() method unavailable with ModuleAPIFileImpl.\n";
}

/// Get the module ID.
std::string ModuleAPIFileImpl::getID() const
{
	return moduleName;
}

/// Get a port source ID.
std::string ModuleAPIFileImpl::getPortID(Port* port) const
{
	std::string source = moduleName;
	source += ":";
	source += port->name;
	return source;
}

/// Check if a port is connected (only valid after the first wait).
bool ModuleAPIFileImpl::isPortConnected(const Port* port) const
{
	return getPortPrivate(port) != NULL;
}

bool ModuleAPIFileImpl::isBoundByInports() const
{
	for( std::vector<InputPortData*>::const_iterator cit = inputPorts.begin(); cit != inputPorts.end(); ++cit )
	{
		InputPort *port = (*cit)->port;
		if( port->isNonBlockingPort() )
				continue;

		if( isPortConnected( port ) )
			return true; // one port is enough
	}
	return false;// no connected port -> this module is not bound by inports
}



size_t ModuleAPIFileImpl::getNumberOfInputPorts() const
{
	return inputPorts.size() + 1; // at least beginIt will be there for modules
}

size_t ModuleAPIFileImpl::getNumberOfOutputPorts() const
{
	return outputPorts.size() + 1; // at least endIt will be there for modules
}


Allocator *ModuleAPIFileImpl::getAllocator() const
{
	return mem::MemoryManager::instance();
}


/// Alloc a shared memory buffer
BufferWrite ModuleAPIFileImpl::alloc(size_t size)
{
	return mem::MemoryManager::instance()->alloc(size);
}

/// Alloc a shared memory buffer and initialize it with the specified std::string
BufferWrite ModuleAPIFileImpl::allocString(const std::string& str)
{
	BufferWrite buf = alloc(str.size());
	if (str.size() > 0 && buf.writeAccess() != NULL)
		memcpy(buf.writeAccess(), str.c_str(), str.size());
	return buf;
}

/// Realloc a shared memory buffer.
bool ModuleAPIFileImpl::realloc(BufferWrite &buffer, size_t size, bool amortized)
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
int ModuleAPIFileImpl::wait()
{
	if (getStatus() != 0)
	{
		++it;
		status = StatusRun;
		if (it == 0)
		{ // open input files
			for (unsigned int i = 0; i < inputPorts.size(); i++)
			{
				InputPortData* data = inputPorts[i];
				if (!data->reader.init(data->port->stamps))
				{
					std::cerr << "Port " << data->port->name << " init failed."
							<< std::endl;
					status = StatusError;
				}
			}
		}
		for (unsigned int i = 0; i < inputPorts.size(); i++)
		{
			InputPortData* data = inputPorts[i];
			if (it == 0 || !data->repeat)
			{
				MessageWrite msg;
				if (!data->reader.read(msg, this->getAllocator()))
				{
					status = StatusError;
				}
				data->msg = msg;
			}
		}
	}
	return getStatus();
}

/// Get the current message available on an input port.
int ModuleAPIFileImpl::get(InputPort* port, Message& message)
{
	message.clear();
	if (status == StatusRun)
	{
		InputPortData* data = (InputPortData*) getPortPrivate(port);
		if (data != NULL) {
			message = data->msg;
			data->msg.clear();
		}
	}
	return getStatus();
}

/// Send a new message to an output port.
///
/// Note: The message buffer should not be modified by the module
/// after calling this method.
int ModuleAPIFileImpl::put(OutputPort* port, MessagePut& message)
{
	if (status == StatusRun || status == StatusInit)
	{
		OutputPortData* data = (OutputPortData*) getPortPrivate(port);
		if (data != NULL)
		{
			// First complete the system-defined stamps
			int nummsg = data->nextnum++;
			std::string source = getPortID(port);
			message.stamps.write(port->stamps->source, source);
			message.stamps.write(port->stamps->num, nummsg);
			message.stamps.write(port->stamps->it, it);
			data->writer.write(message);
		}
	}
	return getStatus();
}

int ModuleAPIFileImpl::put(OutputPort* port, MessageWrite& message)
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
int ModuleAPIFileImpl::put(OutputPort* port, const StampList *stamplist)
{
	if ( ! stamplist ) {
		std::cerr
				<< "ERROR: flowvr::ModuleAPI::put called with a NULL stampliston port "
				<< port->name << std::endl;
        return getStatus();
	}

    OutputPortData* data = (OutputPortData*) getPortPrivate(port);
	if ( (status == StatusRun || status == StatusInit) && data )
	{
        MessageWrite message;
        // First complete the system-defined stamps
        int nummsg = data->nextnum; // do NOT increment
        std::string source = getPortID(port);
        message.stamps.write(port->stamps->source, source);
        message.stamps.write(port->stamps->num, nummsg);
        message.stamps.write(port->stamps->it, it);
        // perform stamp update
        xml::DOMNode* xmlspec = stamplist->generateXML();
        const std::string strspec( xml::DOMWriter::toString( xmlspec ) );
        message.data = alloc( strspec.size() );
        if ( message.data.valid() ) {
            port->stamps->updateFromXML( xmlspec );
            memcpy( message.data.writeAccess(), strspec.data(), strspec.size() );
            data->writer.write(message);
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


/// Explicitly close the current module.
///
/// This is automatically done when calling the destructor.
int ModuleAPIFileImpl::close()
{
	for (unsigned int i = 0; i < inputPorts.size(); i++)
		inputPorts[i]->reader.close();
	for (unsigned int i = 0; i < outputPorts.size(); i++)
		outputPorts[i]->writer.close();
	status = 0;
	return getStatus();
}

/// Return the current state of this module.
///
/// Each of the other methods also returns the current status after completion.
/// The exact meaning of this have to be defined...
int ModuleAPIFileImpl::getStatus() const
{
	return status;
}

/// Return the local host name.
std::string ModuleAPIFileImpl::getHostName() const
{
	return "localhost";
}

/// Generate a unique 64 bits ID.
ID ModuleAPIFileImpl::generateID()
{
	unsigned int hostID = 0x00000001;
	static ipc::MTAtomicInt countID = ((int) getpid()) << 16;
	unsigned int count = countID.exchange_and_add(1);
	return (static_cast<ID> (hostID) << 32) | static_cast<ID> (count);
}

Port *ModuleAPIFileImpl::getPortByName( const std::string &strName ) const
{
	for( std::vector<InputPortData*>::const_iterator cit = inputPorts.begin();
	     cit != inputPorts.end(); ++cit )
	{
		if( (*cit)->port->name == strName )
			return (*cit)->port;
	}

	for( std::vector<OutputPortData*>::const_iterator ocit = outputPorts.begin();
	     ocit != outputPorts.end(); ++ocit )
	{
		if( (*ocit)->port->name == strName )
			return (*ocit)->port;
	}

	return NULL;
}

Trace *ModuleAPIFileImpl::getTraceByName( const std::string &strName ) const
{
	//The traces are not kept in this implementation
	return NULL;
}


} // namespace flowvr
