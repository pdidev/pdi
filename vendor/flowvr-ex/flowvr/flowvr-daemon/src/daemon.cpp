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
* File: src/daemon.cpp                                            *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/daemon.h"
#include "flowvr/daemondata.h"
#include "flowvr/ipc/mtsignal.h"
#include "flowvr/ipc/mtlock.h"
#include <iostream>
#include <map>
#include <algorithm>
#include "flowvr/utils/tcptools.h"

namespace flowvr
{

namespace daemon
{

typedef std::map<std::string,BufferWrite> ModulesRegistry;

namespace // hinder external visibility a bit
{
	ModulesRegistry       newModules;
	flowvr::ipc::MTLock   newModulesLock("flowvr::daemon::newModulesLock");
	flowvr::ipc::MTSignal newModulesSignal("flowvr::daemon::newModulesSignal");
}

/// Get the pointer to a "new" module (new meaning registered but not yet assigned to any regulator).
/// This function also removes the module from the list of new modules.
BufferWrite getNewModule(std::string name, std::string parent, bool wait)
{
  flowvr::ipc::ScopedMTLock locker(newModulesLock,"flowvr::daemon::getNewModule");
  std::string moduleName = parent+"/"+name;
  ModulesRegistry::iterator it;
  it = newModules.find(moduleName);
  if (wait)
  {
    while (it == newModules.end())
    {
      newModulesSignal.wait(newModulesLock);
      it = newModules.find(moduleName);
    }
  }
  if (it == newModules.end())
    return BufferWrite(); // module not found
  BufferWrite buf = it->second; // get the modules
  newModules.erase(it); // and remove it from the registry
  return buf; // return the result (and release the lock)
}

/// Register a new module (well, you can read the function name too ;))
void registerNewModule(BufferWrite moduleBuffer)
{
  MPModuleDescription* moduleDesc =
			moduleBuffer.getWrite<MPModuleDescription> (0);
        assert(moduleDesc); 
	std::string moduleName = (std::string) moduleDesc->parent + "/"
			+ (std::string) moduleDesc->name;

	std::cout << "daemon: Registering new module " << moduleName << std::endl;
	std::cout << "  Input ports:";
	for (int i = 0; i < moduleDesc->nbPorts; i++)
	{
		if (moduleDesc->ports[i].flags & MPPort::INPUT)
			std::cout << ' ' << (std::string) moduleDesc->ports[i].name;
		if(moduleDesc->ports[i].flags & MPPort::NONBLOCKING)
			std::cout << "$";
	}
	std::cout << std::endl;
	std::cout << "  Output ports:";
	for (int i = 0; i < moduleDesc->nbPorts; i++)
		if (moduleDesc->ports[i].flags & MPPort::OUTPUT)
			std::cout << ' ' << (std::string) moduleDesc->ports[i].name;
	std::cout << std::endl;
	{
		flowvr::ipc::ScopedMTLock locker(newModulesLock,
				"flowvr::daemon::registerNewModule");
		ModulesRegistry::iterator it = newModules.find(moduleName);
		if (it != newModules.end())
		{
			std::cerr << "daemon: Found duplicate module while registering "
					<< moduleName << ". Removing old module." << std::endl;
			newModules.erase(it);
		}
		newModules.insert(make_pair(moduleName, moduleBuffer));
		newModulesSignal.notifyAll();
	}
}


// Program arguments. Might be used by network plugins but this is UNSUPPORTED.

int verboseLevel = 0;

} // namespace daemon

} // namespace flowvr
