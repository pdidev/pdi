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
 * File: src/moduleapifactory.cpp                                  *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/moduleapifactory.h"
#include "flowvr/moduleapi.h"
#include "flowvr/moduleapimultiprocessimpl.h"
#include "flowvr/moduleapifileimpl.h"
#include "flowvr/daemondata.h"

#include <cstdlib>
#include <iostream>
#include <string>
#include <unistd.h>


namespace
{
	std::string readEnv(const char* envname)
	{
		const char* val = getenv(envname);
		if (val==NULL)
			val="";

		return std::string(val);
	}
}


namespace flowvr
{


/// choose the appropriate ModuleAPI implementation for the new module
ModuleAPI* ModuleAPIFactory::registerModule(const std::string &instancename,
                                            const std::string &modulename,
                                            const std::string &parentname)
{
	std::string daemonInfo = flowvr::DaemonInterface::getDeamonIDString();
	std::string parentInfo;
	std::string modname;

        // input args overwrite env var


	modname = modulename;
	if( modname.empty() )
            modname = readEnv("FLOWVR_MODNAME");

	parentInfo = parentname;        
        if( parentInfo.empty() )
	  parentInfo = readEnv("FLOWVR_PARENT");

	std::cout << "FLOWVR Module name: " << modname << std::endl;
	std::cout << "FLOWVR Parent  name: " << parentInfo << std::endl;

	
	if (modname.empty()) // could still be empty
	{
		std::cerr
				<< "ModuleAPI::registerModule: No module name specified."
				<< " Please use environment variable FLOWVR_MODNAME."
				<< " or provide module name and parent name through the initModule call"
				<< std::endl;
		return NULL;
	}
	if (!instancename.empty())
	{
		modname += "/";
		modname += instancename;
	}
	//std::cout << "FLOWVR Module name: " << modname << std::endl;



	if (parentInfo.empty())
	  {
	    std::cerr
	      << "ModuleAPI::registerModule: No parent  name specified."
	      << " Please use environment variable FLOWVR_PARENT."
	      << " or provide module name and parent name through the initModule call"
	      << std::endl;
	    return NULL;
	  }

	//	std::cout << "FLOWVR Module parent: " << parentInfo << std::endl;


	if (!strncmp(daemonInfo.c_str(), "file:", 5))
	{
		std::cout << "Using File-based Implementation."<< std::endl;
		return new ModuleAPIFileImpl(modname, parentInfo, daemonInfo);
	}
	else
	{
		return new ModuleAPIMultiProcessImpl(modname,parentInfo,daemonInfo);
	}
}


ModuleAPI* ModuleAPIFactory::registerController(const std::string &parentname,
                                                const std::string &modulename,
                                                const std::string &instancename)

{
  // Controllers do not behave like modules:  their name integrate directly the parent prefix.
  // The FLOWVR_PARENT value is build from the controller name (telnet.cpp)
  
	std::string daemonInfo = flowvr::DaemonInterface::getDeamonIDString();
	std::string modname;
	
	modname =parentname+"/"+modulename;
	if (!instancename.empty())
	{
	    modname += "/";
	    modname += instancename;
	}


	if ( parentname.empty() || modulename.empty())
	  {
	    std::cerr
	      << "ModuleAPI::registerController: module name or parent name missing"
	      << std::endl;
	    return NULL;
	  }

	std::cout << "FLOWVR Controller: " << modname << std::endl;


	return new ModuleAPIMultiProcessImpl(modname,
                                             "",
                                             daemonInfo,
                                             MPModuleDescription::CONTROLLER);
  }

} // namespace flowvr
