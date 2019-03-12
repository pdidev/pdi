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
		                                    const std::string &modname)
{
	std::string daemonInfo = flowvr::DaemonInterface::getDeamonIDString();

	std::string parentInfo = readEnv("FLOWVR_PARENT");
	std::string modulename = readEnv("FLOWVR_MODNAME"); // environment overrides compilation setting
	if( modulename.empty() )
		modulename = modname;

	if (modulename.empty()) // could still be empty
	{
		std::cerr
				<< "ModuleAPI::registerModule: No module name specified."
				<< " Please use environment variable FLOWVR_MODNAME."
				<< std::endl;
		return NULL;
	}
	if (!instancename.empty())
	{
		modulename += "/";
		modulename += instancename;
	}
	std::cout << "FLOWVR Module name: " << modulename << std::endl;
	if (!parentInfo.empty())
	{
		std::cout << "FLOWVR Module parent: " << parentInfo << std::endl;
	}

	if (!strncmp(daemonInfo.c_str(), "file:", 5))
	{
		std::cout << "Using File-based Implementation."<< std::endl;
		return new ModuleAPIFileImpl(modulename, parentInfo, daemonInfo);
	}
	else
	{
		return new ModuleAPIMultiProcessImpl(modulename,parentInfo,daemonInfo);
	}
}

/// choose the appropriate ModuleAPI implementation for the new controller
ModuleAPI* ModuleAPIFactory::registerController(const std::string &instancename,
		                                        const std::string &hostnameIP)
{
	std::string daemonInfo = flowvr::DaemonInterface::getDeamonIDString();

	std::string parentInfo = readEnv("FLOWVR_PARENT");
	std::string modulename = readEnv("FLOWVR_MODNAME");


	if (modulename.empty())
	{
		char buf[1024];

		// If hostnameIP different from gethostname (default value) use the value of hostnameIP
		// else, simply use hostname returned by gethostname
		if (hostnameIP.compare("gethostname") != 0)
		{
			buf[sizeof(buf)-1]='\0';
			modulename = "";
			modulename += hostnameIP;
			modulename += "/";
			snprintf(buf, sizeof(buf)-1,"%d",getpid());
			buf[sizeof(buf)-1]='\0';
			modulename += buf;
		}
		else if (!gethostname(buf,sizeof(buf)-1))
		{
			buf[sizeof(buf)-1]='\0';
			modulename = "";
			modulename += buf;
			modulename += "/";
			snprintf(buf,sizeof(buf)-1,"%d",getpid());
			buf[sizeof(buf)-1]='\0';
			modulename += buf;
		}

		//std::cerr<<"No module name specified. Please use environment variable FLOWVR_MODNAME"<<std::endl;
					//return NULL;
	}

	if (!instancename.empty())
	{
	    modulename += "/";
	    modulename += instancename;
	}


	std::cout << "FLOWVR Module name: " << modulename << std::endl;

	if (!parentInfo.empty())
	  std::cout << "FLOWVR Module parent: " << parentInfo << std::endl;


	// Well currently we only support one implementation
	return new ModuleAPIMultiProcessImpl(modulename,
			                             parentInfo,
			                             daemonInfo,
			                             MPModuleDescription::CONTROLLER);
  }

} // namespace flowvr
