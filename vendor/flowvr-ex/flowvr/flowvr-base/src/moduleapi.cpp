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
* File: src/moduleapi.cpp                                         *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/moduleapi.h"
#include "flowvr/moduleapifactory.h"
#include "flowvr/parallel.h"
#include "flowvr/stamp.h"
#include <unistd.h>
#include <stdio.h>
#include <fstream>
#include <iostream>
#include <time.h>
#include <string.h>
#include <sstream>
#include <sys/stat.h>
//#include <stdlib.h>

#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/moduleapimultiprocessimpl.h"
#include "ftl/convert.h"

#include "flowvr/config.h"

namespace flowvr
{

// ############################################################################
// PORTS
// ############################################################################
Port::Port(const std::string& myname, StampList* mystamps, bool bOwn)
: name(myname)
, stamps(mystamps)
, module(NULL)
, privateInfo((void*)~0) // invalidate
, trace(TypedTrace<int>(myname))
, stampsOwned(false)
{
	if (stamps == NULL)
	{
		stamps = new StampList;
		stampsOwned = true;
	}
	else
	{
		// check for the "owns" flag for later cleanup
		stampsOwned = bOwn;
	}
}

Port::~Port()
{
	if(stampsOwned)
		delete stamps;
}

/// Check if this port is connected (only valid after the first wait).
bool Port::isConnected() const
{
	ModuleAPI* module = getModule();
	if (module == NULL)
		return false;
	else
		return module->isPortConnected(this);
}

InputPort::InputPort(const std::string& myname, StampList* mystamps, bool bOwn,
					 bool bIsNonBlockingPort)
: Port(myname, mystamps, bOwn)
, isNonBlocking(bIsNonBlockingPort)
{
}

OutputPort::OutputPort(const std::string& myname, StampList* mystamps, bool bOwn) :
	Port(myname, mystamps, bOwn)
{
}

/// Returns true.
bool InputPort::isInput() const
{
	return true;
}

/// Returns false.
bool InputPort::isOutput() const
{
	return false;
}

bool InputPort::isNonBlockingPort() const
{
	return isNonBlocking;
}

void InputPort::setNonBlockingFlag(bool bBlock)
{
	isNonBlocking = bBlock;
}

/// Returns false.
bool OutputPort::isInput() const
{
	return false;
}

/// Returns true.
bool OutputPort::isOutput() const
{
	return true;
}


// ############################################################################
// moduleAPI
// ############################################################################

ModuleAPI::ModuleAPI()
//: topology(false)
{
    //	hwloc_bitmap_zero(cpumask);
}

/// Mandatory virtual destructor.
ModuleAPI::~ModuleAPI()
{
}

// hwloc_cpuset_t ModuleAPI::getCPUMask() const
// {
// 	return cpumask;
// }

// bool      ModuleAPI::setCPUMask(hwloc_cpuset_t mask, const Topo &topo)
// {
// 	if(topo.getIsValid())
// 		topology = topo;

// 	if(hwloc_set_cpubind(topology.topology.t_topology, mask, 0 ) == 0)
// 	{
// 		memcpy( &cpumask, &mask, sizeof( hwloc_cpuset_t) );
// 		return true;
// 	}
// 	return false;
// }

// int       ModuleAPI::getCurrentCPU() const
// {
// #if FLOWVR_SCHED_HAVE_GETCPU == 1 // explicitly test for the value being 1
// 	/// @todo check: is there a pthreads call for this?
// 	return sched_getcpu(); // define me in subclass
// #else
// 	return -1;
// #endif // FLOWVR_SCHED_HAVE_GETCPU
// }

// ############################################################################
// ############################################################################


namespace
{
	void writePID(const std::string &name)
	{
		//log the module PID + cmdline in file [hostname_cmdline_pid_date.pid.log]

            char * logdir = getenv("FLOWVR_PID_LOG_DIR");// dir for pid log files
            if ( logdir == NULL)
                {
                    std::cerr << "ModuleAPI::writePID("<<name<<"): FLOWVR_PID_LOG_DIR not defined: cannot store PID, making  flowvr-kill ineffective" 
                              << std::endl;
                    return;
                }
            // Create directory if does not exist
            std::string logdirs = std::string(logdir);
            if (access(logdir, W_OK) != 0)
		{
                    if (mkdir(logdir, 0744) != 0)
			{
                            std::cerr << "ModuleAPI::writePID("<<name<<"): Could not create directory '" << logdirs<< "' for storing PID, making flowvr-kill ineffective"
                                      << std::endl;
                            return;
			}
		}

                logdirs +="/" ;  // trailing backslash may be missing. Add one for safety.
                int pid =getpid();
                std::string pids=ftl::toString<int>(pid);
                // ps -p 2344  -o comm may return a string with / (bin/compute for instance), ret1 being seen a path to a file and not a single file as expected. Change all occruences of / by _ with sed.
                int ret1 = std::system( 
                                       ("echo \""+pids+"\n`ps -p " +pids + " -o args=`\" >"+logdirs+"`hostname`_`ps -p "+pids+" -o comm=|sed sx/x-xg`_"+pids+"_`date +%Hh%M_%d-%m-%Y`.pid.log").c_str()
                                        );
                if (ret1 != 0 ) std::cerr << "ModuleAPI::writePID("<<name<<"): Failed to fill the log file for PID="<<pids<<" in dir="<<logdirs << std::endl;
	}
} // anonymous namespace for writePID

/// Registers and initializes the module. Returns NULL in case of error.
flowvr::ModuleAPI *initModule(const std::vector<flowvr::Port*> &ports,
		const std::string &instancename,
		const std::string &strmodulename)
{

        writePID(strmodulename);//save pid before other problems show up. Usually strmodulename empty at this point (set in  flowvr::ModuleAPIFactory)


	flowvr::ModuleAPI * module;
	std::string instName = instancename; // create a copy, as we might modify it below

    // All modules are initialized in parallel mode by default.
	// They then turn to sequential mode if the environment variables
	// FLOWVR_RANK and FLOWVR_NBPROC are not set when starting the module.

	if (!Parallel::isInitialized())
	{
		Parallel::init();
	}
	if (Parallel::isParallel())
	{
		char buf[10];
		sprintf(buf, "%d", Parallel::getRank());
		if (!instName.empty())
			instName += "/";
		instName += buf;
	}

	module = flowvr::ModuleAPIFactory::registerModule(instName, strmodulename);

        //std::cout << "ModuleAPI::initModule"<<strmodulename<<std::endl; // Usually strmodulename not set before this point. 

	if (module == NULL)
	{
		std::cerr << "ModuleAPI::initModule: registerModule Failed."
				  << std::endl;
		return NULL;
	}

	if (module->init(ports) == 0)
	{
		std::cerr << "ModuleAPI::initModule: Failed." << std::endl;
		module->close();
		delete module;
		return NULL;
	}

	return module;
}

/// Registers and initializes the module with traces. Returns NULL in case of error.
flowvr::ModuleAPI *initModule(const std::vector<flowvr::Port*> &ports,
			      const std::vector<flowvr::Trace*> &traces,
			      const std::string &instancename,
			      const std::string &modulename)
{
        writePID(modulename);//save pid before other problems show up. Usually strmodulename empty at this point (set in  flowvr::ModuleAPIFactory)

	flowvr::ModuleAPI * module;
	std::string instName = instancename;
	if (!Parallel::isInitialized())
	{
		Parallel::init();
	}
	if (Parallel::isParallel())
	{
		char buf[10];
		sprintf(buf, "%d", Parallel::getRank());
		if (!instName.empty())
			instName += "/";
		instName += buf;
	}
	module = flowvr::ModuleAPIFactory::registerModule(instName, modulename);
        //        std::cout << "ModuleAPI::initModule"<<modulename<<std::endl; // Usually strmodulename not set before this point. 
	if (module == NULL)
	{
		std::cerr << "ModuleAPI::initModule:  registerModule Failed."
				<< std::endl;
		return NULL;
	}
	if (module->init(ports, traces) == 0)
	{
		std::cerr << "ModuleAPI::initModule: ModuleAPI Init Failed."
				<< std::endl;
		module->close();
		delete module;
		return NULL;
	}
	return module;
}


} // namespace flowvr
