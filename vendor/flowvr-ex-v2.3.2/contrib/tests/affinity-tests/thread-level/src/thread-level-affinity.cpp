/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                         Affinity tests (thread level)           *
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
******************************************************************/

#include <flowvr/thread.h>
#include <flowvr/topo.h>

#include <iostream>
#include <cmath>


/**
 * we pass a number of arguments to the testing thread
 */
struct sTestArgs
{
	flowvr::Thread *me; /**< a backlink to the thread api, for setting the affinity */
	int             cpu; /**< the cpu to set (will always be 0 in this test */
	int             nNum; /**< the number of CPUs detected (cached, we do not expect
	                           to change that property during run-time) */
};


/**
 * the threaded function to perform. Does merely waste cpu cycles. Use a
 * system monitor (for example htop) to see the cpu utilization go up
 * to 100% on the selected cpu. If it does not, this is a problem.
 */
void cpuTest( sTestArgs *args )
{
	// welcome message
	std::cout << std::endl << "## this thread performs 100% idle polling on all cpus reported." << std::endl;

	int nCpu = args->cpu; // we cache this value just for the printout in the middle (to have the old value)
	for( int j=0; j < args->nNum; ++j)
	{
		std::cout << "this thread runs on cpu [" << args->me->getCurrentCPU() << "]: ";
		std::cout.flush(); // we did not give an endline, so call flush()

		for(int i=0; i < 2000000000;++i) ; // empty on purpose, will caus 100% cpu time for some seconds

		nCpu = j+1;

		if( nCpu < args->nNum )
		{
			std::cout << "thread affinity change from cpu "
					  << args->cpu << " to cpu " << nCpu <<  std::endl;
		}
		else
		{
			break; // leave loop
		}

		// switch to the next cpu using the libtopology types
		topo_cpuset_t t;
		topo_cpuset_zero(&t); // clear
		topo_cpuset_cpu(&t, nCpu); // set 1 bit for the selected cpu

		flowvr::Thread::eAffinityError err;
		if((err=args->me->setAffinity(t)) != flowvr::Thread::ERR_OK) // set affinity
		{
			std::cerr << "flowvr::Thread::setAffinity() indicated an error." << std::endl; // error message
			switch(err)
			{
				case flowvr::Thread::ERR_CPUBIND_FAILED:
					std::cerr << "\tflowvr::Thread::ERR_CPUBIND_FAILED" << std::endl;
					break;
				case flowvr::Thread::ERR_INVALID_TOPOLOGY:
					std::cerr << "\tflowvr::Thread::ERR_INVALID_TOPOLOGY" << std::endl;
					break;
				default:
					std::cerr << "\tUNKNOWN (" << err << ")" << std::endl;
					break;
			}
		}
		// else: say nothing, expect it worked.
	}

	std::cout << "## test DONE" << std::endl; // good-bye message
}


int main(int argc, char **argv)
{
	int nIterations;
	if(argc == 2)
		nIterations = abs(strtol(argv[1], NULL, 10));
	else
		nIterations = 100;

	std::cout << argv[0] << " doing " << nIterations << " iteration"
			  << (nIterations>1 ? "s.":".") << std::endl;

	// create a persistent argument to the thread
	sTestArgs args;

	// create a single thread handle for running the function
	// given above (cpuTest, passing args as arguments)
	flowvr::ThreadFunction<sTestArgs> f(cpuTest, &args);

	// create backlink
	args.me = &f;

	// get some statistics
	flowvr::Topo topo;

	// see, whether the getCurrentCPU() call is working properly
	if( flowvr::Thread::getCurrentCPU() == -1)
		std::cerr << "OS does not support flowvr::Thread::getCurrentCPU()" << std::endl;
	else
		std::cout << "OS supports flowvr::Thread::getCurrentCPU()" << std::endl;


	// get number of physical cpus
	int nNum  = topo.getNumberOfPhysicalCPUs();

	// but iterate over every cpu, including logicals
	args.nNum = topo.getNumberOfLogicalCPUs();

	std::cout << "number of (physical) CPUs: " << nNum << std::endl;
	std::cout << "number of (logical) CPUs: " << topo.getNumberOfLogicalCPUs() << std::endl;
	std::cout << "number of cores: " << topo.getNumberOfCores() << std::endl;

	// iterate over the L3 caches for each CPU
	for( int k=0; k < nNum; ++k)
		std::cout << "cpu[" << k << "] NUMA size: " << topo.getL3CacheSize(k) << std::endl;

	// show some info on the L2 caches for each core
	for( int l2 = 0; l2 < topo.getNumberOfCores(); ++l2 )
	{
		std::cout << "L2 on core [" << l2 << "] is of size " << topo.getL2CacheSize(l2) << "KB" << std::endl;
		std::cout << "L1 on core [" << l2 << "] is of size " << topo.getL1CacheSize(l2) << "KB" << std::endl;
	}

	std::cout << "L2 cache is [" << (topo.getL2CacheShared(0) ? "" : "NOT ") << "SHARED]" << std::endl;
	std::cout << "L2 cache is [" << (topo.getL2CacheShared(1) ? "" : "NOT ") << "SHARED]" << std::endl;


	std::cout << "We will start an idle-polling thread on a dedicated CPU." << std::endl
			  << "We will iterate this " << nIterations
			  << " time" << (nIterations > 1 ? "s":"")
			  << " per CPU." << std::endl;

	for(int j=0; j < nIterations; ++j)
	{
		// always start at cpu 0
		args.cpu = 0;

		topo_cpuset_t t;
		topo_cpuset_zero(&t);

		topo_cpuset_cpu(&t, 0); // set in libtopo

		if(f.setAffinity( t, topo ) == flowvr::Thread::ERR_OK) // set in thread
		{
			if(f.start()) // start thread
				f.wait(); // wait for it to finish (join())
			else
				std::cerr << "could not start thread" << std::endl;
		}
		else
			std::cerr << "could not set affinity to cpu[0]" << std::endl;

		std::cout << "test iteration " << j+1 << " done, " << nIterations-j-1 << " time"
				  << (nIterations-j-1 == 1 ? "":"s") << " to go." << std::endl;
	}

	return 0;
}
