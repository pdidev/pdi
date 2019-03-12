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
* File: src/utils/shmdump.cpp                                     *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/sharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/utils/hexdump.h"
#include "flowvr/daemondata.h"

#include "flowvr/utils/cmdline.h"

#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <signal.h>

#include <iostream>
#include <set>

using namespace flowvr;
using namespace flowvr::mem;
using namespace ftl;

flowvr::utils::Option<int> mem_id("memid",'m',"Shared Memory ID");
flowvr::utils::FlagOption opt_pid("pid",'p',"Output list of attached PID");
flowvr::utils::FlagOption opt_pname("pname",'n',"Output list of attached process names");
flowvr::utils::Option<int> opt_signal("signal",'s',"Send given signal to all attached processes",2);
flowvr::utils::FlagOption opt_p1("p1",'1',"Start at process 1 (skipping flowvrd process)");

#define tostr2(a) #a
#define tostr(a) tostr2(a)

int main(int argc, char** argv)
{
  mem_id = flowvr::DaemonInterface::getDaemonID();



  bool err = false;
  flowvr::utils::CmdLine cmd("FlowVR Shared Memory Dump version " tostr(FLOWVR_DAEMON_VERSION)
	      "\nUsage: flowvr-shmdump [options] [size [pos]]");


  if (!cmd.parse(argc,argv,&err))
    return err?1:0;

  SharedMemoryManager *shmman = new SharedMemoryManager;
  Allocator::setAllocator( shmman );

  SharedMemoryArea* shm = shmman->openMemoryArea(mem_id,(opt_pid.count+opt_pname.count+opt_signal.count>0)?-1:0);
  if (shm==NULL) return 2;

  if (opt_pid.count>0 || opt_pname.count>0 || opt_signal.count>0)
  {
    std::set<int> pids;
    for (int i=(opt_p1?1:0);i<shm->attachSize();i++)
    {
      int p = shm->getAttachPID(i);
      if (p!=0 && pids.find(p)==pids.end())
      {
	pids.insert(p);
	bool print = false;
	if (opt_pid.count>0)
	{
	  std::cout << p;
	  print = true;
	}
	if (opt_pname.count>0)
	{
	  char buf[256];
	  snprintf(buf, sizeof(buf), "/proc/%d/cmdline",p);
	  int f = open(buf, O_RDONLY);
	  if (f!=-1)
	  {
	    memset(buf, 0, sizeof(buf));
	    read(f, buf, sizeof(buf)-1);
	    close(f);
	    if (print) std::cout << ' ';
	    std::cout << buf;
	    print = true;
	  }
	}
	if (opt_signal.count>0)
	{
	  kill((pid_t)p,(int)opt_signal);
	}
	if (print)
	  std::cout << std::endl;
      }
    }
    if (pids.empty())
    {
      std::cerr << "No PID attached ("<<shm->attachSize()<<" values reserved)."<<std::endl;
    }
  }
  else
  {
    const unsigned char* p = shm->getMappingRead();
    std::cout<<"Shared memory area mapped at "<<(void*)p<<std::endl;
    if (!cmd.args.empty())
    {
      size_t size = atoi(cmd.args[0].c_str());
      size_t pos=0;
      if (cmd.args.size()>=2) pos = atoi(cmd.args[1].c_str());
      flowvr::utils::hexdump(p,size,pos);
    }
    else
    {
      shm->dumpDebugInfo();
    }
  }

  Allocator::setAllocator(NULL, true);
  delete shmman;
  return 0;
}
