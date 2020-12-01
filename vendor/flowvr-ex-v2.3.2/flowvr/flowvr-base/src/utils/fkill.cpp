#include <iostream>
#include <sstream>
#include <stdlib.h>
#include <signal.h>
#include <unistd.h>
#include <sys/stat.h>
#include <sys/select.h>


#include "flowvr/utils/fkill.h"
#include "ftl/convert.h"


namespace flowvr
{ 
   namespace utils
   { 
     void writePID(const std::string &name)
     {
       //log the process (module or daemon)  PID + cmdline in file [hostname_cmdline_pid_date.pid.log]

       char * logdir = getenv("FLOWVR_PID_LOG_DIR");// dir for pid log files
       if ( logdir == NULL)
	 {
	   std::cerr << "flowvr::utils::writePID("<<name<<"): FLOWVR_PID_LOG_DIR not defined: cannot store PID, making  flowvr-kill ineffective" 
		     << std::endl;
	   return;
	 }
       // Create directory if does not exist
       std::string logdirs = std::string(logdir);
       if (access(logdir, W_OK) != 0)
	 {
	   if (mkdir(logdir, 0744) != 0)
	     {
	       std::cerr << "flowvr::utils::writePID("<<name<<"): Could not create directory '" << logdirs<< "' for storing PID, making flowvr-kill ineffective"
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
       if (ret1 != 0 ) std::cerr << "flowvr::utils::writePID("<<name<<"): Failed to fill the log file for PID="<<pids<<" in dir="<<logdirs << std::endl;
     }
   }//end namespace
}//end namespace


