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
* File: flowvr-suite/flowvr/flowvrd/src/utils/flowvr-kill.cpp     *
*                                                                 *
* Contacts: antoine.vanel@imag.fr                                 *
*                                                                 *
*                                                                 *
******************************************************************/

#include <string>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <iostream>
#include <fstream>
#include <sstream>
#include "flowvr/module.h"
#include "flowvr/utils/cmdline.h"
#include "flowvr/xml.h"


typedef flowvr::xml::DOMElement XML;



flowvr::utils::Option<std::string> OptHostFile    ("hostfile"       , 'f', "File containing the hosts (one per line) to  be inspected for  module killing", false);

flowvr::utils::Option<std::string> OptHostList   ("hostlist"   , 'l' , "List of hosts (comma separated) machine1,machine2,...  to  be inspected for module killing",false);

flowvr::utils::FlagOption          OptClean      ("noclean"   , 'n' , "Do not delete pid log files once module killed");
flowvr::utils::FlagOption          OptVerbose   ("verbose"   , 'v' , "verbose mode (always on)");
flowvr::utils::FlagOption          OptSilent   ("silent"   , 's' , "turn off verbose mode");


int main(int argc, char** argv) {
	

	bool dist=true;
	//bool rm = true;
        std::string flowvrLogDir= std::string(getenv("FLOWVR_PID_LOG_DIR"))+"/";

	
	std::string description = "flowvr-kill: cleaning after a flowvr app failed.\n   Kill module processes logged in FLOWVR_PID_LOG_DIR="+flowvrLogDir+"\n     -- Modules register their pid in the directory given by the  env variable FLOWVR_PID_LOG_DIR\n     -- MMake sure  FLOWVR_PID_LOG_DIR is correctly set (before launching the flowvr app) on all hosts invoved.\n   Kill on localhost if not host given\n\n";

	flowvr::utils::CmdLine cmdline(description.c_str());
	 
	//parse the command line 

        bool error=false;			
        if (!cmdline.parse(argc,argv,&error))
            {
                std::cout << "Error parsing arguments" << std::endl
			  << cmdline.help() << std::endl;
                return error?1:0;
            }
        if (!cmdline.args.empty())
            {
                std::cerr << "Unrecognized arguments:";
                for (unsigned int i=1;i<cmdline.args.size();i++)
                    std::cerr << " " << cmdline.args[i];
                std::cerr << std::endl;
            }

        //        std::vector<std::string> hostList;


        // Force verbose mode on even if not set unless silent mode activated
        if (OptSilent.count<=0) OptVerbose.count=1;



        if (OptHostFile.count>0)
            {
                std::string hostFile=OptHostFile.value();
                if (OptVerbose.count>0) std::cout << "=> kill on hosts listed in file: "<< hostFile << std::endl;
		// open the hostFile :
                std::ifstream fichier_tmp(hostFile.c_str());
                if ( fichier_tmp ) {
                    std::string line_tmp; 
                    while ( std::getline( fichier_tmp, line_tmp ) )
                        {

                            std::string system = "ssh " + line_tmp + " flowvr-kill" ;
                            if(OptVerbose.count>0) {system += " -v";}
                            if(OptClean.count>0) system += " -n"; 
                            if (OptVerbose.count>0) std::cout << "=> "<< system;
                            int getReturnValue = std::system(system.c_str());
                            if (OptVerbose.count>0) std::cout << ": "<<(getReturnValue ==0?"OK":"FAILED")<< std::endl;;
                        }
                }
                else { std::cerr << "flowvr-kill cannot open " <<hostFile<<std::endl;}		
            }
	if (OptHostList.count>0)
            {
                std::string hostList=OptHostList.value();
                if (OptVerbose.count>0) std::cout << "=> kill on hosts: "<< hostList << std::endl;
                std::istringstream iss(hostList);
                std::string mach;
                while ( std::getline( iss, mach, ',' ) )
                    {
                        std::string system =  "ssh " + mach + " flowvr-kill";
                        if(OptVerbose.count>0) {system += " -v";}
                        if(OptClean.count>0) {system += " -n";}
                        if (OptVerbose.count>0) std::cout << "=> "<< system;
                        int getReturnValue = std::system(system.c_str());
                        if (OptVerbose.count>0) std::cout << ": "<<(getReturnValue ==0?"OK":"FAILED")<< std::endl;;
                    }
            }
	else 
            {//local killing (we may be on a local host or a distant machine)

                //Test if  log directory exist.
                char s[200];
                sprintf(s, "test -e %s",flowvrLogDir.c_str());
                if (system(s) != 0)
                    {
                        std::cerr <<"Failed: directory "<<flowvrLogDir<<" does not exist (given by  env var FLOWVR_PID_LOG_DIR - supposed to contain module pids)"<<std::endl;
                        return 0;
                    }
                
                std::string  full_cmd;
                std::string  workfile;
                
                // get the host name to launch kill command only on local processes			
                char host[255];
                gethostname(host,sizeof(host));
                
                full_cmd += "ls " + flowvrLogDir +"*"+host+"*.pid.log > " + flowvrLogDir + "workfile 2>/dev/null";
                //std::cout<<full_cmd<<std::endl;
                int getReturnValue = std::system(full_cmd.c_str());
                
                workfile = flowvrLogDir + "workfile";
                // open it
                std::ifstream fichier_tmp(workfile.c_str());
                int countworklines=0;
                int countkill=0;
                if ( !fichier_tmp.fail() ) 
                    {
                        std::string line_tmp; 
                        while ( std::getline( fichier_tmp, line_tmp ) )
                            {
                                countworklines++;
                                //std::cout << line_tmp << std::endl;
                                std::ifstream fichier_log(line_tmp.c_str());
                                if (!fichier_log.fail())
                                    {
                                        std::string line_log, cmdline;
                                        
                                        // first line contain pid
                                        if ( !std::getline( fichier_log, line_log )  )
                                            {
                                                if (OptVerbose.count>0) std::cout << "=> file "<<line_tmp<<": first line empty (pid expected). Skip"<< std::endl;
                                                goto CLEAN;
                                            }

                                        //second one contains cmdline
                                        if ( !std::getline( fichier_log, cmdline ) )
                                            {
                                                if (OptVerbose.count>0) std::cout << "=> file "<<line_tmp<<": second line empty (cmdline expected). Do not attemp to kill this process"<< std::endl;
                                                goto CLEAN;
                                            }


                                        // Still running ? 


                                        //                                        int ret = std::system(("ps -p " + line_log + " -o pid= | grep -q \"^"+line_log+"$\" ").c_str()); 
                                        // Make grep more flexible (founded cases where 'ps -p 3333  -o pid=' put spaces before the pid 
                                        
                                        int ret = std::system(("ps -p " + line_log + " -o pid= | grep -q \""+line_log+"\" ").c_str()); 
                                        if (ret != 0 )
                                            { 
                                                if (OptVerbose.count>0)
                                                    std::cout << "=> Process pid="<<line_log << " already killed" << std::endl;
                                            }
                                        else
                                            {
                                                // check that process cmdline  match the one recorded in log. 
                                                ret = std::system(("ps -p " + line_log + " -o args= | grep -q \"^"+cmdline+"$\" ").c_str()); 
                                                if(ret == 0 )// found a running pid with same cmdline
                                                    { 
                                                        countkill++;
                                                        std::string cmd = "kill -9 " + line_log;
                                                        if (OptVerbose.count>0) std::cout << "=> "<<cmd.c_str();
                                                        int getreturnvalue = std::system(cmd.c_str());
                                                        if (OptVerbose.count>0) std::cout << "=> "<<(getreturnvalue ==0?"ok":"failed")<< std::endl;;
                                                    }
                                                else
                                                    {
                                                        if (OptVerbose.count>0)
                                                            std::cout << "=> Process pid="<<line_log << " cmdline does not match with the cmdline stored in log  file "<< line_tmp <<". Pid has  probably  been reused for new process. Don't kill it" << std::endl;
                                                        
                                                    }
                                            }
                                    }
                                
                                CLEAN:if( OptClean.count < 1)
                                    {
                                        //remove pid log file
                                        std::string rm_cmd = "rm " + line_tmp;
                                        if (OptVerbose.count>0) std::cout << "=> " <<rm_cmd<< std::endl;
                                        getReturnValue = std::system( (rm_cmd+" &> /dev/null").c_str());
                                    }            
                                
                            }
                        if (OptVerbose.count>0) std::cout << "====> Found "<<countworklines<<" pid log files for host "<< host<<" in "<<flowvrLogDir<<".\n====> Killed "<< countkill<< " processes"<< std::endl;
                        //close and rm workfile
                        fichier_tmp.close();
                        getReturnValue = std::system( ("rm "+workfile+"&> /dev/null").c_str() );
                    }
                return 0;
            }
}

