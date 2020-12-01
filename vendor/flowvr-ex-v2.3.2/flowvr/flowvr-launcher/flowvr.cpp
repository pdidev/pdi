/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Application Library                         *
 *                                                                 *
 *-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
 * INRIA                                                           *
 * ALL RIGHTS RESERVED.	                                          *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Thomas Arcila,                                               *
 *    Jean-Denis Lesage.                                           *
 *    Clement Menier,                                              *
 *    Bruno Raffin                                                 *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 *        File: ./src.flowvr.cpp                                   *
 *                                                                 *
 *	Contact : Jean-Denis Lesage                               *
 *                                                                 *
 ******************************************************************/


/*! \mainpage
The goal of the FlowVR library is to provide users with the necessary tools to develop and
run high performance interactive applications on PC clusters and Grids. The main target
applications include virtual reality and scientific visualization. FlowVR enforces a modular
programming that leverages software engineering issues while enabling high performance
executions on distributed and parallel architectures.
 */




#include <cctype>
#include <algorithm>
#include <unistd.h>
#include <sys/stat.h>
#include <flowvr/telnet.h>
#include <flowvr/utils/filepath.h>
#include <flowvr/utils/cmdline.h>
#include <string>
#include <iostream>

using namespace flowvr::xml;




#define EXECUTE (1 << 0)
#define SETHOSTS (1 << 1)
#define RESOLVEMSGTYPES (1 << 2)
#define SETCONNECTION (1 << 3)
#define XMLBUILD  (1 << 4)
#define RUNBUILD  (1 << 5)


flowvr::utils::FlagOption OptBatchMode("batch", 'a', "switch from interactive to  batch execution: disable reading command lines from terminal");



int main(int argc, char** argv)
{

  /*
   * Process command line arguments
   */
  const std::string compclass="[app]";
  const std::string compclasslower="[app]";

  std::string description = "usage: flowvr [options] "+ compclasslower;

  flowvr::utils::CmdLine cmdline(description.c_str());

  std::string  prefixFile = compclasslower;

  // Parse commande line
  // Exit if error or --help option
  bool error = false;
  if ( !cmdline.parse(argc, argv, &error ) )
  {
    std::cout << cmdline.help() << std::endl;
    if(error)
      return 1;
    return 0;
  }


  /*
   * Retrieving application name
   */
  std::string compname;

  if  ( cmdline.args.size() !=  1 || cmdline.args[0].empty() )
  {
      std::cerr << "One and only one application name expected as input argument (without any extension)" << std::endl;
      return 1;
  }
  // component name  (also the component class name)
  compname = cmdline.args[0];



  // We actually always use the lower case version of the component name.
  // So we are case insensitive regarding the component name
  // The dynamic component loader (@class GenClass) also register the component name using its lower case name.
  std::string compnamelower = compname;
  std::transform(compnamelower.begin(), compnamelower.end(),compnamelower.begin(),::tolower);

  // Restore option values to default value if not overwritten
  if (prefixFile == compclasslower)
      prefixFile=compnamelower;




  /*
   * Loading .run.xml and .cmd.xml files
   */
  // Loading the .run.xml produced by py-app
  std::string runFile = compnamelower+".run.xml";
  DOMDocument* fRun = new  DOMDocument(runFile);

  // Process the run.xml
  if(!fRun->LoadFile())
  {
    std::cerr << prefixFile.c_str() << ".run.xml cannot be loaded." << std::endl;
    return 1;
  }

  std::string cmdFile = compnamelower+".cmd.xml";
  DOMDocument* fCmd = new DOMDocument(cmdFile);

  // Process cmd.xml file
  if(!fCmd->LoadFile())
  {
    std::cerr << prefixFile.c_str() << ".cmd.xml cannot be loaded." << std::endl;
    return 1;
  }



  /*
   * Launching the application
   */
  BasicController appController;
  if (!appController.init(prefixFile))
  {
    std::cerr << "Cannot launch the application" << std::endl
      << "\tMake sure a flowvr daemon is running" << std::endl
      << "\tLaunching aborted" << std::endl;
    delete fCmd;
    delete fRun;
    return 1;
  }

  std::string parent = appController.getParent();

  std::cout << "FLOWVR_PARENT= " << parent.c_str() << std::endl;
  
  if (setenv("FLOWVR_PARENT", parent.c_str(), 1))
  {
    std::cerr << "setting FLOWVR_PARENT environment variable failed." << std::endl;
  }

  {
    char buffer[1024];
    if (getcwd(buffer, sizeof(buffer))==NULL)
    {
      std::cerr << "getcwd failed." << std::endl;
    }
    else if (setenv("FLOWVR_PWD",buffer,1))
    {
      std::cerr << "setting FLOWVR_PWD environment variable failed." << std::endl;
    }
  }

  appController.start();


  // Register all modules that are expected to connect
  // (to sync the .run.xml with the .cmd.xml)
  {
    std::set<std::string> & allExpectedModules = appController.allExpectedModules;

    for(DOMElement* cmd = fCmd->RootElement()->FirstChildElement();
        cmd;
        cmd = cmd->NextSiblingElement()) {
      TiXmlElement *elt = cmd->ToElement();
      if(strcmp(cmd->Value(),            "addobject"               ) == 0 && elt &&
         strcmp(elt->Attribute("class"), "flowvr.plugins.Regulator") == 0) {
        allExpectedModules.insert(cmd->Attribute("id"));
      }
    }
  }

  // CALL COMMANDS FROM .RUN.XML FILE
  std::cout << "Processing " << runFile.c_str() << std::endl ;
  DOMElement* root = fRun->RootElement();
  DOMElement* cmd = root->FirstChildElement();
  while (cmd!=NULL)
  {
      if (!appController.ok())
      {
          std::string cmds="";
          std::cerr << "Cannot send command: " << std::endl
            << "\t " << (cmds<<*cmd).c_str() << std::endl
            << "\tLaunching aborted" << std::endl;

          delete fRun;
          delete fCmd;
          return 1;
      }
      appController.processCommand(cmd);
      cmd = cmd->NextSiblingElement();
  }


  // CALL COMMANDS FROM .CMD.XML FILE
  std::cout << "Processing " << cmdFile.c_str()  << std::endl ;
  root = fCmd->RootElement();
  cmd = root->FirstChildElement();
  while (cmd!=NULL)
  {
      if (!appController.ok())
      {
          std::string cmds="";
          std::cerr << "Cannot send command: " << std::endl
            << "\t " << (cmds<<*cmd).c_str() << std::endl
            << "\tLaunching aborted" << std::endl;


          delete fRun;
          delete fCmd;
          return 1;
      }
      appController.processCommand(cmd);
      cmd = cmd->NextSiblingElement();
  }


  // APPLICATION IS RUNNING
  
  // READ USER COMMANDS FROM THE TERMINAL
  if (!OptBatchMode)
    {
      appController.startInteractiveThread();
    }


  // Wait stop that can be executed either from the interactivethread or from the abortthread 
  appController.stopOnSignal();

  std::cout << "Flowvr "<< compname <<" done!" << std::endl << std::flush;
  delete fRun;
  delete fCmd;
  return 0;
};


