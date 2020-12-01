/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
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
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/gltrace/gltrace.cpp                                   *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#include "gltrace.h"
#include <flowvr/utils/filepath.h>
#include <flowvr/utils/cmdline.h>

using namespace flowvr;

int main(int argc, char** argv)
{
	std::string description = "usage: flowvr-gltrace *.gltrace.xml file [-l  path to the log-results file. (default is /tmp/localhost)]  [-d debug output file]";
	flowvr::utils::CmdLine cmdline(description.c_str());

	std::string metaFile="";
	std::string logFile="";
	std::string debugFile="";

    cmdline.opt<std::string>("gltrace", 'g', "your *.gltrace.xml file ",&metaFile  , false);
    cmdline.opt<std::string>("output ", 'l', "your log-results file "  ,&logFile   , false);
    cmdline.opt<std::string>("debug",   'd', "debug output file "      ,&debugFile , false);

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

    if  ( cmdline.args.size() <  1 || cmdline.args[0].size() == 0)
    {
    	std::cout << cmdline.help() << std::endl;
        return 1;
    }

  Parser parser;

  std::cout << "PARSING xml config file ("<< cmdline.args[0] <<")\n";

  parser.ParseConfigFile(cmdline.args[0].c_str());
  if(parser.Error())
  {
    std::cerr << parser.ErrorText() << std::endl;
    return 1;
  }

  FileList files = parser.GetFileList();


  if(logFile != "")
  {
	  std::cout << "Parsing trace result file : " << logFile << std::endl;
	  parser.ParseTraceFile( (char*)logFile.c_str() );
      if(parser.Error())
      {
        std::cerr << parser.ErrorText() << std::endl;
        return 1;
      }
  }
  else{
	  for(unsigned int i=1;i<files.size();i++)
	  {
		std::cout << "PARSING trace result file ("<<files[i]<<")\n";
		parser.ParseTraceFile( (char*)files[i].c_str() );
		if(parser.Error())
		{
		  std::cerr << parser.ErrorText() << std::endl;
		  return 1;
		}
	  }
  }

// DEBUG output
  if(debugFile!="")
  {
    std::cout << "REFORMATING the xml config file in output file ("<< cmdline.args[2] <<")\n";
    xml::DOMElement desc = parser.xmlDesc();
    FILE* debugfile;
    debugfile = fopen(cmdline.args[2].c_str(),"w");
    if(debugfile == NULL)
      std::cout << "\t error -> Can't open file ("<< cmdline.args[2] <<")\n";
    else
    {
      desc.Print(debugfile,0);
      fclose(debugfile);
    }
  }

  std::cout << "DISPLAY... 'q' to quit\n";

  display(&parser);

  return 0;
}
