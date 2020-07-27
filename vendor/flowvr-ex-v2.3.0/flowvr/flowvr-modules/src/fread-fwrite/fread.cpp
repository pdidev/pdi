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
* File: src/utils/fread.cpp                                       *
*                                                                 *
* Contacts:                                                       *
*  02/12/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <flowvr/portreader.h>
#include <iostream>

class PortFile
{
public:
  flowvr::OutputPort port;
  flowvr::MessageWrite msg;
  flowvr::PortReader reader;
  bool repeat;
  PortFile(const std::string& portname, const std::string& filename, bool _repeat)
  : port(portname), reader(filename), repeat(_repeat)
  {
  }
};

std::vector<PortFile*> fports;

int main(int argc, const char** argv)
{
  std::cout << "FlowVR fread module."<<std::endl;
  if (argc<2)
  {
    std::cout << "fread  is a FlowVR module to read data from a file and send it on output ports."<<std::endl;
    std::cout << "Usage: fread -0 [-r] [-l] port=filename ..."<<std::endl;
    std::cout << "Use '-r' to infinitely repeat the first data."<<std::endl;
    std::cout << "Use '-l' to infinitely repeat the complete dataset."<<std::endl;
    std::cout << "Use '-0' to send first message before the first wait."<<std::endl;
    std::cout << "If no port is specified a single \"out\" port is created."<<std::endl;
    return 1;
  }

  bool repeat = false;
  bool loop = false;
  bool first = false;

  for (int i=1;i<argc;i++)
  {
    if (!strcmp(argv[i],"-r"))
    {
      repeat = true;
    }
    else
    if (!strcmp(argv[i],"-0") || !strcmp(argv[i],"-O") || !strcmp(argv[i],"-o"))
    {
      first = true;
    }
    else
    if (!strcmp(argv[i],"-l") || !strcmp(argv[i],"-L"))
    {
      loop = true;
    }
    else if (argv[i][0]!='\0')
    {
      std::string portname = "out";
      std::string filename = argv[i];
      std::string::size_type eqpos = filename.find('=');
      if (eqpos != std::string::npos)
      {
	portname.assign(filename,0,eqpos);
	filename.erase(0,eqpos+1);
      }
      std::cout << "Creating port "<<portname<<" using file "<<filename<<std::endl;
      fports.push_back(new PortFile(portname, filename, repeat));
      repeat = false;
    }
  }

  
  std::vector<flowvr::Port*> ports;
  for (unsigned int p=0;p<fports.size();p++)
  {
    if (!fports[p]->reader.init(fports[p]->port.stamps))
    {
      std::cerr << "Port "<<fports[p]->port.name<<" init failed."<<std::endl;
      return 1;
    }
    flowvr::xml::DOMNode* stamps = fports[p]->port.stamps->generateXML();
    std::cout << "Initialized port "<<fports[p]->port.name<<" with stamps "<<flowvr::xml::DOMWriter::toString(stamps)<<std::endl;
    delete stamps;
    ports.push_back(&(fports[p]->port));
  }

  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL) return 1;

  int it=0;
  bool stop = false;
  while ((!it && first) || flowvr->wait())
  {
    for (unsigned int p=0;p<fports.size();p++)
    {
      if (it==0 || !fports[p]->repeat)
      {
	if (! fports[p]->reader.read(fports[p]->msg, flowvr->getAllocator()) )
	{
	  std::cout << "File "<<fports[p]->reader.filename<<" stopped at iteration "<<it<<std::endl;
	  if (loop)
	  {
	    fports[p]->reader.close();
	    fports[p]->reader.init();
	    if (! fports[p]->reader.read(fports[p]->msg, flowvr->getAllocator()) )
	    {
	      stop = true;
	      break;
	    }
	  }
	  else
	  {
	    stop = true;
	    break;
	  }
	}
      }
      else
      {
	flowvr::StampsWrite newstamps;
	newstamps.clone(fports[p]->msg.stamps, fports[p]->port.stamps);
	fports[p]->msg.stamps = newstamps;
      }
    }
    if (stop) break;

    for (unsigned int p=0;p<fports.size();p++)
    {
      flowvr->put(&(fports[p]->port),fports[p]->msg);
    }

    ++it;
  }

  for (unsigned int p=0;p<fports.size();p++)
  {
    delete fports[p];
  }
  flowvr->close();

  return 0;
}
