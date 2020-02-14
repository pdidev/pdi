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
* File: src/utils/fwrite.cpp                                      *
*                                                                 *
* Contacts:                                                       *
*  02/12/2004 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <flowvr/module.h>
#include <flowvr/portwriter.h>
#include <iostream>

class PortFile
{
public:
  flowvr::InputPort port;
  flowvr::PortWriter writer;
  PortFile(const std::string& portname, const std::string& filename, bool _raw)
  : port(portname), writer(filename, _raw)
  {
  }
};

std::vector<PortFile*> fports;

int main(int argc, const char** argv)
{
  std::cout << "FlowVR fwrite module."<<std::endl;

  if (argc<2)
  {
    std::cout << "fwrite is a FlowVR module to write data received on input ports to a file"<<std::endl;
    std::cout << "Usage: fwrite [-raw] port=filename "<<std::endl;
    std::cout << "If no port is specified a single \"in\" port is created."<<std::endl;
    std::cout << "If -raw is specified only raw message data is written (no stamps specification, stamps&data size, or stamps values)."<<std::endl;
    std::cout << "Note: raw files can not currently be read-back using flowvr-fread."<<std::endl;
    return 1;
  }

  bool raw = false;

  for (int i=1;i<argc;i++)
  {
    if (!strcmp(argv[i],"-raw"))
    {
      raw = true;
    }
    else if (argv[i][0]!='\0')
    {
      std::string portname = "in";
      std::string filename = argv[i];
      std::string::size_type eqpos = filename.find('=');
      if (eqpos != std::string::npos)
      {
	portname.assign(filename,0,eqpos);
	filename.erase(0,eqpos+1);
      }
      std::cout << "Creating port "<<portname<<" using file "<<filename<<std::endl;
      fports.push_back(new PortFile(portname, filename, raw));
      raw = false;
    }
  }

  std::vector<flowvr::Port*> ports;
  for (unsigned int p=0;p<fports.size();p++)
  {
    ports.push_back(&(fports[p]->port));
  }

  flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  if (flowvr == NULL) return 1;

  int it=0;
  while (flowvr->wait())
  {
    if (it==0)
    {
      for (unsigned int p=0;p<fports.size();p++)
      {
	if (!fports[p]->writer.init(fports[p]->port.stamps))
	{
	  std::cerr << "Port "<<fports[p]->port.name<<" init failed."<<std::endl;
	  return 1;
	}
      }
    }
    for (unsigned int p=0;p<fports.size();p++)
    {
      flowvr::Message msg;
      flowvr->get(&(fports[p]->port), msg);
      fports[p]->writer.write(msg);
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
