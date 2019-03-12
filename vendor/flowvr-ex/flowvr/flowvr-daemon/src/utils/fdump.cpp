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
#include "flowvr/portreader.h"
#include "flowvr/mem/memorymanager.h"
#include "flowvr/utils/hexdump.h"
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
using namespace ftl;

//Option<int> mem_id("memid",'m',"Shared Memory ID");
flowvr::utils::FlagOption opt_pid("data",'d',"Dump message content");
//FlagOption opt_pname("pname",'n',"Output list of attached process names");
//Option<int> opt_signal("signal",'s',"Send given signal to all attached processes",2);
//FlagOption opt_p1("p1",'1',"Start at process 1 (skipping flowvrd process)");

#define tostr2(a) #a
#define tostr(a) tostr2(a)

// dummy class just to be able to instance an allocator
/*
class DummyModule : public ModuleAPIFileImpl
{
public:
    DummyModule()
    : ModuleAPIFileImpl("module","","");
};
*/

std::string xml2text(xml::DOMNode* xml)
{
    if (xml->getNodeType() == xml::DOMNode::ELEMENT_NODE)
    {
	xml::DOMElement* e = dynamic_cast<xml::DOMElement*>(xml);
	std::string res = e->getNodeName();
	xml::TiXmlAttribute* attr = e->FirstAttribute();
	xml::DOMElement* child = e->FirstChildElement();
	if (attr || child)
	{
	    res += '(';
	    bool first = true;
	    while (attr)
	    {
		if (!first) res +=','; else first = false;
		//res += attr->Name();
		//res += '=';
		res += attr->Value();
		attr = attr->Next();
	    }
	    while (child)
	    {
		if (!first) res +=','; else first = false;
		res += xml2text(child);
		child = child->NextSiblingElement();
	    }
	    res += ')';
	}
	return res;
    }
    else
    {
	return xml->getNodeValue();
    }
}

int main(int argc, char** argv)
{
  bool err=false;
  flowvr::utils::CmdLine cmd("FlowVR File Dump version " tostr(FLOWVR_DAEMON_VERSION)
	      "\nUsage: flowvr-fdump [options] file [first [nb]]");
  if (!cmd.parse(argc,argv,&err))
    return err?1:0;
  if (cmd.args.empty()) { std::cerr << cmd.help() << "\n"; return 1; }

  std::string filename = cmd.args[0];

  int first = 0;
  int nb = -1;
  if (cmd.args.size() >= 2) first = atoi(cmd.args[1].c_str());
  if (cmd.args.size() >= 3) nb = atoi(cmd.args[2].c_str());
  
  PortReader infile(filename);
  Allocator* allocator = mem::MemoryManager::instance();
  //DummyModule allocator;
  flowvr::StampList stamps;
  if (!infile.init(&stamps)) return 2;

  std::cout << "Stamps:";
  for (int i=0;i<stamps.nbStamp();i++)
  {
    StampInfo* s = stamps[i];
    xml::DOMNode* xml = s->getType()->xmlDesc();
    std::cout << "\t" << s->getName() << ":" << xml2text(xml); // *xml;
    delete xml;
  }
  std::cout << "\t" << "DATA";
  std::cout << std::endl;
  std::cout << "Messages:" << std::endl;
  flowvr::MessageWrite msg;
  int num = 0;
  while (infile.read(msg, allocator))
  {
      std::cout << num;
      for (int i=0;i<stamps.nbStamp();i++)
      {
	  std::string val;
	  int offset;
	  if (stamps[i]->getType()->array(0,offset))
	  {
	      int a = 0;
	      while (stamps[i]->getType()->array(a, offset))
	      {
		  if (a) val += ',';
		  std::string v;
		  msg.stamps.read((*stamps[i])[a],v);
		  val += v;
		  ++a;
	      }
	  }
	  else
	      msg.stamps.read(*stamps[i],val);
	  std::cout << "\t" << val;
      }
      std::cout << "\t" << msg.data.getSize();
      std::cout << std::endl;      
      ++num;
      msg.clear();
  }
  infile.close();
  return 0;
}
