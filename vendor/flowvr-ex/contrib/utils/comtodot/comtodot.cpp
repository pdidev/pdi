#include <iostream>
#include <string>
#include <vector>

#include <flowvr/xml.h>
#include <flowvr/app/core/arch.h>

struct Host
{
   std::string id;
   std::vector<unsigned int> comm;
   Host(const std::string& id_, unsigned int nbHost) : id(id_), comm(nbHost, 0) {};
};


std::string getHost(const std::string& id, const flowvr::xml::DOMNode* netXML)
{
   const flowvr::xml::DOMElement* object = netXML->FirstChildElement();
   while(object && object->Attribute("id") != id)
   {
      object = (const flowvr::xml::DOMElement*) netXML->IterateChildren(object);
   }	

   if(object)
   {
      return object->Attribute("host");	
   }
   else
   {
      std::cerr << "ERROR: "<<  id << " cannot be found in network. Create a host called 'UNKNOWN'" << std::endl;
      return "UNKNOWN";
   }
};

int main(int argc, char** argv)
{
   if(argc < 2 && argc >3)
   {
      std::cerr << "Usage: comtodot file.net.xml [file.arch.xml]" << std::endl;
      return 1;
   }


   flowvr::xml::DOMDocument netXML;
   if (!netXML.LoadFile(argv[1]))
   {
      std::cerr << "ERROR: cannot open " << argv[1] << std::endl;
      return 1;
   }

   bool archColor = argc == 3;

   std::vector<Host> matrix;

   std::vector<std::string> type;
   type.push_back("");
   type.push_back("stamps");
   for(unsigned int k = 0; k != type.size(); ++k)
   {
   const flowvr::xml::DOMElement* e = netXML.RootElement();
   for(const flowvr::xml::DOMElement* comXML = e->FirstChildElement("connection"+type[k]); comXML; comXML = (const flowvr::xml::DOMElement*) e->IterateChildren("connection"+type[k], comXML))
   {
      std::string source = getHost(comXML->FirstChildElement("source"+type[k])->FirstChildElement()->Attribute("id"), e);		
      std::string dest = getHost(comXML->FirstChildElement("destination"+type[k])->FirstChildElement()->Attribute("id"), e);		

      unsigned int iSource = 0;
      for(; iSource < matrix.size() && matrix[iSource].id != source; ++iSource) {}

      if(iSource == matrix.size())
      {	
	 for(unsigned int i = 0; i != matrix.size(); ++i)
	 {
	    matrix[i].comm.push_back(0);
	 }
	 Host sourceHost(source, matrix.size() + 1);
	 matrix.push_back(sourceHost);
      }

      unsigned int iDest = 0;
      for(; iDest < matrix.size() && matrix[iDest].id != dest; ++iDest) {} 
      if(iDest == matrix.size())
      {
	 for(unsigned int i = 0; i != matrix.size(); ++i)
	 {
	    matrix[i].comm.push_back(0);
	 }
	 Host destHost(dest, matrix.size() + 1);
	 matrix.push_back(destHost);
      }
      matrix[iSource].comm[iDest] += 1;
   }
   }

   flowvr::app::Architecture a;
   if (archColor)
   {
	a.importArchitecture(argv[2]);
   }
   // intro
   std::cout << "graph comnet" << std::endl << "{" << std::endl;
   unsigned int N = 0;
   unsigned int P = 0;
   for(unsigned int i = 0; i != matrix.size(); ++i)
   {
      for(unsigned int j = 0; j != i; ++j)
      {
	 unsigned int nbCom = matrix[i].comm[j] + matrix[j].comm[i];
	 if (nbCom > 0 )
	 {
		 std::cout << matrix[i].id << " -- " << matrix[j].id << "[label=" << nbCom;
	    if(archColor)
	    {
		if(a.getClusterFromHost(matrix[i].id) != a.getClusterFromHost(matrix[j].id))
		{
		   std::cout<< ",color=red";
		}
	    }
	    std::cout<< "]" << std::endl;
	 }
	 
      }
   }
   std::cout << "}" << std::endl;
   return 0;
}

