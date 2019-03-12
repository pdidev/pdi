/******************************************************************************/
/**
 *
 * Author : Andres Saraos Luna
 * File : ModuleCreator.cpp
 * Description : Création automatique de Module et MetaModule
 *  pour flowvr-app à partir d'un fichier de description xml.
 *
 **/
/******************************************************************************/

#include <stdlib.h> // Pour les commandes du shell
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <map>

#include <fstream>
#include <cctype> // pour toupper
#include <algorithm> // pour transform

#include <flowvr/xml.h>

#include "deviceparser.h"

using namespace flowvr::xml;
using namespace std;

// fonction qui teste un argument du main
int checkarg(int argc, const char **argv, const char * param);
// procédure affichant l'utilisation de l'éxécutable
void usage();

// variable globale pour le debug du parcours de fichier xml
bool debug = false;

class ModuleCreator : public DeviceParser
{
private :
  // Attributes
  map<string, std::string> attributes;
  vector<string> output_ports;
  vector<string> input_ports;

  // Methods
  bool getValue(string tagname, std::string valuename);
  bool getPorts();
  bool getAttribute(string tagname, std::string attributename, std::string valuename);

  void fillAttributes();
  void clear();

  void node_err(string node);

  void createModule();
  void createModuleSource();


public :
  ModuleCreator();
  ~ModuleCreator();

  void setParam(string attributename, const char* valuename);
  bool init(string filename);
  bool generateFiles();

  void copyFiles();

};

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/

ModuleCreator::ModuleCreator() : DeviceParser() { }

ModuleCreator::~ModuleCreator() 
{
  clear();
}

bool ModuleCreator::getAttribute(string tagname, std::string attributename, std::string valuename)
{
  attributes[valuename] = DeviceParser::getAttribute(tagname, attributename);
  if (!attributes[valuename].empty())
    {
      if (debug)
        {
          cout << valuename << " field : " << attributes[valuename] << endl;
        }
      return true;
    }
  else
    {
      node_err(attributename);
      return false;
    }
}

bool ModuleCreator::getValue(string tagname, std::string valuename)
{
  attributes[valuename] = DeviceParser::getValue(tagname);
  if (attributes[valuename].empty())
    {
      node_err(tagname);
      return false;
    }
  else
    {
      if(debug)
        {
          cout << valuename << " field : " << attributes[valuename] << endl;
        }
      return true;
    }
}


bool ModuleCreator::getPorts()
{
  // INPUT
  std::string ret = DeviceParser::getInput("buttons");
  if (!ret.empty())
    {
      output_ports.push_back(ret);
      attributes[ret] = getInputModule(ret);
    }
  
  ret = DeviceParser::getInput("analog");
  if (!ret.empty())
    {
      output_ports.push_back(ret);
      attributes[ret] = getInputModule(ret);
    }
  
  ret = DeviceParser::getInput("tracker");
  if (!ret.empty())
    {
      output_ports.push_back(ret);
      attributes[ret] = getInputModule(ret);
    }

  // FEEDBACK
  if(DeviceParser::isNode("feedback"));
  {
    ret = DeviceParser::getFeedback("plane");
    if (!ret.empty())
      {
        input_ports.push_back(ret);
        attributes[ret] = getFeedbackModule(ret);
      }
    
    ret = DeviceParser::getFeedback("trimesh");
    if (!ret.empty())
      {
        input_ports.push_back(ret);
        attributes[ret] = getFeedbackModule(ret);
      }
    
    ret = DeviceParser::getFeedback("field");
    if (!ret.empty())
      {
        input_ports.push_back(ret);
        attributes[ret] = getFeedbackModule(ret);
      }
  }
  
  // On teste si on a au moins un port d'utilisé
  if (output_ports.size() == 0 && input_ports.size() == 0)
    return false;
  else
    return true;
}


bool ModuleCreator::init (string filename)
{
  if (!DeviceParser::loadFile(filename))
    {
      return false;
    }
  else
    return true;
}

bool ModuleCreator::generateFiles()
{
  // name
  if (!getAttribute("device", "name", "name"))
    return false;
  // server
  if (!getAttribute("device", "server", "command"))
    return false;
  
  // info
  if (!getValue("info", "info"))
    return false;
  
  // ports
  if(!getPorts())
    return false;
    
  if (debug)
    {
      cout << "Input :\n";
      for (unsigned int i = 0; i < output_ports.size(); i++)
        cout << i << " : " << output_ports[i] << endl;
    
      cout << "\nFeedback :\n";
      for (unsigned int i = 0; i < input_ports.size(); i++)
        cout << i << " : " << input_ports[i] << endl;
    }

  fillAttributes();
  // Generating -- modulex.comp.h
  createModule();
  // Generating -- modulex.comp.cpp
  createModuleSource();
  
  //  createMetaModuleSource();
  if ( !attributes["cp_header"].empty() && !attributes["cp_source"].empty() )
    copyFiles();
  
  cout << "Good luck with your " << attributes["name"] << " !\n";
  
  clear();
  return true;

}

void ModuleCreator::node_err(string node)
{
  cerr << "Un problème est survenu au noeud " 
       << "<" << node << ">"
       << " du fichier xml.\nFin du Monde.\n";
}

void ModuleCreator::clear()
{
  // Clearing all the data vectors
  attributes.clear();
  output_ports.clear();
  input_ports.clear();
}

void ModuleCreator::fillAttributes()
{
  // Module related informations
  // Name of the module / ex : ModuleJoypad
  attributes["moduleclassname"] = "Module" + attributes["name"];
  // Name of the created file / ex : ModuleJoypad.comp.h
  attributes["modulefilename"] = attributes["moduleclassname"] + ".comp.h";
  // Name of the define clause in the header file / ex : METAMODULEJOYPAD_COMP_H_
  attributes["moduledefinename"] = attributes["moduleclassname"] + "_COMP_H_";
  transform(attributes["moduledefinename"].begin(), attributes["moduledefinename"].end(), attributes["moduledefinename"].begin(), (int(*)(int))toupper);
  
  // Source file related informations
  attributes["modulesourcefilename"] = attributes["moduleclassname"] + ".comp.cpp";

  // Output and Input modules
  if (attributes["output_module"].empty())
    attributes["output_module"] = "vrpn2flowvr";
  attributes["input_module"] = "flowvr2vrpn";
}

void ModuleCreator::createModule()
{
  ofstream file( attributes["modulefilename"].c_str() );

  // INCLUDES AND NAMESPACES
  file << "#ifndef " << attributes["moduledefinename"] << endl
       << "#define " << attributes["moduledefinename"] << endl
       << "\n"
       << "#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>\n"
       << "\n"
       << "using namespace flowvr::app;\n"
    // CLASS
       << "class " << attributes["moduleclassname"] << " : public Composite\n"
       << "{\n"
       << "public :\n"
       << "\t" << attributes["moduleclassname"] << "(const std::string& id_) : Composite(id_)\n"
       << "\t\{" << endl;
  
  // PORTS
  for (unsigned int i=0; i < output_ports.size(); i++)
    file << "\t\taddPort(\"" << output_ports[i] << "\", OUTPUT);\n";
  for (unsigned int i=0; i < input_ports.size(); i++)
    file << "\t\taddPort(\"" << input_ports[i] << "\", INPUT);\n";

  file << "\t\tsetInfo(\"" << attributes["info"] << "\");\n"
       << "\t}\n"
       << "\n"
    // EXECUTE
       << "\tvirtual void execute();\n"
    // CREATE()
       << "\tvirtual Component* create() const\n"
       << "\t{\n"
       << "\t\treturn new " << attributes["moduleclassname"] << "(getId());\n"
       << "\t}\n"
       << "};\n"
       << "\n"
       << "\n"
       << "#endif // " << attributes["moduledefinename"] << "\n";

  cout << attributes["modulefilename"] << " has been succesfully created !\n";

  file.close();

}

void ModuleCreator::createModuleSource()
{
  // TO DO :: Corriger l'argument de vrpn2flowvr et l'inverser --> un
  // module en trop mais pas partout.
  // TO DO :: Ajouter des commentaires dans le code pour repérer les
  // numéros de ligne du code généré pour débugger plus facilement.
  ofstream file( attributes["modulesourcefilename"].c_str() );
  std::string mod, in, out, connex, cmd, p_in, p_out;

  // INCLUDES
  file << "#include <flowvr/app/components/connection.comp.h>\n"
    // TO DO : Corriger le tri qui sort de nulle part
    // Coller les chemins fournis après l'argument ModulePath
    // Sinon les coller dans le repertoire par défaut :
    // "flowvr-vrpn/device/include/"
    // "flowvr-vrpn/device/src/"
       << "#include \"" << attributes["path"] << "/" << attributes["modulefilename"] << "\"\n";
  // Getting the input/output ports filenames for includes
  if (output_ports.size() != 0)
    file << "#include <flowvr-vrpn/components/" << attributes["output_module"] << ".comp.h>\n";
  if (input_ports.size() != 0)
    file << "#include <flowvr-vrpn/components/" << attributes["input_module"] << ".comp.h>\n";

  // On crée le std::string pour vrpn2flowvr
  std::string module_name = attributes["output_module"];
  //transform(module_name.begin(), module_name.begin()+1, module_name.begin(), (int(*)(int))toupper);
  
  // INCLUDES FOR INPUT/OUTPUT PORTS
  for (unsigned int i=0; i < output_ports.size(); i++)
    {
      file << "#include <flowvr-vrpn/components/module" << attributes[output_ports[i]] << ".comp.h>\n";
    }
  for (unsigned int i=0; i < input_ports.size(); i++)
    {
      file << "#include <flowvr/vrpn/components/module" << attributes[input_ports[i]] << ".comp.h>\n";
    }
  // NAMESPACES
  file << "\n"
       << "using namespace flowvr::app;\n"
       << "using namespace flowvrvrpn;\n"
       << "\n"
    // EXECUTE()
       << " void " << attributes["moduleclassname"] << "::execute()\n"
       << "{\n"

    // VRPN2FlowVR Module

       << "\tComponent* " << attributes["output_module"] << " = addObject(MetaModuleFlowvrRunSSHSingleton"
       << "<" << module_name << ">(\"" << attributes["output_module"]
       << "\", CmdLine(\"" << attributes["output_module"] << " "
       << attributes["command"] << "\")));\n";
  
  // Ici en fonction du nombre de ports :
  // On instancie les modules fvvrpn qui correspondent
  // On cree autant de connexions que de ports
  // On connecte les connexions aux ports correspondants

  // OUTPUT PORTS
  for (unsigned int i=0; i < output_ports.size(); i++)
    {
      cmd = attributes[output_ports.at(i)]; 
      //      mod = "metamodule" + cmd;
      //in = "connex_in_" + output_ports[i];
      connex = "c" + output_ports[i];
      p_in = "vrpn_" + cmd.substr(7, cmd.size());
      p_out = "ftl_" + cmd.substr(7, cmd.size());
      
      // Creating the component
      file << "\n/* Output Port " << i+1 << " : " << output_ports[i] << " */\n"
           << "\tComponent* " << cmd
           << " = addObject(MetaModuleFlowvrRunSSHSingleton<Module" << cmd << ">"
           << "(\"" << cmd << "\", CmdLine(\""  
           << cmd << "\")));\n\n"

        // AddObjectAndLink
           << "\taddObjectandLink<Connection>(\"" << connex << "\","
           << attributes["output_module"] << "->getPort(\"vrpnmsg\"),"
           << cmd << "->getPort(\"" << p_in << "\"));\n\n"

        // Linking
           << "/* Linking */\n"
           << "\tlink((" << cmd << "->getPort(\"" << p_out << "\")), (getPort(\""
           << output_ports.at(i) << "\")));\n";
        
        // Linking
//            << "/* Linking " << cmd << " to " << attributes["output_module"] << " */\n"
//            << "\tComponent* " << in << " = "
//            << "addObject(Connection(\"" << in << "\"));\n\n"
      
//            << "\tlink(*(" << attributes["output_module"] << "->getPort(\"" << "vrpnmsg" << "\")), "
//            << "*(" << in << "->getPort(\"in\")));\n"
//            << "\tlink(*(" << in << "->getPort(\"out\")), "
//            << "*(" << cmd << "->getPort(\"" << p_in << "\")));\n\n"
      
//            << "/* Linking " << cmd << " to the MetaModule's ports */\n"
//            << "\tComponent* " << out << " = "
//            << "addObject(Connection(\"" << out << "\"));\n\n"
      
//            << "\tlink(*(" << cmd << "->getPort(\"" << p_out << "\")), "
//            << "*(" << out << "->getPort(\"in\")));\n";
    }

  // INPUT PORTS
  for (unsigned int i=0; i < input_ports.size(); i++)
    {
      cmd = attributes[input_ports.at(i)];
      mod = "metamodule" + cmd;
      in = "connex_in_" + input_ports[i];
      out = "connex_out_" + input_ports[i];
      p_in = "ftl_" + cmd.substr(7, cmd.size());
      p_out = "flowvr_" + cmd.substr(7, cmd.size());
      
      // Creating the component
      file << "\n/* Input Port " << i+1 << " : " << input_ports[i] << " */\n"
           << "\tComponent* " << mod
           << " = addObject(MetaModuleFlowvrRunSSHSingleton<Module" << cmd << ">"
           << "(\"" << cmd << "\", CmdLine(\""
           << cmd << " " << attributes["command"] << "\")));\n\n"
        
        // Linking
           << "\t/* Linking " << mod << " to FlowVR2VRPN */\n"
           << "\tComponent* " << out << " = "
           << "addObject(Connection(\"" << out << "\"));\n\n"
      
           << "\tlink(*(" << mod << "" << "->getPort(\"" << p_out << "\")), "
           << "*(" << out << "->getPort(\"in\")));\n"
           << "\tlink(*(" << out << "->getPort(\"out\")), "
           << "*(" << attributes["input_module"] << "->getPort(\"flowvrmsg\")));\n\n"
      
           << "\t/* Linking " << mod << " to the MetaModule's ports */\n"
           << "\tComponent* " << in << " = "
           << "addObject(Connection(\"" << in << "\"));\n\n"
      
           << "\tlink(*(" << in << "->getPort(\"in\")), "
           << "*(getPort(\"" << input_ports[i] << "\")));\n";
	
    }
  
  file << "}\n"
       << "\n"
       << "\n";
  
  cout << attributes["modulesourcefilename"] << " has been succesfully created !\n\n";
  
  file.close();
}

void ModuleCreator::setParam(string attributename, const char* valuename)
{
  attributes[attributename] = (string)valuename;
  if (debug)
    {
      cout << "Parameter \"" << attributename << "\" inserted.\n"
           << "Value : " << valuename << endl << endl;
    }
}

void ModuleCreator::copyFiles()
{
  std::string msg = "cp " + attributes["modulefilename"] + " " + attributes["cp_header"];
  system(msg.c_str());
  msg = "cp " + attributes["modulesourcefilename"] + " " + attributes["cp_source"];
  system(msg.c_str());

  system("echo Done...");
}

/******************************************************************************/
/******************************************************************************/
/******************************************************************************/


int checkarg(int argc, const char **argv, const char * param)
{ 
  int i;
  for(i = 1; (i<argc) && (strcmp(argv[i],param) != 0) ; i++);
  if ( i < argc && strcmp(argv[i],param) == 0) 
    {
      return i;
    }
  return 0;
}

void usage()
{
  cout << "HowTo :\n"
       << "modulecreator [-h] [-D] -ModuleDir /path/to/copy/files -Path /path/to/include filename.xml\n"
       << "-h | --help : this help menu\n"
       << "-D : DEBUG Mode\n"
       << "-ModuleDir /path/to/copy/header /path/to/copy/sources : copy the created files to the specified location.\n"
       << "-Path /path/to/include/ : path to your include folder of your application\n"
       << "The filename MUST be the last argument given to modulecreator.\n";
}


int main (int argc, const char** argv)
{
  // On test le nombre d'arguments minimal
  if (argc < 2 || checkarg(argc, argv, "-h") || checkarg(argc, argv, "--help"))
    {
      usage();
      return 1;
    }

  ModuleCreator * device = new ModuleCreator();
  
  // Debug Mode
  if (checkarg(argc, argv, "-D"))
    {
      debug = true;
      cout << "Debug Mode On.\n\n";
    }

  cout << "Reading File : " << argv[argc-1] << endl;

  // Initialisation
  if (!device->init((string)argv[argc-1]))
    {
      cerr << "XML File Error.\n"
           << "THE END !!!\n";
      return 1;
    }
  else
    {
      cout << "\nSuccesful Initialisation.\n\n";
      // -Path is mandatory
      if (!checkarg(argc, argv, "-Path"))
        {
          cerr << "**************************************************************\n"
               << "Error : You must specify a Path for the MetaModule*.comp.h "
               << "using the \"-Path /path/to/include\" option.\n"
               << "Exiting program...\n";
          return 1;
        }
      else
        {
          if (checkarg(argc, argv, "-Path") < argc-1)
            device->setParam("path", argv[checkarg(argc, argv, "-Path") + 1]);
          if (checkarg(argc, argv, "-Simulator"))
            device->setParam("output_module", "simulated_device");
          if (checkarg(argc, argv, "-ModuleDir"))
            {
              device->setParam("cp_header", argv[checkarg(argc, argv, "-ModuleDir") + 1]);
              device->setParam("cp_source",  argv[checkarg(argc, argv, "-ModuleDir") + 2]);
            }
        }
      while (device->isDevice())
        {
          if (!device->generateFiles())
            {
              cerr << "Error while generating files.\n";
              return 1;
            }
          else
            device->nextDevice();
        }

    }
  
  return 0;
}

