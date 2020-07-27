#include "deviceparser.h"

using namespace flowvr::xml;
using namespace std;

DeviceParser::DeviceParser() : rootnode(NULL), node(NULL), file(NULL), hdl_root(NULL) { }

DeviceParser::~DeviceParser() 
{ 
  delete rootnode; rootnode = NULL;
  delete node; node = NULL;
  delete file; file = NULL;
  delete hdl_root; hdl_root = NULL;
}

void DeviceParser::node_err(string _node)
{
  cerr << "Un problème est survenu au noeud " 
       << "<" << _node << ">"
       << " du fichier xml.\nFin du Monde.\n";
}

bool DeviceParser::init()
{
  if (!file->LoadFile())
  {
    cerr << "Loading Error..." << endl;
    cerr << "error #" << file->ErrorId() << " : " << file->ErrorDesc() << endl;
    return false;
  }
  else
  {
    rootnode = file->FirstChildElement("device");
    hdl_root = new TiXmlHandle( rootnode );
    if(!rootnode)
      return false;
    else
    {
      node = rootnode;
      return true;
    }
  }
} 

bool DeviceParser::loadFile(string filename)
{
  file = new DOMDocument( filename );
  return init();
}

void DeviceParser::nextDevice() 
{
  rootnode = rootnode->NextSiblingElement();
  delete hdl_root;
  hdl_root = new TiXmlHandle( rootnode );
}

bool DeviceParser::isDevice() { return rootnode; }
bool DeviceParser::isNode(string tagname) { setNode(tagname); return node; }

void DeviceParser::setNode(string tagname)
{
//   if(!rootnode)
//     rootnode = hdl_root->FirstChildElement("device").Node();
   
  if (tagname.compare("device") == 0)
    node = rootnode;
  else if (tagname.compare("info") == 0)
    node = hdl_root->FirstChildElement("info").Node();
  // INPUT
  else if (tagname.compare("input") == 0)
    node = hdl_root->FirstChildElement("input").Node();
  else if (tagname.compare("buttons") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("buttons").Node();
  else if (tagname.compare("analog") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("analog").Node();
  else if (tagname.compare("tracker") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("tracker").Node();
  else if (tagname.compare("sensor") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("tracker").FirstChildElement("sensor").Node();
  else if (tagname.compare("tracker2room") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("tracker").FirstChildElement("tracker2room").Node();
  else if (tagname.compare("workspace") == 0)
    node = hdl_root->FirstChildElement("input").FirstChildElement("tracker").FirstChildElement("workspace").Node();
  // FEEDBACK
  else if (tagname.compare("feedback") == 0)
    node = hdl_root->FirstChildElement("feedback").Node();
  else if (tagname.compare("plane") == 0)
    node = hdl_root->FirstChildElement("feedback").FirstChildElement("plane").Node();
  else if (tagname.compare("trimesh") == 0)
    node = hdl_root->FirstChildElement("feedback").FirstChildElement("trimesh").Node();
  else if (tagname.compare("field") == 0)
    node = hdl_root->FirstChildElement("feedback").FirstChildElement("field").Node();

  else
    node = NULL;

}

string DeviceParser::getValue(string tagname)
{
  setNode(tagname);

  if (node)
  {
    return node->FirstChild()->ValueStr();
  }
  else
  {
    node_err(tagname);
    return "";
  }
}


string DeviceParser::getAttribute(string tagname, std::string attributename)
{
  setNode(tagname);

  if (!node->ToElement()->Attribute(attributename))
  {
    node_err(attributename);
    return "";
  }

  std::string ret = node->ToElement()->Attribute(attributename);
  if (!ret.empty())
  {
    return ret;
  }
  else
  {
    node_err(attributename);
    return "";
  }
}

string DeviceParser::getAttribute(DOMNode * tag, std::string attributename)
{
  if (!tag->ToElement()->Attribute(attributename))
  {
    node_err(tag->ValueStr());
    return "";
  }
  else
    return tag->ToElement()->Attribute(attributename);
}

int DeviceParser::getAttribute(DOMNode * tag, std::string attributename, int * value)
{
  if (!tag->ToElement()->Attribute(attributename))
  {
    node_err(tag->ValueStr());
    return 0;
  }
  else
    return tag->ToElement()->QueryIntAttribute(attributename, value);
}

string DeviceParser::getInput(string tagname)
{
  setNode(tagname);

  if (!node)
  {
    return "";
  }
  else
  {
    return node->ToElement()->ValueStr();
  }
}

string DeviceParser::getInputType(string tagname, std::string type)
{
  return getAttribute(tagname, type);
}

string DeviceParser::getInputModule(string tagname)
{
  if (getInputType(tagname, "type").compare("BUTTONCB") == 0)
    return "flowvr_button";
  else if (getInputType(tagname, "type").compare("ANALOGCB") == 0)
    return "flowvr_analog";
  else if (getInputType(tagname, "type").compare("TRACKERCB") == 0)
    return "flowvr_tracker";
  else
    return "";
}

void DeviceParser::getInputs(string tagname, multimap<string, std::string> &m)
{
  setNode(tagname);
  DOMNode * temp = node->FirstChildElement();

  while (temp)
  {
    if (temp->ValueStr().compare("button") == 0	||
	temp->ValueStr().compare("slider") == 0 ||
	temp->ValueStr().compare("sensor") == 0)
      m.insert(pair<string, std::string>(getAttribute(temp,"name"), getAttribute(temp,"id")));

    else if (temp->ValueStr().compare("dpad") == 0 ||
	     temp->ValueStr().compare("stick") == 0 )
    {
      m.insert(pair<string, std::string>(getAttribute(temp,"name"), getAttribute(temp,"idtd")));
      m.insert(pair<string, std::string>(getAttribute(temp,"name"), getAttribute(temp,"idlr")));
    }
    else if (temp->ValueStr().compare("tracker2room") == 0 ||
	     temp->ValueStr().compare("workspace") == 0)
    {
      m.insert(pair<string, std::string>("type", getAttribute(temp,"type")));
    }
    
    temp = temp->NextSiblingElement();
  }

  delete temp;
}

string DeviceParser::getSensorChild(string tagname)
{
  DOMNode * temp = node;
  if (tagname.compare("position") == 0)
  {
    temp = temp->FirstChildElement(tagname);
    return getAttribute(temp, "type");
  }
  if (tagname.compare("velocity") == 0)
  {
    temp = temp->FirstChildElement(tagname);
    return getAttribute(temp, "type");
  }
  if (tagname.compare("acceleration") == 0)
  {
    temp = temp->FirstChildElement(tagname);
    return getAttribute(temp, "type");
  }
  if (tagname.compare("unit2sensor") == 0)
  {
    temp = temp->FirstChildElement(tagname);
    return getAttribute(temp, "type");
  }
  else
    return "";
}

void DeviceParser::getSensorChildren(multimap<string, std::string> &m)
{
  setNode("sensor");

  while (node)
  {
    if (node->ValueStr().compare("sensor") == 0)
    {
      m.insert(pair<string,string>(getAttribute(node,"id"),getSensorChild("position")));
      m.insert(pair<string,string>(getAttribute(node,"id"),getSensorChild("velocity")));
      m.insert(pair<string,string>(getAttribute(node,"id"),getSensorChild("acceleration")));
      m.insert(pair<string,string>(getAttribute(node,"id"),getSensorChild("unit2sensor")));
    }
    node = node->NextSiblingElement("sensor");
  }
}

void DeviceParser::getWorkspaceMinMax(map<double, double> &m)
{
  // Pour chaque min, on va associer la max correspondant.
  setNode("workspace");

  int i;
  m.insert(pair<double,double>
	   (getAttribute(node, "minX", &i),getAttribute(node, "maxX", &i)));
  m.insert(pair<double,double>
	   (getAttribute(node, "minY", &i),getAttribute(node, "maxY", &i)));
  m.insert(pair<double,double>
	   (getAttribute(node, "minZ", &i),getAttribute(node, "maxZ", &i)));
}


string DeviceParser::getFeedback(string tagname)
{
  setNode(tagname);

  if (!node)
  {
    return "";
    setNode();
  }
  else
  {
    return node->ToElement()->ValueStr();
  }
}

string DeviceParser::getFeedbackModule(string tagname)
{
  if (getFeedback(tagname).compare("plane") == 0)
    return "flowvr_plane";
  else if (getFeedback(tagname).compare("trimesh") == 0)
    return "flowvr_trimesh";
  else if (getFeedback(tagname).compare("field") == 0)
    return "flowvr_field";

  else
    return "";
}
