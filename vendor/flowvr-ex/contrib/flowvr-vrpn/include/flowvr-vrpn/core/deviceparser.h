#ifndef DEVICE_PARSER_H_
#define DEVICE_PARSER_H_

#include <string>
#include <map>

#include <flowvr/xml.h>

class DeviceParser
{
private :
  // Charge le fichier XML et initialise les DOMNode à leurs positions initiales
  bool init();

  // Permets de fixer le DOMNode "node" à la balise donnée en paramètre
  void setNode(std::string tagname = "device");

protected :
  flowvr::xml::TiXmlHandle * hdl_root;
  flowvr::xml::TiXmlNode * rootnode;
  flowvr::xml::TiXmlNode * node;
  flowvr::xml::TiXmlDocument * file;

  // Message d'erreur simple à afficher
  void node_err(std::string _node);

public :
  DeviceParser();

  ~DeviceParser();

  // Renvoie le nom de la balise
  std::string getValue(std::string tagname);

  // 2 méthodes pour récupérer un attribut donné d'une balise
  // La première prends en premier argument le nom de la balise et se charge d'y accéder
  // La deuxième prends en premier argument un DOMNode pointant sur la balise concernée
  std::string getAttribute(std::string tagname, std::string attributename);
  std::string getAttribute(flowvr::xml::DOMNode * tag, std::string attributename);
  int getAttribute(flowvr::xml::DOMNode * tag, std::string attributename, int * value);
  //  double getAttribute(flowvr::xml::DOMNode * tag, std::string attributename);

  // Récupère le nom de la balise <tagname>. Permets surtout d'en contrôler l'existence.
  std::string getInput(std::string tagname);

  // Récupère l'attribut "type" de la balise d'Input <tagname>
  std::string getInputType(std::string tagname, std::string type);

  // En fonction du nom de la balise et de son type, cette méthode renvoie le nom
  // du module FlowVR-VRPN correspondant
  std::string getInputModule(std::string tagname);

  // Remplit la multimap donnée en paramètre des attributs de toutes les balises input
  // filles de <tagname>
  void getInputs(std::string tagname, std::multimap<std::string, std::string> &storage);

  // Renvoie l'attribut "type" d'une balise fille de <sensor>, donnée en paramètre
  std::string getSensorChild(std::string tagname);

  // Remplit la multimap donnée en paramètre de tous les attributs de toutes les balises
  // filles de toutes les balises <sensor>
  void getSensorChildren(std::multimap<std::string, std::string> &storage);

  void getWorkspaceMinMax(std::map<double, double> &storage);

  // Récupère le nom de la balise <tagname>. Permets surtout d'en contrôler l'existence.
  std::string getFeedback(std::string tagname);

  // En fonction du nom de la balise, on renvoit le module FlowVR-VRPN correspondant
  std::string getFeedbackModule(std::string tagname);

  // Charge le fichier XML donné en paramètre
  bool loadFile(std::string filename);

  // Fais passer le parser au noeud <device> suivant
  void nextDevice();

  // Vérifie l'existence d'un noeud <device>
  bool isDevice();
  bool isNode(std::string tagname);
};


#endif // DEVICE_PARSER_H_
