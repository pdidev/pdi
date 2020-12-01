/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                    Graph Visualization Tool                     *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA                                                           *
 *  ALL RIGHTS RESERVED.                                           *
 *                                                                 *
 * This source is covered by the GNU GPL, please refer to the      *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Jeremie Allard,                                              *
 *    Thomas Arcila,                                               *
 *    Antoine Meler                                                *
 *    Clement Meniers,                                             *
 *    Bruno Raffin,                                                *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: ./src/utils.cpp                                           *
 *                                                                 *
 * Contacts: Xavier Martin <xavier.martin@imag.fr>                 *
 *                                                                 *
 ******************************************************************/

#ifndef ADLTREE_H_
#define ADLTREE_H_

// XML parser
#include <libxml/xmlreader.h>

#include <vector>
#include <list>
#include <string>

#include <iostream>
#include <fstream>


//    Data storage classes
//    (in order of appearance)
//    Host, Port, Parameter, Link, FilterClass, Component

// Example: <host vname="localhost" from="file.csv" />
class Host {
public:
	Host(char* value, std::string fromVal) : value(value), fromVal(fromVal) {}

	// Attributes
	char* value;
        std::string fromVal;
};

// Example: <port id="beginIt" type="INPUT" msgtype="FULL" blockstate="" />
class Port {
public:
	Port(char* id, char* type, std::string msgtype, std::string blockstate) :
		id(id), type(type), msgtype(msgtype), blockstate(blockstate) {}

	char* id;
	char* type;
	std::string msgtype;
	std::string blockstate;

	char* newId;
};


// Example: <stamp from="code">computationTimeIt</stamp>
class Parameter {
public:
	Parameter(char* nodeName, std::string fromWhere, char* value) :
		nodeName(nodeName), fromWhere(fromWhere), value(value) {}

	char* nodeName;
	std::string fromWhere;
	char* value;
};


// Example:  <link source="primes/visu" outport="endIt" dest="primes/greedyKeys" inport="sync" />
class LinkBetweenComponents {
public:
	LinkBetweenComponents(char* source, char* outport, char* dest, char* inport) :
		source(source), outport(outport), dest(dest), inport(inport) {}

	char* source;
	char* outport;
	char* dest;
	char* inport;
};


// Example: <filterclass>flowvr.plugins.Merge</filterclass>
// ---------
// Tag used to specify the type of filter
// ---------
class FilterClass {
public:
	FilterClass(char* value) : value(value) {}

	char* value;
};



//    ---------------------------------
//    /!\ Component is also a tree node
//    ---------------------------------
class Component {
public:
	Component(xmlNode* node, char* type, char* id) :
		node(node), type(type), id(id),
		isCluster(false), filterclass(NULL), newId(NULL), label(NULL), hostLabel(NULL) {}

public:
	xmlNode* node;

	Component* parent;

	char* type;
	char* id;

	std::list<Host*> hosts;
	std::list<Port*> ports;
	std::list<Parameter*> parameters;
	std::list<LinkBetweenComponents*> links;
	std::list<Component*> children;

	FilterClass* filterclass;


	// Later usage
	bool isCluster;

	char* newId;
	char* label;
	char* hostLabel;
};

// Tree building
class ComponentRecord {
public:
	ComponentRecord(){}
public:
	std::list<Component*> comps;
};

class ComponentLevelRecord {
public:
	ComponentLevelRecord(){
		ComponentRecord* rec = new ComponentRecord();
		compLevel.push_back(rec);
	}

public:
	std::vector< ComponentRecord* > compLevel;
};

/*
 * FUNCTIONS
 */

class Viewer;
class QMessageBox;

Component* parseAdl(char* filename_adl, Viewer* v);


char* toChar(std::string str);

void renderNode(Component* node, std::ofstream& Fdot, int depth, Viewer* v);

void browseNodes(Component* node, Viewer* v);

bool strContains(const std::string inputStr, const std::string searchStr);

std::string sanitize(std::string name);

#endif /* ADLTREE_H_ */
