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

#include "adltree.h"
#include <libxml/xmlreader.h>

#include "interface.h"

#include <iostream>
#include <sstream>

using namespace std;

/* ==============================
 * FUNCTIONS
 */

bool strContains(const string inputStr, const string searchStr) {
	size_t contains;
        
        // simply looking at string inclusion lead to errors (ports confused with patternparallelfromport for instance)
        // Change to a strict string equality test

        
        //        contains = inputStr.find(searchStr);
        //        if (contains != string::npos)
        //        std::cout<<"COMPARE: "<<inputStr<<" "<<searchStr<<""<< ( inputStr.compare(searchStr) == 0 ? " TRUE": "FALSE" ) <<std::endl;

        if ( inputStr.compare(searchStr) == 0 )
		return true;
	else
		return false;
}

/* ==============================
 * DOM TREE
 */

bool buildTree(Component* component, Viewer* v) {
	/* Iterative version;
	 *
	 *	In this algorithm, the Component tree is divided into successive levels.
	 *
	 *	Levels represent the depth of the tree.
	 * 	The root node is level 0. The children of the root node are level 1.
	 * 	Children of those nodes are level 2, and so on.
	 *
	 *	There is one list of Components per level.
	 *
	 *	The algorithm proceeds level by level, to avoid exploding the stack with recursive calls.
	 *	To process level 1, you take the first component of the level and process it's parameter nodes.
	 *	As for child components, you create them and list them in the level below for further examination.
	 *
	 *	When you're done with one component, you process it's sibling.
	 *
	 *	Once you've reached the end of the level, you go down one level, rinse and repeat.
	 *
	 *	The algorithm stops when the level below is empty.
	 *	It means that even after computing all of the level's components, no children components were found.
	 *
	 *	You can trigger popups from within the algorithm with the pointer to Viewer.
	 *	It proved useful for debugging; could be used to inform user about possible anomalies.
	 *
	 */

	int level = 0;

	// Record class; holds the vector of levels in memory
	ComponentLevelRecord* cRec = new ComponentLevelRecord();
	cRec->compLevel[level]->comps.push_back(component);

	// Example of popup
	//v->displayInfo("begin:");

	xmlNode* child;
	do {
		// Going down one level
		level += 1;

		// Creating the list of Components for the level below
		ComponentRecord* rec = new ComponentRecord();
		cRec->compLevel.push_back(rec);

		// Browse through all the upper level components to compute children
		for (std::list<Component*>::iterator i =
				cRec->compLevel[level - 1]->comps.begin(); i
				!= cRec->compLevel[level - 1]->comps.end(); i++) {
			child = (*i)->node->children;

			// Compute this component's children
			while (child !=  NULL) {
				// the 'while' and the 'if' are necessary to avoid processing format nodes (unrelated to content)
				if (child->type == XML_ELEMENT_NODE) {


					/**
					 * HOSTS
					 */
					if (strContains((char*)child->name, "hostlist")) {
						// Process every <host>
						xmlNode* hostNode = child->children;
                                                while (hostNode != NULL) {

							// Check whether it's an <host> tag
							if (hostNode->type == XML_ELEMENT_NODE
									&& strContains((char*)hostNode->name, "host")) {
								// {node is an host}
								//print '    ', hostNode.localName, hostNode.getAttribute('value')

								xmlAttr* value = NULL;
								xmlAttr* FROM = NULL;

								xmlAttr* hostAttr = hostNode->properties;
								while (hostAttr != NULL) {

									if (strContains((char*)hostAttr->name,"name"))
										value = hostAttr;
									else if (strContains((char*)hostAttr->name, "from"))
										FROM = hostAttr;
									hostAttr = hostAttr->next;
								}
								if (value == NULL) { // {FROM is not necessary}
									// {could not find one or more attributes}
									// {file has the wrong format, inform user and return false for failure}
									v->displayInfo(".adl.xml syntax error (in hostlist tag).  Try regenerating your .adl file.");
									return false;
								} else {
									string FROMcontent;
									if (FROM != NULL)
										FROMcontent = string((char*) FROM->children->content);
									else
										FROMcontent = "";

									// create Host
									Host* hostObj = new Host(
											(char*) value->children->content,
											FROMcontent);
									// add Host to Component
									(*i)->hosts.push_back(hostObj);
								}
							}
							hostNode = hostNode->next;
						}
						// END OF HOSTS
					}

					/**
					 * PORT
					 */
                                        else if (strContains( (char*)child->name, "port")) {
						// create Port and add it to Component

						xmlAttr* id = NULL;
						xmlAttr* type = NULL;
						xmlAttr* msgtype = NULL;
						xmlAttr* blockstate = NULL;

						xmlAttr* portAttr = child->properties;
						while (portAttr != NULL) {
							if (strContains((char*) portAttr->name, "id"))
								id = portAttr;
							else if (strContains((char*) portAttr->name, "msgtype"))
								msgtype = portAttr;
							else if (strContains((char*) portAttr->name, "type"))
								type = portAttr;
							else if (strContains((char*) portAttr->name,
									"blockstate"))
								blockstate = portAttr;
							portAttr = portAttr->next;
						}

						if (id == NULL || type == NULL) {
							// {could not find one or more critical attributes}
							// {file has the wrong format, inform user and return false for failure}
							v->displayInfo("Your file seems to have an incorrect format (detected in port tag). It could mean it is too old. Try regenerating your .adl file.");
							return false;
						} else {
							string msgtypecontent = "";
							if (msgtype != NULL) {
								msgtypecontent = string((char*)msgtype->children->content);
							}

							string blockstatecontent = "";
							if (blockstate != NULL) {
								blockstatecontent = string((char*)blockstate->children->content);
							}

							Port* portObj = new Port((char*) id->children->content,
									(char*) type->children->content,
									msgtypecontent,
									blockstatecontent);
							(*i)->ports.push_back(portObj);
						}
						// END OF PORT
					}

					/**
					 * PARAMETERS
					 */

					else if (strContains((char*) child->name, "parameters")) {
						// Process every parameter (unknown tag names)

                                            xmlNode* paramNode = child->children;
						while (paramNode != NULL) {
							if (paramNode->type == XML_ELEMENT_NODE) {

								xmlAttr* from = NULL;

								xmlAttr* paramAttr = paramNode->properties;
								while (paramAttr != NULL) {
									if (strContains((char*) paramAttr->name, "from"))
										from = paramAttr;
									paramAttr = paramAttr->next;
								}

								xmlNode* textNode = paramNode->children;
								char* text;
								while (textNode != NULL) {
									if (textNode->type == XML_TEXT_NODE) {
										text = (char*) textNode->content;
									}
									textNode = textNode->next;
								}

								string fromcontent = "";
								if (from != NULL) {
									fromcontent = string((char*) from->children->content);
								}

								Parameter* parameterObj = new Parameter(
										(char*) paramNode->name,
										fromcontent,
										text);
								(*i)->parameters.push_back(parameterObj);
							}
							paramNode = paramNode->next;
						}
						// END OF PARAMETERS
					}

					/**
					 * LINK
					 */
					else if (strContains((char*) child->name, "link")) {
						// create Link and add it to Component

						xmlAttr* source = NULL;
						xmlAttr* outport = NULL;
						xmlAttr* dest = NULL;
						xmlAttr* inport = NULL;

						xmlAttr* linkAttr = child->properties;
						while (linkAttr != NULL) {
							if (strContains((char*) linkAttr->name, "source"))
								source = linkAttr;
							else if (strContains((char*) linkAttr->name,
									"outport"))
								outport = linkAttr;
							else if (strContains((char*) linkAttr->name, "dest"))
								dest = linkAttr;
							else if (strContains((char*) linkAttr->name,
									"inport"))
								inport = linkAttr;
							linkAttr = linkAttr->next;
						}
						if (source == NULL || outport == NULL || dest == NULL || inport == NULL) {
							// {could not find one or more attributes}
							// {file has the wrong format, inform user and return false for failure}
							v->displayInfo("Your file seems to have an incorrect format (detected in link tag). It could mean it is too old. Try regenerating your .adl file.");
							return false;
						} else {

							LinkBetweenComponents* linkObj =
									new LinkBetweenComponents(
											(char*) source->children->content,
											(char*) outport->children->content,
											(char*) dest->children->content,
											(char*) inport->children->content);

							(*i)->links.push_back(linkObj);
						}

						// END OF LINK
					}

					/**
					 * FILTERCLASS
					 */
					else if (strContains((char*) child->name, "filterclass")) {
						xmlNode* textNode = child->children;
						char* text;
						while (textNode != NULL) {
							if (textNode->type == XML_TEXT_NODE) {
								text = (char*) textNode->content;
							}
							textNode = textNode->next;
						}

						FilterClass* filterClassObj = new FilterClass(text);
						(*i)->filterclass = filterClassObj;

						// END OF FILTERCLASS
					}

					/**
					 * CHILD COMPONENT
					 */
					else {
						// create Component and link it to it's parent

						xmlAttr * id = NULL;
						xmlAttr* compAttr = child->properties;
						while (compAttr != NULL) {
							if (strContains((char*) compAttr->name, "id"))
								id = compAttr;
							compAttr = compAttr->next;
						}

						if (id == NULL) {
							// {could not find one or more attributes}
							// {file has the wrong format, inform user and return false for failure}
							v->displayInfo("Your file seems to have an incorrect format (detected in component tag). It could mean it is too old. Try regenerating your .adl file.");
							return false;
						} else {

							Component* childComp = new Component(child,
									(char*) child->name,
									(char*) id->children->content);


							(*i)->children.push_back(childComp);
							childComp->parent = (*i);

							cRec->compLevel[level]->comps.push_back(childComp);
						}

						// END OF CHILD COMPONENT
					}

					// END OF COMPONENT
				}
				child = child->next;
			}
		}
	} while (!cRec->compLevel[level]->comps.empty());
	// {did not encounter trouble, return true for success}
	return true;
}

// @ deprecated @
void buildTreeRecursive(Component* component, Viewer* v) {
	// Recursive function to build a tree of Components
	//print 'build_tree: processing', component->id

	//v->displayInfo("buildTree()");
	//Browse through the component's nodes

	xmlNode* child = component->node->children;
	while (child != NULL) {
		// Necessary test to avoid processing format tags (tags unrelated to content)
		if (child->type == XML_ELEMENT_NODE) {
			//v->displayInfo("element child found:");
			//v->displayInfo(child->name);

			/**
			 * HOSTS
			 */
			if (strContains((char*) child->name, "hosts")) {
				//v->displayInfo("we have found the hosts :)");
				// Process every <host>
				xmlNode* hostNode = child->children;
				while (hostNode != NULL) {
					// Check whether it's an <host> tag
					if (hostNode->type == XML_ELEMENT_NODE && strContains(
							(char*) hostNode->name, "host")) {
						//v->displayInfo("we found one host");
						// {node is an host}
						//print '    ', hostNode.localName, hostNode.getAttribute('value')

						xmlAttr* value;
						xmlAttr* FROM;

						xmlAttr* hostAttr = hostNode->properties;
						while (hostAttr != NULL) {
							if (strContains((char*) hostAttr->name, "value"))
								value = hostAttr;
							else if (strContains((char*) hostAttr->name, "FROM"))
								FROM = hostAttr;

							hostAttr = hostAttr->next;
						}
						// create Host
						Host* hostObj = new Host(
								(char*) value->children->content,
								(char*) FROM->children->content);
						// add Host to Component
						component->hosts.push_back(hostObj);
					}
					hostNode = hostNode->next;
				}
				//v->displayInfo("end hosts");
				// END OF HOSTS
			}

			/**
			 * PORT
			 */
			else if (strContains((char*) child->name, "port")) {
				// create Port and add it to Component

				//v->displayInfo("we found a port");

				xmlAttr * id;
				xmlAttr * type;
				xmlAttr* msgtype;
				xmlAttr* blockstate;

				xmlAttr* portAttr = child->properties;
				while (portAttr != NULL) {
					if (strContains((char*) portAttr->name, "id"))
						id = portAttr;
					else if (strContains((char*) portAttr->name, "type"))
						type = portAttr;
					else if (strContains((char*) portAttr->name, "msgtype"))
						msgtype = portAttr;
					else if (strContains((char*) portAttr->name, "blockstate"))
						blockstate = portAttr;
					portAttr = portAttr->next;
				}

				Port* portObj = new Port((char*) id->children->content,
						(char*) type->children->content,
						(char*) msgtype->children->content,
						(char*) blockstate->children->content);
				component->ports.push_back(portObj);

				// END OF PORT
				//v->displayInfo("end port");
			}

			/**
			 * PARAMETERS
			 */

			else if (strContains((char*) child->name, "parameters")) {
				// Process every parameter (unknown tag names)

				//v->displayInfo("we found the parameters, woot");

				xmlNode* paramNode = child->children;
				while (paramNode != NULL) {
					if (paramNode->type == XML_ELEMENT_NODE) {
						xmlAttr* from;

						xmlAttr* paramAttr = paramNode->properties;
						while (paramAttr != NULL) {
							if (strcmp((char*) paramAttr->name, "from"))
								from = paramAttr;
							paramAttr = paramAttr->next;
						}

						Parameter* parameterObj = new Parameter(
								(char*) paramNode->name,
								(char*) from->children->content,
								(char*) paramNode->content);
						component->parameters.push_back(parameterObj);
					}
					paramNode = paramNode->next;
				}
				// END OF PARAMETERS
				//v->displayInfo("end parameters");
			}

			/**
			 * LINK
			 */
			else if (strContains((char*) child->name, "link")) {
				// create Link and add it to Component

				//v->displayInfo("we found one link");

				xmlAttr* source;
				xmlAttr* outport;
				xmlAttr* dest;
				xmlAttr* inport;

				xmlAttr* linkAttr = child->properties;
				while (linkAttr != NULL) {
					if (strContains((char*) linkAttr->name, "source"))
						source = linkAttr;
					else if (strContains((char*) linkAttr->name, "outport"))
						outport = linkAttr;
					else if (strContains((char*) linkAttr->name, "dest"))
						dest = linkAttr;
					else if (strContains((char*) linkAttr->name, "inport"))
						inport = linkAttr;
					linkAttr = linkAttr->next;
				}

				//v->displayInfo("creating a link.. here goes");
				LinkBetweenComponents* linkObj = new LinkBetweenComponents(
						(char*) source->children->content,
						(char*) outport->children->content,
						(char*) dest->children->content,
						(char*) inport->children->content);

				component->links.push_back(linkObj);

				// END OF LINK
				//v->displayInfo("end link");
			}

			/**
			 * FILTERCLASS
			 */
			else if (strContains((char*) child->name, "filterclass")) {

				//v->displayInfo("hey, look at that filterclass.");

				FilterClass* filterClassObj = new FilterClass(
						(char*) child->content);
				component->filterclass = filterClassObj;

				// END OF FILTERCLASS
				//v->displayInfo("end filterclass");
			}

			/**
			 * CHILD COMPONENT
			 */
			else {
				// create Component and link it to it's parent

				//v->displayInfo("child is a child. or something else. teehee");

				xmlAttr * id;
				xmlAttr* compAttr = child->properties;
				while (compAttr != NULL) {
					if (strContains((char*) compAttr->name, "id"))
						id = compAttr;
					compAttr = compAttr->next;
				}

				//v->displayInfo("we're about to create a child component... here goes");

				Component* childComp = new Component(child,
						(char*) child->name, (char*) id->children->content);

				//v->displayInfo("it worked! :o");

				component->children.push_back(childComp);
				childComp->parent = component;

				//v->displayInfo("calling buildTree on our new friend");

				// recursive call
				buildTree(childComp, v);

				// END OF CHILD COMPONENT
				//v->displayInfo("end of child");
			}

			// END OF COMPONENT
			//component->printComponentResume();
		}
		child = child->next;
	}
}

Component* parseAdl(char* filename_adl, Viewer* v) {
	xmlDoc* file_adl = xmlParseFile(filename_adl);
	if (file_adl == NULL) {
		printf("error: could not parse file %s\n", filename_adl);
		return NULL;
	}

	xmlNode* root_element = xmlDocGetRootElement(file_adl);

	xmlAttr* id = root_element->properties;

	Component* root = new Component(root_element, (char*) root_element->name,
			(char*) id->children->content);

	if(!buildTree(root, v)) {
		// {could not build the adl tree}
		return NULL;
	} else {
		// {built the adl tree}
		return root;
	}
}

void browseNodes(Component* node, Viewer* v) {
	//v->displayInfo(node->id);

	/*
	for (std::list<Host*>::iterator h = node->hosts.begin();
			h != node->hosts.end(); h++) {
		v->displayInfo((*h)->value);
		v->displayInfo((*h)->fromVal);
	}

	for (std::list<Port*>::iterator p = node->ports.begin();
			p != node->ports.end(); p++) {
		//v->displayInfo((*p)->id);
		//v->displayInfo((*p)->type);
		//v->displayInfo((*p)->msgtype);
		//v->displayInfo((*p)->blockstate);
	}
	*/
	for (std::list<Parameter*>::iterator p = node->parameters.begin();
			p != node->parameters.end(); p++) {
		v->displayInfo((*p)->nodeName);
		v->displayInfo((*p)->fromWhere);
	}

	for (std::list<Component*>::iterator c = node->children.begin();
			c != node->children.end(); c++) {
		browseNodes((*c), v);
	}
}


/* ==============================
 * CONVERT TO DOT
 */
// #########################
// ### Utility functions ###
// #########################
// Outputs valid dot identifiers
string sanitize(string name) {
	int pos;
	pos = name.find('/');
	while (pos != string::npos) {
		name[pos] = '_';
		pos = name.find('/');
	}

	pos = name.find("-");
	while (pos != string::npos) {
		name.erase(pos, 1);
		name.insert(pos, "_minus_");
		pos = name.find('-');
	}

	pos = name.find('.');
	while (pos != string::npos) {
		name.erase(pos, 1);
		name.insert(pos, "_dot_");
		pos = name.find('.');
	}
	return name;
}

// #########################
//# Separate function to generate appropriate spacing for subgraphs
string genIndent(int number) {
	string indent = "";
	for (int i = 0; i < number; i++) {
		indent += "      ";
	}
	return indent;
}

// #########################
//# Select a shape depending on the node type
string shapeToUse(char* type) {
	if (strContains((char*) type, "filter")) {
		return "diamond";
	} else if (strContains((char*) type, "connection") || strContains((char*) type, "connectionstamps")) {
		return "rect, height=1.5";
	} else if (strcmp(type, "port")) {
		return "rect";
	} else {
		return "Mrecord";
	}
}

// #########################
//# Generates labels for filters, connections and primitive components
//# The label entails port declaration for primitives.
//# Uses DOT's table feature
string generateLabel(Component* node, Viewer* v) {
	string result = "";
	if (strContains((char*) node->type, "filter")) {
		result += node->filterclass->value;
		int pos = result.find_last_of('.');
		result = result.substr(pos+1);
	} else if (strContains((char*) node->type, "connection") || strContains((char*) node->type, "connectionstamps")) {
		result += node->id;
		int pos = result.find_last_of('/');
		result = result.substr(pos+1);
	} else {
		// generating for a primitive
		string inputs = "";
		string outputs = "";

		bool firstIn = true;
		bool firstOut = true;

		for (std::list<Port*>::iterator p = node->ports.begin(); p != node->ports.end(); p++) {
			if (strContains((char*) (*p)->type, "INPUT")) {

				if (!firstIn) {
					inputs += "| <" + string(sanitize((*p)->id)) + "> " + string((*p)->id);
				} else {
					inputs += "<" + string(sanitize((*p)->id)) + "> " + string((*p)->id);
					firstIn = false;
				}

				if (!(*p)->blockstate.empty()) {
					inputs += ": " + string((*p)->blockstate);
				}

			} else if (strContains((char*) (*p)->type, "OUTPUT")) {
				if (!firstOut) {
					outputs += "| <" + string(sanitize((*p)->id)) + "> " + string((*p)->id);
				} else {
					outputs += "<" + string(sanitize((*p)->id)) + "> " + string((*p)->id);
					firstOut = false;
				}

				if (!(*p)->blockstate.empty()) {
					outputs += ": " + string((*p)->blockstate);
				}
			}

		}//#for

		result = node->id;
		int pos = result.find_last_of('/');
		node->label = toChar(result.substr(pos+1));
		result = "{{" + inputs + "} | " + result.substr(pos+1);

		// Hosts
		result += " | ";
		string hostLabel("");
		if (!node->hosts.empty()) {
                    //                    hostLabel += "[";
			for (std::list<Host*>::iterator h = node->hosts.begin(); h != node->hosts.end(); h++) {
				hostLabel += string((*h)->value);
				// if (!(*h)->fromVal.empty()) {
				// 	hostLabel += "; from " + string((*h)->fromVal) + " ";
				// }
				hostLabel+= ", ";
			}
			hostLabel = hostLabel.substr(0, hostLabel.length()-2);
                        //			hostLabel += "]";
		}

		node->hostLabel = toChar(hostLabel);

		result += hostLabel + " | {" + outputs + "}}";

	}//#if
	return result;
}

// #########################
string determinePortColor(Port* port) {
	if (strContains((char*) port->type, "INPUT")) {
		return "green";
	} else if (strContains((char*) port->type, "OUTPUT")) {
		return "lightblue";
	} else {
		return "red";
	}
}

// #########################
string selectFillColor(Component* node) {
	if (strContains((char*) node->type, "filter")) {
		return "orange";
	} else {
		return "mediumpurple1";
		//return "mediumaquamarine";
	}
}

// #########################
string selectClusterFillColor(int depth) {
	// DOT takes hexadecimal numbers for fill colors.
	// The root cluster has a fill color of white.
	// As we go deeper, the fill color tends to go gray.
	// We need contrast, so every step goes 25 darker (25 out of 255).
	// The fillcolor is capped at 180 out of 255, so that the clusters remain visible.

	int value = 60 - (depth*15);
	if (value < 0) value = 0;

	value += 195;

	std::stringstream ss;
	ss << std::hex << value << value << value;

	string result;

	ss >> result;

	if (result.size() == 3) {
		result.insert(0, "0");
		result.insert(2, "0");
		result.insert(4, "0");
	}
	return result;
}

// #########################
string selectClusterBorderColor(int depth) {

	int value = 200 - (depth * 30);
	if (value < 0)
		value = 0;

	value += 15;
	std::stringstream ss;
	ss << std::hex << value << value << value;

	string result;
	ss >> result;

	if (result.size() == 3) {
		result.insert(0, "0");
		result.insert(2, "0");
		result.insert(4, "0");
	}

	return result;
}

// #########################
string selectStyle(Component* node) {
	if (strContains((char*) node->type, "connectionstamps")) {
		return "dashed";
	} else if (strContains((char*) node->type, "connection")) {
		return "filled";
	} else {
		return "filled";
	}
}




// Additional
char* toChar(string str) {
	char* writable = new char[str.size() + 1];
	copy(str.begin(), str.end(), writable);
	writable[str.size()] = '\0';

	return writable;
}

// #########################
// ### Render function   ###
// #########################

void renderNode(Component* node, ofstream& Fdot, int depth, Viewer* v) {
	// render a node and call recursively on children
	/*
	 * Symbols
	 * -------
	 *  ## C ## : Compute
	 *  	The code following this symbol performs computation.
	 *  	It could produce a formatted cluster label for instance.
	 *
	 *	## W ## : Write
	 *		Following code will write to a target file.
	 */
	// Used for string conversion
	char* writable;
	string str;

	// Is the component a composite?
	if (!node->children.empty()) {
		// {component is a composite}
		/*
		 *
		 */
		// 1) Begin cluster
		node->isCluster = true;
		Fdot << genIndent(depth) << " subgraph cluster_" << sanitize(node->id) << " { " << endl;
		node->newId = toChar("cluster_"+sanitize(node->id));
		/*
		 *
		 */
		// 2) Generate a reasonably sized cluster label

		// You can only see as far as the parent, provided there is one.
		// Pick a filling color for the cluster.
		// Write your results.

		// ## C ##

		string clusterLabel;
		if (node->parent == NULL) {
			// If we're looking at the root node => get the raw id
			// If we're looking at a level 1 node, get the raw id too
			clusterLabel = node->id;
		}
		else {
			/*
			if (node->parent->parent == NULL) {
				clusterLabel = node->id;
			} else {*/
				// There is a parent, and the parent isn't root.
				// This means we have at least two delimiters in our id.
				const string id = node->id;
				// pos is the boundary between our component and it's parent
				int pos = id.find_last_of('/');

				// Get a substring and find out where the second boundary is.
				// Truncate at second delimiter.
				string subStr = id.substr(0, pos-1);
				int pos2 = subStr.find_last_of('/');

				clusterLabel = id.substr(pos2 +1);
			//}
		}

		node->label = toChar(clusterLabel);
		// {clusterLabel is fit for duty and not sanitized, as it isn't a dot ID}

		// ## W ## Cluster label, fillcolor, border color

		Fdot << genIndent(depth+1) << " label=\"" << clusterLabel << "\";" << endl;
		Fdot << genIndent(depth+1) << " fillcolor=\"#" << selectClusterFillColor(depth) << "\";" << endl;

		Fdot << genIndent(depth+1) << " pencolor=\"#" << selectClusterBorderColor(depth) << "\"" << ";" << endl;
		/*
		 *
		 */
		// 3) Render children nodes

		for (std::list<Component*>::iterator c = node->children.begin();
				c != node->children.end();
					c++) {
			renderNode((*c), Fdot, depth+1, v);
		}
		/*
		 *
		 */
		// 4) Render ports

		// ## W ## Input
		Fdot << genIndent(depth+1) << " subgraph cluster_" << sanitize(node->id) << "_inputPorts_ { " << endl;
		Fdot << genIndent(depth+2) << " label=\"\" " << endl;
		//Fdot << genIndent(depth+2) << " rank=same " << endl;
		Fdot << genIndent(depth+2) << " pencolor=\"#" << selectClusterFillColor(depth) << "\"" << ";" << endl;

		// Browse through all the ports
		for (std::list<Port*>::iterator p = node->ports.begin();
				p != node->ports.end();
					p++) {
			if (strContains((char*) (*p)->type, "INPUT")) {
				// We've found an input port
				char portTable[5] = "port";
				Fdot << genIndent(depth+2) << " " << sanitize(node->id) << "__" << sanitize((*p)->id)
						<< " [ label=\"" << (*p)->id << "\", shape=" << shapeToUse(portTable) << " , style=filled, fillcolor=" << determinePortColor(*p) << " , height=0.25" << " ];" << endl;

				(*p)->newId = toChar( sanitize(node->id) + "__" + sanitize((*p)->id) );
			}
		}//{input ports rendered}

		Fdot << genIndent(depth + 1) << " }" << endl;


		// ## W ## Output
		Fdot << genIndent(depth + 1) << " subgraph cluster_" << sanitize(node->id) << "_outputPorts_ {" << endl;
		Fdot << genIndent(depth + 2) << " label=\"\"" << endl;
		Fdot << genIndent(depth + 2) << " rank=same" << endl;
		Fdot << genIndent(depth + 2) << " pencolor=\"#" << selectClusterFillColor(depth) << "\"" << ";" << endl;

		// Browse through all the ports
		for (std::list<Port*>::iterator p = node->ports.begin(); p
				!= node->ports.end(); p++) {
			if (strContains((char*) (*p)->type, "OUTPUT")) {
				// We've found an output port
				char portTable[5] = "port";
				Fdot << genIndent(depth + 2) << " " << sanitize(node->id)
						<< "__" << sanitize((*p)->id) << " [ label=\"" << (*p)->id
						<< "\", shape=" << shapeToUse(portTable) << ", style=filled, fillcolor="
						<< determinePortColor(*p) << " , height=0.25" << " ];" << endl;
				(*p)->newId = toChar( sanitize(node->id) + "__" + sanitize((*p)->id) );
			}
		}//{output ports rendered}

		Fdot << genIndent(depth + 1) << " }" << endl;

		/*
		 *
		 */
		// 5) Links

		// If you're reaching a primitive's port, the separator you have to use is ':'
		// On the other hand, if you're reaching a composite's port, the separator is '__'
		// Be careful, as those are _two_ underscores.

		// This naming convention for composite ports gets rid of a major conflict and introduces a smaller one instead.
		// Thanks to this addition, you can have a port "A" and a component "A" living in the same composite.
		// But you can't have a port "A" and a component "_A" in the same composite, as those will collide.
		// You can have a port "_A" and a component "A" though.

		// This should not happen, but you never know.
		// The easy way to tackle this is to forbid component names to begin with an underscore.

		// If you are willing to change this naming convention,
		// be well aware that only alphanumerical characters and underscores are allowed in a DOT ID.


		// ## C ## \AND/ ## W ## mixed

		// Process every link
		for (std::list<LinkBetweenComponents*>::iterator l = node->links.begin();
				l != node->links.end();
					l++) {

			// Let's find source
			if (strContains((char*) node->id, (*l)->source)) {
				// source is the working node (composite)

				// ## W ##

				Fdot << genIndent(depth+1) << " " << sanitize((*l)->source) << "__" << sanitize((*l)->outport) << " -> ";
				// {we're done with the source}

			} else {
				// source is not the working node
				// Browse through children components to find it
				for (std::list<Component*>::iterator c = node->children.begin();
						c != node->children.end();
							c++) {
					if (strContains((char*) (*c)->id, (*l)->source)) {
						// We have found the source node
						if (!(*c)->children.empty()) {
							// Source node is a composite

							// ## W ##

							Fdot << genIndent(depth+1) << " " << sanitize((*l)->source) << "__" << sanitize((*l)->outport) << " -> ";
							break;
						} else {
							// Source node is a primitive/filter/connection
							// If filter/connection: do not point to ports

							// ## W ##
							if (strContains((*c)->type, "filter")) {
								// Get rid of the port (fixes warnings)
								Fdot << genIndent(depth+1) << " " << sanitize((*l)->source) << " -> ";
							} else if ( strContains((*c)->type, "connection") || strContains((*c)->type, "connectionstamps") ) { // the second test is unnecessary
								// Get rid of the port (fixes warnings)
								Fdot << genIndent(depth+1) << " " << sanitize((*l)->source) << " -> ";
							}
							else {
								Fdot << genIndent(depth+1) << " " << sanitize((*l)->source) << ":" << sanitize((*l)->outport) << " -> ";
							}
							break;
						}
						// {we're done with the source}
					}
				}
			}
			// {source part of the link written}


			// Let's find dest
			if (strContains((char*) node->id, (*l)->dest)) {
				// dest is the working node

				// ## W ##

				Fdot << sanitize((*l)->dest) << "__" << sanitize((*l)->inport) << ";" << endl;
				// {we're done with the dest}

			} else {
				// dest is not the working node
				// Browse through children components to find it
				for (std::list<Component*>::iterator c = node->children.begin(); c
						!= node->children.end(); c++) {

					if (strContains((char*) (*c)->id, (*l)->dest)) {
						// We have found the dest node
						if (!(*c)->children.empty()) {
							// Dest node is a composite

							// ## W ##

							Fdot << sanitize((*l)->dest) << "__" << sanitize((*l)->inport) << ";" << endl;
							break;
						} else {
							// Dest node is a primitive/filter/connection

							// ## W ##
							if (strContains((*c)->type, "filter")) {
								Fdot << sanitize((*l)->dest) << ";" << endl;
							} else if ( strContains((*c)->type, "connection") || strContains((*c)->type, "connectionstamps") ) {
								Fdot << sanitize((*l)->dest) << ";" << endl;
							} else {
								Fdot << sanitize((*l)->dest) << ":" << sanitize((*l)->inport) << ";" << endl;
							}
							break;
						}
						// {we're done with the dest}
					}
				}
			}
			// {dest part of the link written}
			// {link written}

		}// {all links are processed}

		Fdot << genIndent(depth) << " } " << endl;
		// #end cluster
	}//#endcomposite

	else if (strContains((char*) node->type, "connectionstamps")) {
		node->newId = toChar(sanitize(node->id));
		Fdot << genIndent(depth) << " " << sanitize(node->id) << " [ shape=" << shapeToUse(node->type) << ", label=\"" << generateLabel(node, v) << "\"" << ", style=" << selectStyle(node) << ", fillcolor=aliceblue" << " ];" << endl;
	}
	else if (strContains((char*) node->type, "connection")) {
		node->newId = toChar(sanitize(node->id));
		Fdot << genIndent(depth) << " " << sanitize(node->id) << " [ shape=" << shapeToUse(node->type) << ", label=\"" << generateLabel(node, v) << "\"" << ", style=" << selectStyle(node) <<  ", fillcolor=aliceblue" << " ];" << endl;
	} else {
		// {component is a primitive / a filter}

		node->newId = toChar(sanitize(node->id));
		Fdot << genIndent(depth) << " " << sanitize(node->id) << " [ shape=" << shapeToUse(node->type) << ", style=" << selectStyle(node) << ", fillcolor=" << selectFillColor(node) << ", label=\"" << generateLabel(node, v) << "\" ];" << endl;
	}
}
