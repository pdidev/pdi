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

#include "Graph.h"
#include "interface.h"
#include "adltree.h"

#include "ConnectionNode.h"
#include "ConnectionStampsNode.h"
#include "FilterNode.h"
#include "PortInNode.h"
#include "PortOutNode.h"
#include "PrimitiveNode.h"
#include "RoutingNode.h"

#include "GEdge.h"

#include <list>
#include <string>
#include <sstream>
#include <iostream>
#include <iomanip>
#include <cstdlib>
#include <vector>

using namespace std;

string toStr(float v) {
	stringstream ss;
	string res;
	ss << v;
	ss >> res;
	return res;
}

// Utility
struct t_boundBox {
	int val[4];
};
typedef struct t_boundBox t_boundBox;
t_boundBox parseBoundBox(string str) {
	t_boundBox box;
	box.val[0]=0;
	box.val[1]=0;
	box.val[2]=0;
	box.val[3]=0;

	if (str.empty())
		return box;

	size_t found;

	found = str.find_first_of(",");
	while (found != string::npos) {
		str[found] = ' ';
		found = str.find_first_of(",", found + 1);
	}

    string str2 = str;
    string buf; // Have a buffer string
    stringstream ss(str2); // Insert the string into a stream

    vector<string> tokens; // Create vector to hold our words

    while (ss >> buf)
        tokens.push_back(buf);

	box.val[0]=atoi(tokens[0].c_str());
	box.val[1]=atoi(tokens[1].c_str());
	box.val[2]=atoi(tokens[2].c_str());
	box.val[3]=atoi(tokens[3].c_str());

	return box;
}

struct t_color3i {
	int val[3];
};
typedef struct t_color3i t_color3i;
t_color3i parseColor(string str, Viewer* v) {
	// Hex or pre-defined
	t_color3i col;
	if (strContains(str.c_str(), "green")) {
		col.val[0] = 0;
		col.val[1] = 255;
		col.val[2] = 0;
	} else if (strContains(str.c_str(), "lightblue")) {
		col.val[0] = 173;
		col.val[1] = 219;
		col.val[2] = 230;
	} else if (strContains(str.c_str(), "mediumpurple1")) {
		col.val[0] = 168;
		col.val[1] = 130;
		col.val[2] = 255;
	} else if (strContains(str.c_str(), "aliceblue")) {
		col.val[0] = 240;
		col.val[1] = 249;
		col.val[2] = 255;
	} else if (strContains(str.c_str(), "orange")) {
		col.val[0] = 255;
		col.val[1] = 162;
		col.val[2] = 0;
	} else if (str[0] == '#') {
		//v->displayInfo("Hex: "+str);
		col.val[0] = strtoul(str.substr(1, 2).c_str(), NULL, 16);

		col.val[1] = strtoul(str.substr(3, 2).c_str(), NULL, 16);

		col.val[2] = strtoul(str.substr(5, 2).c_str(), NULL, 16);
	} else {
		col.val[0] = 255;
		col.val[1] = 255;
		col.val[2] = 255;
	}

	//v->displayInfo(toStr((float)col.val[0])+" "+toStr((float)col.val[1])+" "+toStr((float)col.val[2]));

	return col;
}

struct t_pos2d {
	double val[2];
};
typedef struct t_pos2d t_pos2d;

t_pos2d parsePos(string str) {
	t_pos2d box;
	box.val[0]=0;
	box.val[1]=0;

	if (str.empty())
		return box;

	size_t found;

	found = str.find_first_of(",");
	while (found != string::npos) {
		str[found] = ' ';
		found = str.find_first_of(",", found + 1);
	}

    string str2 = str;
    string buf; // Have a buffer string
    stringstream ss(str2); // Insert the string into a stream

    vector<string> tokens; // Create vector to hold our words

    while (ss >> buf)
        tokens.push_back(buf);

	box.val[0]=atoi(tokens[0].c_str());
	box.val[1]=atoi(tokens[1].c_str());

	return box;
}

float parseFloat(string str) {
	stringstream ss;
	float val;
	ss << str;
	ss >> val;

	return val;
}


void handleLinks(Agraph_t* originGraph, Agraph_t* g, Component* tree, GCluster* cluster, int depth, Viewer* v) {
	// Regular components
	for (list<Component*>::iterator c = tree->children.begin(); c != tree->children.end(); c++) {
		// Grab the node in question
		Agnode_t* node = agfindnode(g, (*c)->newId);
		Agedge_t* edge = agfstout(originGraph, node);
		while (edge != NULL) {
			// Handle the edge
			char tablePos[4] = "pos";
			string pos = agget(edge, tablePos);
			GEdge* gEdge = new GEdge(pos, depth);
			gEdge->init();
			gEdge->generateDisplayList();
			cluster->localEdges.push_back(gEdge);
			edge = agnxtout(originGraph, edge);
		}
	}

	// Cluster ports
	for (list<Port*>::iterator p = tree->ports.begin(); p != tree->ports.end(); p++) {
		// Grab the node in question
		Agnode_t* node = agfindnode(g, (*p)->newId);
		Agedge_t* edge = agfstout(originGraph, node);
		while (edge != NULL) {
			// Handle the edge
			char tablePos[4] = "pos";
			string pos = agget(edge, tablePos);
			GEdge* gEdge = new GEdge(pos, depth);
			gEdge->init();
			gEdge->generateDisplayList();
			cluster->localEdges.push_back(gEdge);
			edge = agnxtout(originGraph, edge);
		}
	}


}

/*
 * Function that reads the graph to create a Cluster
 * Main building function
 */
GCluster* handleCluster(Agraph_t* originGraph, Agraph_t* g, Component* tree, int depth, Viewer* v) {
	// BoundBox, fillcolor, outline color, id
	t_boundBox b = parseBoundBox(agget(g, toChar("bb")));

	//v->displayInfo(toStr(b.val[0])+", " +toStr(b.val[1])+ ", " + toStr(b.val[2])+ ", "+toStr(b.val[3]) );

	GCluster* cluster = new GCluster(b.val[0], b.val[1], b.val[2], b.val[3], depth);
	cluster->setId(tree->newId);

	cluster->label = tree->label;
	cluster->fullLabel = tree->id;

	t_color3i color = parseColor( agget(g, toChar("fillcolor")), v);
	cluster->setFillColor(color.val[0], color.val[1], color.val[2]);

	color = parseColor( agget(g, toChar("pencolor")), v);
	cluster->setOutlineColor(color.val[0], color.val[1], color.val[2]);

	//v->displayInfo(tree->newId);

	Agraph_t* subg;



	/*
	 * inputPorts cluster
	 * ------------------
	 * Notice that we're not calling handleCluster; enables finer control.
	 */
#if USING_CGRAPH
	subg = agsubg(g, toChar(cluster->getId()+"_inputPorts_"), 0);
#else
	subg = agfindsubg(g, toChar(cluster->getId()+"_inputPorts_"));
#endif
	char* boundChar = agget(subg, toChar("bb"));
	if (strcmp(boundChar, "") != 0) {
		b = parseBoundBox(boundChar);
		GCluster* inputCl = new GCluster(b.val[0], b.val[1], b.val[2], b.val[3], depth+1);

		// Fill
		color = parseColor( agget(g, toChar("fillcolor")), v );
		inputCl->setFillColor(color.val[0], color.val[1], color.val[2]);
		// Outline
		color = parseColor( agget(subg, toChar("pencolor")), v );
		inputCl->setOutlineColor(color.val[0], color.val[1], color.val[2]);

		cluster->inputPorts = inputCl;

		t_pos2d pos;
		float width;
		float height;
		string label;
		t_color3i fill;

		PortInNode* p;

		Agnode_t* inPort = agfstnode(subg);
		while (inPort != NULL) {
			pos = parsePos( agget(inPort, toChar("pos")) );
			width = parseFloat( agget(inPort, toChar("width")) );
			height = parseFloat( agget(inPort, toChar("height")) );
			label = agget(inPort, toChar("label"));
			fill = parseColor( agget(inPort, toChar("fillcolor")), v );

			p = new PortInNode(pos.val[0], pos.val[1], depth+2, width, height, label, fill.val[0], fill.val[1], fill.val[2]);

			p->generateDisplayList();
			inputCl->localNodes.push_back(p);

			inPort = agnxtnode(subg, inPort);
		}
	}

	/*
	 * outputPorts cluster
	 */
#if USING_CGRAPH
	subg = agsubg(g, toChar(cluster->getId() + "_outputPorts_"), 0);
#else
	subg = agfindsubg(g, toChar(cluster->getId() + "_outputPorts_")); 
#endif

	boundChar = agget(subg, toChar("bb"));
	if (strcmp(boundChar, "") != 0) {
		b = parseBoundBox(boundChar);
		GCluster* outputCl = new GCluster(b.val[0], b.val[1], b.val[2], b.val[3], depth+1);
		// Fill
		color = parseColor(agget(g, toChar("fillcolor")), v);
		outputCl->setFillColor(color.val[0], color.val[1], color.val[2]);
		// Outline
		color = parseColor(agget(subg, toChar("pencolor")), v);
		outputCl->setOutlineColor(color.val[0], color.val[1], color.val[2]);

		cluster->outputPorts = outputCl;

		t_pos2d pos;
		float width;
		float height;
		string label;
		t_color3i fill;

		PortOutNode* p;

		Agnode_t* outPort = agfstnode(subg);
		while (outPort != NULL) {
			pos = parsePos(agget(outPort, toChar("pos")));
			width = parseFloat(agget(outPort, toChar("width")));
			height = parseFloat(agget(outPort, toChar("height")));
			label = agget(outPort, toChar("label"));
			fill = parseColor(agget(outPort, toChar("fillcolor")), v);

			p = new PortOutNode(pos.val[0], pos.val[1], depth+2, width, height, label,
					fill.val[0], fill.val[1], fill.val[2]);

			p->generateDisplayList();
			outputCl->localNodes.push_back(p);

			outPort = agnxtnode(subg, outPort);
		}
	}

	/*
	 * Children (Clusters or primitives)
	 */
	for (list<Component*>::iterator c = tree->children.begin(); c
			!= tree->children.end(); c++) {
		// Is the child a composite?
		if (!(*c)->children.empty()) {
			// {child is a composite}
#if USING_CGRAPH
			subg = agsubg(g, (*c)->newId, 0);
#else
			subg = agfindsubg(g, (*c)->newId);
#endif
			// Turn them into clusters
			GCluster* child = handleCluster(originGraph, subg, (*c), depth+1, v);
			cluster->subClusters.push_back(child);
		} else {
			// {child is a primitive/flow control component}
			Agnode_t* comp = agfindnode(g, (*c)->newId);
			/*
			 * Filter
			 */
			if (strContains((*c)->type, "filter")) {
				t_pos2d pos = parsePos(agget(comp, toChar("pos")));
				float width = parseFloat(agget(comp, toChar("width")));
				float height = parseFloat(agget(comp, toChar("height")));
				string label = agget(comp, toChar("label"));
				t_color3i fill = parseColor(agget(comp, toChar("fillcolor")), v);

				FilterNode* f = new FilterNode(pos.val[0], pos.val[1], depth, width, height, label, fill.val[0], fill.val[1], fill.val[2], 0,0,0, (*c));

				f->componentLabel = string((*c)->id);

				f->generateDisplayList();
				cluster->localNodes.push_back(f);
			}

			/*
			 * Connection
			 */
			else if (strContains((*c)->type, "connection") || strContains((*c)->type, "connectionstamps")) {
				t_pos2d pos = parsePos(agget(comp, toChar("pos")));
				float width = parseFloat(agget(comp, toChar("width")));
				float height = parseFloat(agget(comp, toChar("height")));
				string label = agget(comp, toChar("label"));
				t_color3i fill =
						parseColor(agget(comp, toChar("fillcolor")), v);
				string style = agget(comp, toChar("style"));

				if (style == "dashed") {
					ConnectionStampsNode* cs = new ConnectionStampsNode(pos.val[0], pos.val[1], depth,
							width, height, label, 132,197,250, 0,0,0, (*c));
					// , fill.val[0], fill.val[1], fill.val[2] << use this to override default color

					cs->componentLabel = string((*c)->id);

					cs->generateDisplayList();
					cluster->localNodes.push_back(cs);
				} else {
					ConnectionNode* cn = new ConnectionNode(pos.val[0],
							pos.val[1], depth, width, height, label, 132,197,250, 0,0,0, (*c));

					cn->componentLabel = string((*c)->id);

					cn->generateDisplayList();
					cluster->localNodes.push_back(cn);
				}
			}

			/*
			 * Routing node
			 */

			/*
			 * Primitive
			 */
			else {
				t_pos2d pos = parsePos(agget(comp, toChar("pos")));
				float width = parseFloat(agget(comp, toChar("width")));
				float height = parseFloat(agget(comp, toChar("height")));
				string label = agget(comp, toChar("label"));
				t_color3i fill =
						parseColor(agget(comp, toChar("fillcolor")), v);
				string style = agget(comp, toChar("style"));


				PrimitiveNode* p = new PrimitiveNode(pos.val[0],
							pos.val[1], depth, width, height, label,
							(*c));

				p->componentLabel = string((*c)->id);

				p->generateDisplayList();
				cluster->localNodes.push_back(p);
			}
		}

		//v->displayInfo(agget(subg, toChar("bb")));
	}

	handleLinks(originGraph, g, tree, cluster, depth, v);


	return cluster;
}

Graph::Graph(Agraph_t* g, Component* tree, Viewer* v) {
	// Main cluster
#if USING_CGRAPH
	rootCluster = handleCluster(g, agsubg(g, toChar(tree->newId), 0), tree, 0, v);
#else
	rootCluster = handleCluster(g, agfindsubg(g, toChar(tree->newId)), tree, 0, v);
#endif


	//v->displayInfo((tree->children.front())->newId);
	//v->displayInfo(agget(subg, toChar("bb")));
}

void Graph::getBoundingBox(Viewer* v) {
	leftBound = rootCluster->x1;
	rightBound = rootCluster->x2;
	topBound = rootCluster->y1;
	bottomBound = rootCluster->y2;
}

void Graph::draw(Viewer* v, bool drawChildren, bool drawNodes, bool drawEdges, bool drawPorts) {
	glEnable(GL_DEPTH_TEST);
	rootCluster->draw(v, drawChildren, drawNodes, drawEdges, drawPorts);
}

void Graph::drawForMiniViewer(Viewer* v) {
	rootCluster->drawForMiniViewer(v);
}
