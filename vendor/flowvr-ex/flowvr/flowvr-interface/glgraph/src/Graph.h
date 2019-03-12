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

#ifndef GRAPH_H_
#define GRAPH_H_

#include "settings.h"
#include "GEdge.h"
#include "GNode.h"

#include "GCluster.h"

#include <iostream>
#include <fstream>
#include <string>
#include <list>

#include <gvc.h>

#if USING_CGRAPH
#include <cgraph.h>
#endif

#include "adltree.h"

std::string toStr(float v);

/*
 * Graph
 */

class Viewer;
class Graph {
public:
	float rightBound;
	float leftBound;
	float topBound;
	float bottomBound;
	void* graphvizGraph;

	GNode* findVertex(int id);
	GEdge* findEdge(int id);
	void* find(int id);

	Graph(Agraph_t* g, Component* tree, Viewer* v);
	~Graph() {
		delete rootCluster;
	}

	int findShape(int id);
	int findHost(int id);
	void drawZoomedVertex(int color_mode, float* shape_color,
			float* host_color, GNode* zoomed_vertex);
	void zoomVertex(int num);
	void changeVertexColor(int id, float r, float g, float b);

	void draw(Viewer* v, bool drawChildren=true, bool drawNodes=true, bool drawEdges=true, bool drawPorts=true);

	void drawForMiniViewer(Viewer* v);

	bool draw(bool fast, int color_mode, bool only_distant_conn,
			float* shape_color, float* host_color,
			GNode* zoomed_vertex = NULL, GEdge* zoomed_edge = NULL);


	void select(std::list<GNode*> *selected_vertices, int &niveau_exploration,
			int mode_exploration, bool maxdepth);
	void select(std::list<void*> *selected_vertices, int &niveau_exploration,
			int mode_exploration, bool maxdepth);
	void getBoundingBox(Viewer* v);

	std::list<GNode>::iterator getVertices();
	std::list<GEdge>::iterator getEdges();


	GCluster* rootCluster;
private:
	int selectedVertexID;
	int nbVisible;


	void applyDepthVisibility(int depth);
	GNode* findVertex(std::string id);
	void convertInch2pix(float dpi_x, float dpi_y);
};

#endif /* GRAPH_H_ */
