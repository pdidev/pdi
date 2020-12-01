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

#ifndef GCLUSTER_H_
#define GCLUSTER_H_

#include "GNode.h"
#include "GEdge.h"

#include <string>
#include <list>
#include <set>
#include <vector>
#include <utility>

#include <pthread.h>

class Viewer;
class Q3ListViewItem;

class GCluster {
public:
	GCluster(float x1, float y1, float x2, float y2, int depth) :
		x1(x1), y1(y1), x2(x2), y2(y2), inputPorts(NULL), outputPorts(NULL), id(""), depth(depth),
		minivGenerationPhasePassed(false) {
		outlineColor[0]=0;
		outlineColor[1]=0;
		outlineColor[2]=0;
		fillColor[0]=255;
		fillColor[1]=255;
		fillColor[2]=255;
	}

	bool isClusterVisible(Viewer* v);

	GNode* findNodeByName(std::string name, Viewer* v, std::string fullName);
	GCluster* findClusterByName(std::string name, Viewer* v);

	GNode* findNodeByPosition(float x, float y, Viewer* v);

	void fillHostsVector(std::set<string>& hosts);
	void fillHostPairList(std::vector< std::pair<string,Q3ListViewItem*> >& hosts);

	void draw(Viewer* v, bool drawChildren=true, bool drawNodes=true, bool drawEdges=true, bool drawPorts=true);

	void drawForMiniViewer(Viewer* v);

	void setOutlineColor(int r, int g, int b);
	void setFillColor(int r, int g, int b);

	// Public for ease of use
	std::list<GCluster*> subClusters;
	std::list<GNode*> localNodes;
	std::list<GEdge*> localEdges;

	GCluster* inputPorts;
	GCluster* outputPorts;


    std::string getId() const;
    void setId(std::string id);


	//id
	std::string id;
	std::string label;
	std::string fullLabel;


	// /!\ might not be correct
	// Upper left corner
	float x1;
	float y1;

	// Bottom right corner
	float x2;
	float y2;

	float outlineColor[3];
	float fillColor[3];

	int depth;

	// Miniviewer DL
	GLuint miniViewerDL;
	bool minivGenerationPhasePassed;
};

#endif /* GCLUSTER_H_ */
