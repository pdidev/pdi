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

#ifndef GNODE_H_
#define GNODE_H_

#include "adltree.h"
#include "utils.h"

#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif

#include <string>

class GNode {
public:
	GNode(float x, float y, float z, float width, float height, std::string label, int r=255, int g=255, int b=255, int o_r=0, int o_g=0, int o_b=0, Component* component=NULL, int dpi=72) :
		x(x), y(y), z(z+1), width(width*dpi), height(height*dpi), label(label), dpi(dpi),
		nodeDisplayList(0), directRenderMode(true), component(component) {

		// Default: white
		fillColor[0] = (float)r/255.0;
		fillColor[1] = (float)g/255.0;
		fillColor[2] = (float)b/255.0;
		// Default: black
		outlineColor[0] = (float)o_r/255.0;
		outlineColor[1] = (float)o_g/255.0;
		outlineColor[2] = (float)o_b/255.0;
	}
	~GNode() {
		glDeleteLists(nodeDisplayList, 1);
	}
	// OpenGL calls (fixed pipeline)
	virtual void draw() {}

	virtual void drawToMiniViewer() {}

	float x;
	float y;
	float z;

	float width;
	float height;

	std::string label;
	std::string componentLabel;

	float fillColor[3];
	float outlineColor[3];

	int dpi;

	GLuint nodeDisplayList;
	bool directRenderMode;

	GLuint nodeDisplayListMiniViewer;
	bool directRenderModeMiniViewer;

	Component* component;
};

#endif /* GNODE_H_ */
