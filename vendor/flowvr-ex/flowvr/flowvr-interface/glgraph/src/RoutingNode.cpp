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

#include "RoutingNode.h"

#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif


void RoutingNode::draw() {
	// Inside
	glColor3f(fillColor[0],fillColor[1],fillColor[2]);

	glBegin(GL_QUADS);
		glVertex3f(x - width/2.0, y - height/2.0, z);
		glVertex3f(x - width/2.0, y + height/2.0, z);
		glVertex3f(x + width/2.0, y + height/2.0, z);
		glVertex3f(x + width/2.0, y - height/2.0, z);
	glEnd();

	// Border
	glColor3f(outlineColor[0],outlineColor[1],outlineColor[2]);
	glLineWidth(1.0f);

	glBegin(GL_LINE_LOOP);
		glVertex3f(x - width/2.0, y - height/2.0, z);
		glVertex3f(x - width/2.0, y + height/2.0, z);
		glVertex3f(x + width/2.0, y + height/2.0, z);
		glVertex3f(x + width/2.0, y - height/2.0, z);
	glEnd();
}

