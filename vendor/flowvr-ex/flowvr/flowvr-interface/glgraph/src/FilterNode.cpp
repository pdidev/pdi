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

#include "FilterNode.h"
#include "utils.h"


#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif


void FilterNode::draw() {
	if (!directRenderMode) {
		glCallList(nodeDisplayList);
	} else {
		glCallList(nodeDisplayList);
		// Diamond
		// Inside
		glColor3f(fillColor[0],fillColor[1],fillColor[2]);

		glBegin(GL_QUADS);
			glVertex3f(this->x, this->y + this->height / 2.0f, z);
			glVertex3f(this->x + this->width / 2.0f, this->y, z);
			glVertex3f(this->x, this->y - this->height / 2.0f, z);
			glVertex3f(this->x - this->width / 2.0f, this->y, z);
		glEnd();


		// Border
		glColor3f(outlineColor[0],outlineColor[1],outlineColor[2]);
		glLineWidth(1.0f);

		glBegin(GL_LINE_LOOP);
			glVertex3f(this->x, this->y + this->height / 2.0f, z+1);
			glVertex3f(this->x + this->width / 2.0f, this->y, z+1);
			glVertex3f(this->x, this->y - this->height / 2.0f, z+1);
			glVertex3f(this->x - this->width / 2.0f, this->y, z+1);
		glEnd();

		renderStrokeLineFit(x, y, z+1, toChar(label), width*0.55, height*0.8);
	}
}
