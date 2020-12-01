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

#ifndef PRIMITIVENODE_H_
#define PRIMITIVENODE_H_

#include "GNode.h"


class PrimitiveNode : public GNode {
public:
	PrimitiveNode(float x, float y, float z, float width, float height, std::string label, Component* component, int r=168, int g=130, int b=255, int o_r=0, int o_g=0, int o_b=0) :
		GNode(x,y,z,width,height,label,r,g,b,o_r,o_g,o_b,component) {}

	void labelHandling();

	void generateDisplayList() {
		directRenderMode = false;
		nodeDisplayList = glGenLists(1);
		if(nodeDisplayList == 0) {
			directRenderMode = true;
		}
		float d = 0.1 * this->height;
		glNewList(nodeDisplayList, GL_COMPILE);

			glColor3f(fillColor[0], fillColor[1], fillColor[2]);

			glBegin( GL_POLYGON);
				glVertex3f(this->x - this->width / 2.0f + d, this->y - this->height / 2.0f, z);

				glVertex3f(this->x + this->width / 2.0f - d,
						this->y - this->height / 2.0f, z);

				glVertex3f(this->x + this->width / 2.0f,
						this->y - this->height / 2.0f + d, z);

				glVertex3f(this->x + this->width / 2.0f,
						this->y + this->height / 2.0f - d, z);

				glVertex3f(this->x + this->width / 2.0f - d,
						this->y + this->height / 2.0f, z);

				glVertex3f(this->x - this->width / 2.0f + d,
						this->y + this->height / 2.0f, z);

				glVertex3f(this->x - this->width / 2.0f,
						this->y + this->height / 2.0f - d, z);

				glVertex3f(this->x - this->width / 2.0f,
						this->y - this->height / 2.0f + d, z);
			glEnd();

			glColor3f(outlineColor[0],outlineColor[1],outlineColor[2]);
			glLineWidth( 1.0f);
			glBegin( GL_LINE_LOOP);

				glVertex3f(this->x - this->width / 2.0f + d,
						this->y - this->height / 2.0f, z+1);

				glVertex3f(this->x + this->width / 2.0f - d,
						this->y - this->height / 2.0f, z+1);

				glVertex3f(this->x + this->width / 2.0f,
						this->y - this->height / 2.0f + d, z+1);

				glVertex3f(this->x + this->width / 2.0f,
						this->y + this->height / 2.0f - d, z+1);

				glVertex3f(this->x + this->width / 2.0f - d,
						this->y + this->height / 2.0f, z+1);

				glVertex3f(this->x - this->width / 2.0f + d,
						this->y + this->height / 2.0f, z+1);

				glVertex3f(this->x - this->width / 2.0f,
						this->y + this->height / 2.0f - d, z+1);

				glVertex3f(this->x - this->width / 2.0f,
						this->y - this->height / 2.0f + d, z+1);

			glEnd();

			labelHandling();

		glEndList();
	}

	void draw();
};

#endif /* PRIMITIVENODE_H_ */
