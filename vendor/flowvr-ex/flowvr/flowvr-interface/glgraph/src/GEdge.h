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
#ifndef GEDGE_H_
#define GEDGE_H_

#include "adltree.h"
#include "bspline.h"

#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif


#include <vector>
#include <string>

#include "utils.h"

class GEdge {
public:
	GEdge(std::string pos, int depth);

	void init();

	void generateDisplayList() {
		directRenderMode = false;
		edgeDisplayList = glGenLists(1);
		if (edgeDisplayList == 0) {
			directRenderMode = true;
			return;
		}
		glNewList(edgeDisplayList, GL_COMPILE);
			glBegin(GL_LINE_STRIP);
				vec3f p;
				for (std::vector<vec3f>::iterator v=generatedPoints.begin(); v != generatedPoints.end(); v++ ) {
					p = (*v);
					glVertex3f( p.pos[0], p.pos[1], depth+1 );
				}
			glEnd();

			std::vector<vec3f>::iterator end = points.end();
			vec3f pt = (*end);

			vec3f v1 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowRectUVect, 4.5);
			vec3f v2 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowRectUVect, -4.5);
			vec3f v3 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowDirUVect, 15.0);

			glBegin( GL_TRIANGLES);
				glVertex3f(v1.pos[0], v1.pos[1], depth+1);
				glVertex3f(v2.pos[0], v2.pos[1], depth+1);
				glVertex3f(v3.pos[0], v3.pos[1], depth+1);
			glEnd();

		glEndList();
	}

	void draw();

	std::vector<vec3f> points;
	std::vector<vec3f> generatedPoints;
	vec3f arrowDirUVect;
	vec3f arrowRectUVect;

	// Bspline generation


	int depth;
	std::string pos;

	GLuint edgeDisplayList;
	bool directRenderMode;
};

#endif /* GEDGE_H_ */
