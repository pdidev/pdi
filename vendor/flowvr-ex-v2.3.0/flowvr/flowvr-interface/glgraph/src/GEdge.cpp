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

#include "GEdge.h"

#include "utils.h"

#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif

#include <sstream>
#include <string>
#include <math.h>

using namespace std;

GEdge::GEdge(string pos, int depth) :
	pos(pos), depth(depth+50), edgeDisplayList(0), directRenderMode(true) {
}

void GEdge::init() {

	/*
	found = pos.find_first_of(",");
	while (found != string::npos) {
		pos[found] = ' ';
		found = str.find_first_of(",", found + 1);
	}
	*/

	// Cut the pos string into "x,y" pieces
	string str2 = pos;
	string buf; // Have a buffer string
	stringstream ss(str2); // Insert the string into a stream

	vector < string > tokens; // Create vector to hold our words

	while (ss >> buf)
		tokens.push_back(buf);



	size_t found;
	// Cut every "x,y" piece into x and y
	for (vector<string>::iterator p=tokens.begin(); p != tokens.end(); p++) {
		// Replace ','
		found = (*p).find_first_of(",");
		while (found != string::npos) {
			(*p)[found] = ' ';
			found = (*p).find_first_of(",", found + 1);
		}

		string str3 = (*p);
		string buf2; // Have a buffer string
		stringstream ss2(str3); // Insert the string into a stream

		vector < string > tokens2; // Create vector to hold our words
		while (ss2 >> buf2)
				tokens2.push_back(buf2);
		vec3f vec;
		vec.pos[0] = string2float(tokens2[0]);
		vec.pos[1] = string2float(tokens2[1]);
		vec.pos[2] = depth;

		points.push_back(vec);
	}

	vector<vec3f>::iterator v=points.begin();
	points.erase(v);

	vector<vec3f>::iterator end = points.end();
	end = end-1;
	vec3f pt = (*end);

	end = end-1;

	vec3f ptMinOne = (*end);

	// Compute arrow triangle
	// arrow direction
	vec3f aDir;
	aDir.pos[0] = pt.pos[0] - ptMinOne.pos[0];
	aDir.pos[1] = pt.pos[1] - ptMinOne.pos[1];
	aDir.pos[2] = 0;
	// {aDir == direction arrow}

	// direction unit vector
	// let's find ||aDir||
	float lengthADir = sqrt(aDir.pos[0]*aDir.pos[0] + aDir.pos[1]*aDir.pos[1]);

	// aDir unit vector
	vec3f aDirUVect;
	aDirUVect.pos[0] = aDir.pos[0]/lengthADir;
	aDirUVect.pos[1] = aDir.pos[1]/lengthADir;
	aDirUVect.pos[2] = 0;

	arrowDirUVect = aDirUVect;


	// Compute rect unit vector
	// Let's find ||rectV||
	// 90 degree vector of aDirUVect
	vec3f rectUVect;
	rectUVect.pos[0] = -aDirUVect.pos[1];
	rectUVect.pos[1] = aDirUVect.pos[0];
	rectUVect.pos[2] = 0;

	arrowRectUVect = rectUVect;

	/*
	 * tony z wrote:
	 > I am trying to use graphviz output for layout in my software. how can
	 > I use the control points to draw b-spline curve.
	 > for example, I have following 10 control points from graphviz xdot:
	 > 246 504 246 483 248 451 266 432 306 389 338 417 393 396 395 395 397
	 > 394 399 393
	 > in my software, there is a lib to draw cubic beziers by taking 4
	 > control points, how should I used the lib to draw the b-spline curve
	 > described by above 10 control points? Any help is appreciated.
	 The short answer, since you have a cubic bezier function, is

	 bezier 246 504 246 483 248 451 266 432
	 bezier  266 432 306 389 338
	 417 393 396

	 bezier 393 396 395 395 397 394 399 393

	 that is, take 4 points at a time, overlapping by one point.
	 */

	// B-spline generation
	point_spline* pts_control = new point_spline[points.size()];
	int nbOutputPts = points.size()*10;


	// Fill control point structure
	int i=0;
	for (vector<vec3f>::iterator p = points.begin(); p != points.end(); p++, i++) {
		pts_control[i].x = (*p).pos[0];
		pts_control[i].y = (*p).pos[1];
		pts_control[i].z = (*p).pos[2];
	}
	// {structure filled}

	point_spline* pts_output = new point_spline[nbOutputPts];
	bspline(points.size()-1, 4, pts_control, pts_output, nbOutputPts);

	// Fill edge vector
	for (int j=0; j<nbOutputPts; j++) {
		vec3f newPoint;
		newPoint.pos[0] = pts_output[j].x;
		newPoint.pos[1] = pts_output[j].y;
		newPoint.pos[2] = 0;
		generatedPoints.push_back(newPoint);
	}
	// {vector filled}

	delete [] pts_output;
	delete [] pts_control;
}

void GEdge::draw() {
	if (!directRenderMode) {
		glCallList(edgeDisplayList);
	} else {
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

	}
}
