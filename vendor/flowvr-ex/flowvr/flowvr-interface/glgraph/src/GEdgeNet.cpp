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
* File: ./src/GEdge.cpp                                           *
*                                                                 *
* Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
*                                                                 *
******************************************************************/
#include "../../QGLViewer/qglviewer.h"

#include <iostream>
#include <cstdlib>
#include "settings.h"
#include "utils.h"
#include "GEdgeNet.h"


using namespace std;

void GEdgeNet::set_visible(bool visible)
{
   this->visible = visible;
}
string* GEdgeNet::get_label()
{
   return &(this->label);
}
int GEdgeNet::get_num()
{
   return this->num;
}

void GEdgeNet::draw(bool fast, int color_mode, bool and_visible)
{
   int i;


   // Drawing the spline
   float alpha;
   if (this->visible && and_visible)
      alpha = 1.0f;
   else
      alpha = ALPHA_INVISIBLE;

   if (!super_visible)
      glLineWidth(1.0f);
   else
      glLineWidth(SELECTED_EDGE_WIDTH);

   if (this->style == EDGE_STYLE_DASHED)
   {
      glEnable(GL_LINE_STIPPLE);
      glLineStipple(2, 0x0303);
   }
   if (color_mode == 2)
   {
      glColor4f(color_rgba[0], color_rgba[1], color_rgba[2], alpha);
   }
   else
   {
      if (this->style == EDGE_STYLE_DASHED)
         glColor4f(0.4f, 0.4f, 0.4f, alpha);
      else
         glColor4f(0.0f, 0.0f, 0.0f, alpha);
   }

   glBegin(GL_LINE_STRIP);
		vec3f p;
		for (std::vector<vec3f>::iterator v=generatedPoints.begin(); v != generatedPoints.end(); v++ ) {
			p = (*v);
			glVertex3f( p.pos[0], p.pos[1], 0);
		}
	glEnd();
   glDisable(GL_LINE_STIPPLE);

	std::vector<vec3f>::iterator end = points.end();
	vec3f pt = (*end);

	vec3f v1 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowRectUVect, 4.5);
	vec3f v2 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowRectUVect, -4.5);
	vec3f v3 = addVectorToPos(pt.pos[0], pt.pos[1], pt.pos[2], arrowDirUVect, 15.0);

	glBegin( GL_TRIANGLES);
		glVertex3f(v1.pos[0], v1.pos[1], 0);
		glVertex3f(v2.pos[0], v2.pos[1], 0);
		glVertex3f(v3.pos[0], v3.pos[1], 0);
	glEnd();



   if(message_t >= 0 && message_t <= 1) {
     int npt = generatedPoints.size();
     double t = message_t * (npt - 1); 
     int i = (int)floor(t); 
     double f = t - i; 
     vec3f p0 = generatedPoints[i], p1 = generatedPoints[(i + 1) % npt];
     float x = p0.pos[0] * (1 - f) + p1.pos[0] * f;  
     float y = p0.pos[1] * (1 - f) + p1.pos[1] * f;  
     float side = 6.0; 

     glColor3f(1.0, 0.0, 0.0);
     
     glBegin(GL_POLYGON); 
		glVertex2f(x - side, y - side);
		glVertex2f(x + side, y - side);
		glVertex2f(x + side, y + side);
		glVertex2f(x - side, y + side);
     glEnd(); 
     
   }



   /*
   glBegin(GL_LINES);
   for (i=0; i<this->nb_points-1; i++)
   {
      glVertex2f( this->points[i].x,   this->points[i].y);
      glVertex2f( this->points[i+1].x, this->points[i+1].y);
   }
   glEnd();

   glDisable(GL_LINE_STIPPLE);


   // Drawing the arrow

   glBegin(GL_TRIANGLES);
      glVertex2f( this->fleche.x ,   this->fleche.y );
      glVertex2f( -(this->fleche.y-this->points[i].y)*0.5f+this->points[i].x ,
                   (this->fleche.x-this->points[i].x)*0.5f+this->points[i].y );
      glVertex2f(  (this->fleche.y-this->points[i].y)*0.5f+this->points[i].x ,
                  -(this->fleche.x-this->points[i].x)*0.5f+this->points[i].y );
   glEnd();

   if (fast) return;
   */
}

GEdgeNet::GEdgeNet(std::string pos, int style, std::string label, int num, unsigned char r, unsigned char g, unsigned char b, unsigned char a)
{
   this->visible = true;
   this->super_visible = false;
   this->style = style;
   this->label = label;
   this->num = num;
   this->color_rgba[0] = (float)r/255.0f;
   this->color_rgba[1] = (float)g/255.0f;
   this->color_rgba[2] = (float)b/255.0f;
   this->color_rgba[3] = (float)a/255.0f;
   this->message_t = -1;

    //####################################################
    //	Spline parsing developped for hierarchical version
   	//####################################################

	// Cut the pos string into "x,y" pieces
	string str2 = pos;
	string buf; // Have a buffer string
	stringstream ss(str2); // Insert the string into a stream

	vector <string> tokens; // Create vector to hold our words

	while (ss >> buf)
		tokens.push_back(buf);


	size_t found;
	// Cut every "x,y" piece into x and y
	for (vector<string>::iterator piece=tokens.begin(); piece != tokens.end(); piece++) {
		// Replace ','
		found = (*piece).find_first_of(",");
		while (found != string::npos) {
			(*piece)[found] = ' ';
			found = (*piece).find_first_of(",", found + 1);
		}

		string str3 = (*piece);
		string buf2; // Have a buffer string
		stringstream ss2(str3); // Insert the string into a stream

		vector < string > tokens2; // Create vector to hold our words
		while (ss2 >> buf2)
				tokens2.push_back(buf2);
		vec3f vec;
		vec.pos[0] = string2float(tokens2[0]);
		vec.pos[1] = string2float(tokens2[1]);
		vec.pos[2] = 0;

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

	// direction unit vector
	// let's find ||aDir||
	float lengthADir = sqrt(aDir.pos[0]*aDir.pos[0] + aDir.pos[1]*aDir.pos[1]);

	// aDir unit vector
	vec3f aDirUVect;
	aDirUVect.pos[0] = aDir.pos[0]/lengthADir;
	aDirUVect.pos[1] = aDir.pos[1]/lengthADir;
	aDirUVect.pos[2] = 0;

	arrowDirUVect = aDirUVect;

	// 90 degree vector
	vec3f rectV;
	rectV.pos[0] = -aDir.pos[1];
	rectV.pos[1] = aDir.pos[0];
	rectV.pos[2] = 0;

	// Compute rect unit vector
	// Let's find ||rectV||
	float lengthRectV = sqrt(rectV.pos[0]*rectV.pos[0] + rectV.pos[1]*rectV.pos[1]);

	// rect Unit vector
	vec3f rectUVect;
	rectUVect.pos[0] = rectV.pos[0]/lengthRectV;
	rectUVect.pos[1] = rectV.pos[1]/lengthRectV;
	rectUVect.pos[2] = 0;

	arrowRectUVect = rectUVect;

	// B-spline generation
	point_spline* pts_control = new point_spline[points.size()];
	int nbOutputPts = points.size()*10;


	// Fill control point structure
	int i=0;
	for (vector<vec3f>::iterator point = points.begin(); point != points.end(); point++, i++) {
		pts_control[i].x = (*point).pos[0];
		pts_control[i].y = (*point).pos[1];
		pts_control[i].z = (*point).pos[2];
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

GEdgeNet::~GEdgeNet()
{
}


void GEdgeNet::set_message_t(double t) {
  message_t = t;
}
