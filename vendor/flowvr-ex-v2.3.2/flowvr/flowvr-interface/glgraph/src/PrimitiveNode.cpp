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

#include "PrimitiveNode.h"

#include "adltree.h"
#include <list>
#include <string>




#ifdef __APPLE__
# include <OpenGL/gl.h>
#else
# include <GL/gl.h>
#endif


using namespace std;

void PrimitiveNode::labelHandling() {
	glColor3f(0.f, 0.f, 0.f);
	// Line separators
	glBegin(GL_LINES);
		glVertex3f(x-width/2, y+height/4, z+1);
		glVertex3f(x+width/2, y+height/4, z+1);
	glEnd();

	glBegin(GL_LINES);
		glVertex3f(x-width/2, y, z+1);
		glVertex3f(x+width/2, y, z+1);
	glEnd();

	glBegin(GL_LINES);
		glVertex3f(x-width/2, y-height/4, z+1);
		glVertex3f(x+width/2, y-height/4, z+1);
	glEnd();


	int inputCharLen = 0;
	int outputCharLen = 0;
	for (list<Port*>::iterator p = component->ports.begin(); p != component->ports.end(); p++) {
		string s = (string)(*p)->id;

		/*
			Shows blockstate information.
			If you uncomment this portion, you should uncomment those below.
		*/
                /*
		if ( strContains((char*) (*p)->type, "INPUT") )
		{
			s.append(": ");
			if( !(*p)->blockstate.empty())
				s.append((*p)->blockstate);
                        else
    				s.append( "UNKNOWN" );
		}
                */

		if (strContains((char*) (*p)->type, "INPUT")) {
			inputCharLen += s.length();
		} else if (strContains((char*) (*p)->type, "OUTPUT")) {
			outputCharLen += s.length();
		}
	}
	// {inputCharLen and outputCharLen computed}


	// Ports
	int currentInputCharLen = 0;
	int currentOutputCharLen = 0;
	for (list<Port*>::iterator p = component->ports.begin(); p != component->ports.end(); p++) {
		string s = (string)(*p)->id;

		/*
			Blockstate information

		if (!(*p)->blockstate.empty()) {
			s.append(": ");
			s.append((*p)->blockstate);
		}
		*/
		if (strContains((char*) (*p)->type, "INPUT")) {
                    /*
			s.append(": ");
			if( !(*p)->blockstate.empty())
			   s.append((*p)->blockstate);
                        else
                           s.append( "UNKNOWN" );
                    */

			currentInputCharLen += s.length();

			renderStrokeLineFit(  (x-width/2)+width*((float)currentInputCharLen/inputCharLen)-width*((float(s.length()) / inputCharLen  )/2.0) , y+(height/4)+(height/8), z+1, toChar(s), width*(float(s.length())/float(inputCharLen)), height/4);
			// Right border
			if (currentInputCharLen != inputCharLen) {
				glBegin(GL_LINES);
					glVertex3f((x-width/2)+width*((float)currentInputCharLen/(float)inputCharLen), y+height/2, z+1);
					glVertex3f((x-width/2)+width*((float)currentInputCharLen/(float)inputCharLen), y+height/4, z+1);
				glEnd();
			}
		} else if (strContains((char*) (*p)->type, "OUTPUT")) {
			currentOutputCharLen += s.length();

			renderStrokeLineFit(  (x-width/2)+width*((float)currentOutputCharLen/outputCharLen)-width*((float(s.length()) / outputCharLen  )/2.0) , y-(height/4)-(height/8), z+1, toChar(s), width*(float(s.length())/float(outputCharLen)), height/4);
			// Right border
			if (currentOutputCharLen != outputCharLen) {
				glBegin(GL_LINES);
					glVertex3f((x-width/2)+width*((float)currentOutputCharLen/(float)outputCharLen), y-height/2, z+1);
					glVertex3f((x-width/2)+width*((float)currentOutputCharLen/(float)outputCharLen), y-height/4, z+1);
				glEnd();
			}
		}
	}

	// Node name
	renderStrokeLineFit( x, y+height/8, z+1, component->label, width, height/4 );

	// Hosts
	renderStrokeLineFit( x, y-height/8, z+1, component->hostLabel, width, height/4 );
}

void PrimitiveNode::draw() {
	if (!directRenderMode) {
			glCallList(nodeDisplayList);
	} else {

		float d = 0.1 * this->height;
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
	}


}










