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

// *** NOTICE ***
//
// This class isn't mapped to anything.
// It is simply here if you wish to implement routing node rendering.
// If you wish to do so, you will have to operate on the whole content pipeline,
// starting with the adltree generation.
//
// *** NOTICE ***


#ifndef ROUTINGNODE_H_
#define ROUTINGNODE_H_

#include "GNode.h"

class RoutingNode : public GNode {
public:
	RoutingNode(float x, float y, float z, float width, float height, std::string label, int r=255, int g=255, int b=255, int o_r=0, int o_g=0, int o_b=0) :
		GNode(x,y,z,width,height,label,r,g,b,o_r,o_g,o_b) {}

	void draw();
private:

};

#endif /* ROUTINGNODE_H_ */
