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
 * File: ./src/GEdgeNet.h                                          *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#ifndef GEDGENET_H
#define GEDGENET_H

#include <list>
#include <string>

#include "settings.h"
#include "utils.h"
#include "bspline.h"

#define EDGE_STYLE_DEFAULT 0
#define EDGE_STYLE_DASHED 1


class GVertexNet;

/*

 Classe GEdgeNet

 */

class GEdgeNet {

private:
	int nb_points;


	std::vector<vec3f> points;
	std::vector<vec3f> generatedPoints;

	vec3f arrowRectUVect;
	vec3f arrowDirUVect;


	bool visible;
	string label;
	float color_rgba[4];

        double message_t;  // offset on the arrow of a message

public:
	int num;
	int style;
	void* graphviz_e;
	GVertexNet* vertex_head;
	GVertexNet* vertex_tail;
	bool super_visible;

	void set_visible(bool visible);
        void set_message_t(double t); 

	string* get_label();
	void draw(bool fast, int color_mode, bool and_visible = true);

	int get_num();

	GEdgeNet(std::string pos, int style, string label, int num,
			unsigned char r = 0, unsigned char g = 0, unsigned char b = 0,
			unsigned char a = 255);

	~GEdgeNet();
};

/*

 Liste chain�es d'edges

 */
struct liste_edges {
	GEdgeNet* edge;
	liste_edges* suiv;
};

#endif
