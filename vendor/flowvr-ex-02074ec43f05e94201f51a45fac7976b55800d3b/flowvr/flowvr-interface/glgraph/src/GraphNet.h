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
 * File: ./src/GraphNet.h                                            *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#ifndef GGRAPH_H
#define GGRAPH_H

#include <iostream>
#include <fstream>
#include <string>

#if USING_CGRAPH
#include <gvc.h>
#include <cgraph.h>
#endif

#include "settings.h"
#include "GEdgeNet.h"
#include "GVertexNet.h"

#define MODE_EXPLO_IN     1
#define MODE_EXPLO_OUT    2
#define MODE_EXPLO_IN_OUT 3

#define TYPE_VERTEX 0
#define TYPE_EDGE 1

struct VertexOrEdge {
	void* voe;
	int type; // -1=nothing, 0=vertex, 1=edge
};

/*

 Classe GraphNet

 */
#if !USING_CGRAPH
struct Agraph_t;
#endif
struct Traces; 

class GraphNet {

private:
	int selected_num;
	int nb_visible;
	liste_vertices* l_vertices;
	liste_edges* l_edges;

	void apply_visibility_depth(int depth);
	void add_vertex(GVertexNet* vertex);
	void add_edge(GEdgeNet* edge);
	GVertexNet* find_vertex(string id);
	void convert_inche2pix(float dpi_x, float dpi_y);

public:
	float bounding_right, bounding_left, bounding_top, bounding_bottom;
	void* graphviz_g;

	GVertexNet* find_vertex_by_num(int num);
	GEdgeNet* find_edge_by_num(int num);
	VertexOrEdge find_by_num(int num);

	GraphNet(Agraph_t* g);
	~GraphNet();

	int find_shape(int num);
	int find_host(int num);
	void draw_zoomed_vertex(int color_mode, float* shape_color,
			float* host_color, GVertexNet* zoomed_vertex);
	void zoom_vertex(int num);
	void change_vertex_color(int num, float r, float g, float b);
	bool draw(bool fast, int color_mode, bool only_distant_conn,
                  float* shape_color, float* host_color,
                  GVertexNet* zoomed_vertex = NULL, GEdgeNet* zoomed_edge = NULL, 
                  Traces * traces = NULL);
	void select(std::list<GVertexNet*> *selected_vertices, int &niveau_exploration,
			int mode_exploration, bool maxdepth);
	void select(std::list<VertexOrEdge> *selected_vertices, int &niveau_exploration,
			int mode_exploration, bool maxdepth);
	void get_bounding_box(float* left, float* right, float* top, float* bottom);
	liste_vertices* get_vertices();
	liste_edges* get_edges();
};
#endif
