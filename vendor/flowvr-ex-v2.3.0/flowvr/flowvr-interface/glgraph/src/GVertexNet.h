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
 * File: ./src/GVertexNet.h                                           *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#ifndef GVERTEX_H
#define GVERTEX_H

#include <stdlib.h>
#include <string>
#include <list>
#include "QGLViewer/qglviewer.h"

#include "settings.h"


class GVertexNet;

/**
 * List of vertices
 */
struct liste_vertices {
	GVertexNet* vertex;
	liste_vertices* suiv;
};

struct s_record {
	float LLx, LLy, URx, URy;
	char* text;
	int corner;
	int line;
};

/**
 * GVertexNet class
 */
class GVertexNet {

private:
	float pos_x;
	float pos_y;
	std::string host;
	std::string id;
	int num; // id given to glPushName()
	bool visible;

	void draw_box(bool fast, int color_mode, float alpha, float* color,
			float* host_color, bool and_visible = true);
	void draw_diamond(bool fast, int color_mode, float alpha, float* color,
			float* host_color, bool and_visible = true);
	void draw_circle(bool fast, int color_mode, float alpha, float* color,
			float* host_color, bool and_visible = true);
	void draw_mrecord(bool fast, int color_mode, float alpha, float* color,
			float* host_color, bool and_visible = true);

public:

	std::string label;
	int host_id;
	int shape;
	float color_net[4];
	bool selected;
	float width;
	float height;
	bool fmm_accepted;
	bool is_small; // true => the node will not be taken into account in the depth visualisation
	int depth;
	std::list<s_record>* record_info;
	void* graphviz_n;
	std::list<void*> edges_in;
	std::list<void*> edges_out;

	float get_pos_x();
	float get_pos_y();
	std::string* get_id();
	std::string* get_host();
	int get_num();
	void draw(bool fast, int color_mode, float* shape_color, float* host_color,
			bool and_visible = true);
	void set_selected(bool sel);
	bool get_visible();
	void set_visible(bool visible);

	GVertexNet(float posx, float posy, float width, float height,
			std::string label, std::string host, int shape,
			std::list<s_record>* record_info, unsigned char r, unsigned char g,
			unsigned char b, unsigned char a, std::string id, int num);
	~GVertexNet();
};
#endif
