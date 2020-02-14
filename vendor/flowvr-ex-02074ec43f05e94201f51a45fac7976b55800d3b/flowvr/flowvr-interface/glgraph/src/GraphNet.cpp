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
 * File: ./src/GraphNet.cpp                                          *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include <math.h>
#include "../../QGLViewer/qglviewer.h"
#include <color.h>

#include "GraphNet.h"
#include "utils.h"
#include "traces.h"

#if !USING_CGRAPH
#include <gvc.h>
#endif

#include <vector>

#define INFINITE 10000

extern "C" int
		colorxlate(char *str, gvcolor_t *color, color_type_t target_type);

using namespace std;

liste_vertices* GraphNet::get_vertices() {
	return this->l_vertices;
}

liste_edges* GraphNet::get_edges() {
	return this->l_edges;
}

/**
 * Add a vertex to de vertices list of the graph
 */
void GraphNet::add_vertex(GVertexNet* vertex) {
	liste_vertices* new_vertex = (liste_vertices*) malloc(
			sizeof(liste_vertices));
	new_vertex->suiv = this->l_vertices;
	new_vertex->vertex = vertex;
	this->l_vertices = new_vertex;
}

/**
 * Add an edge to the edges list of the graph.
 */
void GraphNet::add_edge(GEdgeNet* edge) {
	liste_edges* new_edge = (liste_edges*) malloc(sizeof(liste_edges));
	new_edge->suiv = this->l_edges;
	new_edge->edge = edge;
	this->l_edges = new_edge;
}

/**
 * Return the graph bounding box;
 */
void GraphNet::get_bounding_box(float* left, float* right, float* top,
		float* bottom) {
	*left = 900000000.0f;
	*right = -1.0f;
	*bottom = 900000000.0f;
	*top = -1.0f;

	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {
		if (cur_vertex->vertex->get_pos_x() < *left)
			*left = cur_vertex->vertex->get_pos_x();
		if (cur_vertex->vertex->get_pos_x() > *right)
			*right = cur_vertex->vertex->get_pos_x();
		if (cur_vertex->vertex->get_pos_y() < *bottom)
			*bottom = cur_vertex->vertex->get_pos_y();
		if (cur_vertex->vertex->get_pos_y() > *top)
			*top = cur_vertex->vertex->get_pos_y();

		cur_vertex = cur_vertex->suiv;
	}
}

/**
 * GraphNet Destructor.
 */
GraphNet::~GraphNet() {
	// Free the vertices

	liste_vertices* cur_vertex = this->get_vertices();
	liste_vertices* suiv_vertex;

	while (cur_vertex != NULL) {
		suiv_vertex = cur_vertex->suiv;
		delete (cur_vertex->vertex);
		free(cur_vertex);
		cur_vertex = suiv_vertex;
	}

	// Free the edges

	liste_edges* cur_edge = this->get_edges();
	liste_edges* suiv_edge;

	while (cur_edge != NULL) {
		suiv_edge = cur_edge->suiv;
		delete (cur_edge->edge);
		free(cur_edge);
		cur_edge = suiv_edge;
	}
}

/**
 * Find the vertex matching with the given id
 */
GVertexNet* GraphNet::find_vertex(string id) {
	liste_vertices* cur_vertex = this->l_vertices;
	while (cur_vertex != NULL) {
		if (*(cur_vertex->vertex->get_id()) == id)
			return cur_vertex->vertex;
		cur_vertex = cur_vertex->suiv;
	}

	return NULL;
}

/**
 * Set corner=1 to the corners of the records.
 * This attribute is used for the drawing.
 */
void find_record_corners(list<s_record>* record_info, int* pix_width,
		int* pix_height) {
	float x_min = +900000.0f;
	float y_min = +900000.0f;
	float x_max = -900000.0f;
	float y_max = -900000.0f;

	list<s_record>::iterator ite;

	for (ite = record_info->begin(); ite != record_info->end(); ite++) {
		if ((*ite).LLx < x_min)
			x_min = (*ite).LLx;
		if ((*ite).LLy < y_min)
			y_min = (*ite).LLy;
		if ((*ite).URx > x_max)
			x_max = (*ite).URx;
		if ((*ite).URy > y_max)
			y_max = (*ite).URy;
	}

	for (ite = record_info->begin(); ite != record_info->end(); ite++) {
		if (((*ite).LLx <= x_min) && ((*ite).LLy <= y_min))
			(*ite).corner = 0;
		else if (((*ite).URx >= x_max) && ((*ite).LLy <= y_min))
			(*ite).corner = 1;
		else if (((*ite).LLx <= x_min) && ((*ite).URy >= y_max))
			(*ite).corner = 2;
		else if (((*ite).URx >= x_max) && ((*ite).URy >= y_max))
			(*ite).corner = 3;
		else
			(*ite).corner = -1;
	}

	*pix_width = (int) floor(x_max - x_min);
	*pix_height = (int) floor(y_max - y_min);
}

/**
 * Store recursively the record description.
 */
void store_record_info(field_t* record_info, list<s_record>* sr_list, int line) {
	if (record_info->n_flds == 0) {
		s_record sr;
		sr.LLx = record_info->b.LL.x;
		sr.LLy = record_info->b.LL.y;
		sr.URx = record_info->b.UR.x;
		sr.URy = record_info->b.UR.y;
		sr.corner = -1;
		sr.line = line;
		sr.text = record_info->lp->text;
		sr_list->push_back(sr);
	} else {
		for (int i = 0; i < record_info->n_flds; i++) {
			if (line == -1)
				store_record_info(record_info->fld[i], sr_list, -2);
			else if (line == -2)
				store_record_info(record_info->fld[i], sr_list, i);
			else
				store_record_info(record_info->fld[i], sr_list, line);
		}
	}
}

void GraphNet::convert_inche2pix(float dpi_x, float dpi_y) {
	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {
		glPushName(cur_vertex->vertex->get_num());
		cur_vertex->vertex->width = round(cur_vertex->vertex->width * dpi_x);
		cur_vertex->vertex->height = round(cur_vertex->vertex->height * dpi_y);

		cur_vertex = cur_vertex->suiv;
	}
}

void parsePosPointers(string str, float* posx, float* posy) {
	if (str.empty()) {
		(*posx) = 0;
		(*posy) = 0;
		return;
	}

	size_t found;

	found = str.find_first_of(",");
	while (found != string::npos) {
		str[found] = ' ';
		found = str.find_first_of(",", found + 1);
	}

    string str2 = str;
    string buf; // Have a buffer string
    stringstream ss(str2); // Insert the string into a stream

    vector<string> tokens; // Create vector to hold our words

    while (ss >> buf)
        tokens.push_back(buf);

	(*posx) = atoi(tokens[0].c_str());
	(*posy) = atoi(tokens[1].c_str());
}

/**
 * Constructor.
 * Browse the Graphviz structure, create and fill our own graph representation
 */
GraphNet::GraphNet(Agraph_t* g) {
	this->selected_num = -1;
	this->graphviz_g = (void*) g;
	point_spline* pts = new point_spline[500];
	point_spline fleche = { 0.0f, 0.0f, 0.0f };
	this->l_vertices = NULL;
	this->l_edges = NULL;
	int num = 1;
	Agnode_t* n;
	string id;
	float dpi_x = -1.0f;
	float dpi_y = -1.0f;

	// For each vertex of g
	for (n = agfstnode(g); n; n = agnxtnode(g, n)) {
		// attribute [id]

		if (agget(n, (char*) "id") != NULL)
			id = agget(n, (char*) "id");
		else
			id = "";

		// attribute [posx] [posy]

		string str_pos(agget(n, (char*) "pos"));
		float posx = 0.0f;
		float posy = 0.0f;
//		unsigned int t;
//		for (t = 0; (str_pos[t] != ',') && (t < str_pos.length()); t++) {
//			posx *= 10.0f;
//			posx += (float) (str_pos[t] - '0');
//		}
//
//		if (t < str_pos.length()) {
//			t++;
//		}
//
//		for (t; (str_pos[t] != ',') && (t < str_pos.length()); t++) {
//			posy *= 10.0f;
//			posy += (float) (str_pos[t] - '0');
//		}
//
//		posy = 0;

		parsePosPointers(str_pos, &posx, &posy);

		// attribute [width]

		string str_width(agget(n, (char*) "width"));
		float width = string2float(str_width);

		// attribute [height]

		string str_height(agget(n, (char*) "height"));
		float height = string2float(str_height);

		// attribute [label]

		string label = "";
		if (agget(n, (char*) "label") != NULL)
			label = agget(n, (char*) "label");

		// attribute [host]

		string host = "";
		if (agget(n, (char*) "host") != NULL)
			host = agget(n, (char*) "host");

		// attribute [color_net]

		char* color_net = agget(n, (char*) "color");
		gvcolor_t vc;
		if (color_net != NULL)
			colorxlate(color_net, &vc, RGBA_BYTE);
		else {
			vc.u.rgba[0] = 255;
			vc.u.rgba[1] = 255;
			vc.u.rgba[2] = 255;
			vc.u.rgba[3] = 255;
		}

		// attribute [shape]

		int shape;
		string str_shape;
		if (agget(n, (char*) "shape") != NULL) {
			str_shape = agget(n, (char*) "shape");
			if (str_shape == "box") {
				shape = SHAPE_BOX;
			} else if (str_shape == "diamond") {
				shape = SHAPE_DIAMOND;
			} else if (str_shape == "circle") {
				shape = SHAPE_CIRCLE;
			} else if (str_shape == "Mrecord") {
				shape = SHAPE_MRECORD;
			} else {
				shape = SHAPE_BOX;
			}
		} else {
			shape = SHAPE_CIRCLE;
		}

		// Mrecord layout

		list < s_record > *record_info = NULL;
		if (shape == SHAPE_MRECORD) {
			record_info = new list<s_record> ;
			store_record_info((field_t*) ND_shape_info(n), record_info, -1);
			int pix_width, pix_height;
			find_record_corners(record_info, &pix_width, &pix_height);
			dpi_x = round((float) pix_width / width);
			dpi_y = round((float) pix_height / height);
		}

		// Creation of the vertex

		GVertexNet* vertex = new GVertexNet(posx, posy, width, height, label, host,
				shape, record_info, vc.u.rgba[0], vc.u.rgba[1], vc.u.rgba[2],
				vc.u.rgba[3], id, num++);
		add_vertex(vertex);
		vertex->graphviz_n = (void*) n;

		//////////////////////////////////////////////////////////////
		//////////////////////////////////////////////////////////////

		Agedge_t* e;
		// For each node going out of the vertex
		for (e = agfstout(g, n); e; e = agnxtout(g, e)) {

			// Parsing of the spline description

			string str_pos(agget(e, (char*) "pos"));


			/*
			unsigned int p = 0;
			unsigned int p2=0;
			int x, y;

			unsigned int t = 0 + 2; // on passse "e,"
			while (t < str_pos.length()) {
				x = 0;
				for (t = t; (str_pos[t] != ',') && (t < str_pos.length()); t++) {
					x *= 10;
					x += str_pos[t] - '0';
				}
				y = 0;
				for (t++; (str_pos[t] != ' ') && (t < str_pos.length()); t++) {
					y *= 10;
					y += str_pos[t] - '0';
				}
				t++;

				if (str_pos[0] == 'e') {
					if (p > 0) {
						// On ajoute le point dans la liste des points du poly de controle de la spline
						pts[p - 1].x = (float) x;
						pts[p - 1].y = (float) y;
						pts[p - 1].z = 0.0f;
					} else {
						// Le premier point est le bout de la fleche, pas un point du poly de controle de la spline
						fleche.x = (float) x;
						fleche.y = (float) y;
						fleche.z = 0.0f;
					}
				} else if (str_pos[0] == 's') {
					if (p > 0) {
						pts[p - 1].x = (float) x;
						pts[p - 1].y = (float) y;
						pts[p - 1].z = 0.0f;

						if (t >= str_pos.length()) {
							for (p2 = p; p2 <= p + p - 1; p2++) {
								pts[p2].x = pts[p2 - p].x;
								pts[p2].y = pts[p2 - p].y;
								pts[p2].z = pts[p2 - p].z;
							}
							for (p2 = 0; p2 <= p - 1; p2++) {
								pts[p2].x = pts[p + (p - 1) - p2].x;
								pts[p2].y = pts[p + (p - 1) - p2].y;
								pts[p2].z = pts[p + (p - 1) - p2].z;
							}

						}

					} else {
						fleche.x = (float) x;
						fleche.y = (float) y;
						fleche.z = 0.0f;
					}
				} else {
					cout << "Error: edge position encoding non implemented"
							<< endl;
				}
				p++;
			}
			*/

			// attribute [label]

			string label = "";
			if (agget(e, (char*) "label") != NULL)
				label = agget(e, (char*) "label");

			// Attribute [style]

			int style = EDGE_STYLE_DEFAULT;
			if (agget(e, (char*) "style")) {
				string str_style(agget(e, (char*) "style"));
				if (str_style == "dashed") {
					style = EDGE_STYLE_DASHED;
				}
			}

			char* color = agget(e, (char*) "color");
			gvcolor_t c;
			if (color != NULL)
				colorxlate(color, &c, RGBA_BYTE);
			else if (style == EDGE_STYLE_DASHED) {
				c.u.rgba[0] = 100;
				c.u.rgba[1] = 100;
				c.u.rgba[2] = 100;
				c.u.rgba[3] = 255;
			} else {
				c.u.rgba[0] = 0;
				c.u.rgba[1] = 0;
				c.u.rgba[2] = 0;
				c.u.rgba[3] = 255;
			}
			// Creation of the edge

			if (label != "invisible") {
				GEdgeNet* edge = new GEdgeNet(str_pos, style, label,
						num++, c.u.rgba[0], c.u.rgba[1], c.u.rgba[2],
						c.u.rgba[3]);
				add_edge(edge);
				edge->graphviz_e = (void*) e;
			}
		}
	}

	// Storing neighbors lists =>

	liste_edges* cur_edge = this->get_edges();
	while (cur_edge != NULL) {
#if USING_CGRAPH
		GVertexNet* head = find_vertex(string(agget(aghead((Agedge_t*) cur_edge->edge->graphviz_e), (char*) "id")));
		GVertexNet* tail = find_vertex(string(agget(agtail((Agedge_t*) cur_edge->edge->graphviz_e),(char*) "id")));
#else
		GVertexNet* head = find_vertex(string(agget(((Agedge_t*) cur_edge->edge->graphviz_e)->head, (char*) "id")));
		GVertexNet* tail = find_vertex(string(agget(((Agedge_t*) cur_edge->edge->graphviz_e)->tail,(char*) "id")));
#endif
		

		cur_edge->edge->vertex_head = head;
		cur_edge->edge->vertex_tail = tail;

		head->edges_in.push_back(cur_edge->edge);
		tail->edges_out.push_back(cur_edge->edge);

		cur_edge = cur_edge->suiv;
	}

	if ((dpi_x > -1.01f && dpi_x < -0.99f) || (dpi_y > -1.01f && dpi_y < -0.99f)) {
		dpi_x = 72.0f;
		dpi_y = 72.0f;
	}
	convert_inche2pix(dpi_x, dpi_y);

	delete[] pts;
	
}

/**
 * Return the type of the element matching the num
 */
int GraphNet::find_shape(int num) {
	GVertexNet* v = find_vertex_by_num(num);
	if (v == NULL)
		return -1;
	else
		return v->shape;
}

/**
 * Return the host of the element matching the num
 */
int GraphNet::find_host(int num) {
	GVertexNet* v = find_vertex_by_num(num);
	if (v == NULL)
		return -1;
	else
		return v->host_id;
}

/**
 * Return the vertex matching the given num
 */
GVertexNet* GraphNet::find_vertex_by_num(int num) {
	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {
		if (cur_vertex->vertex->get_num() == num)
			return cur_vertex->vertex;
		cur_vertex = cur_vertex->suiv;
	}
	return NULL;
}

/**
 * Return the edge matching the given num
 */
GEdgeNet* GraphNet::find_edge_by_num(int num) {
	liste_edges* cur_edge = this->get_edges();
	while (cur_edge != NULL) {
		if (cur_edge->edge->get_num() == num)
			return cur_edge->edge;
		cur_edge = cur_edge->suiv;
	}
	return NULL;
}

/**
 * Return the vertex or edge matching the given num
 */
VertexOrEdge GraphNet::find_by_num(int num) {
	// search il vertices list
	GVertexNet* vertex = find_vertex_by_num(num);
	if (vertex != NULL) {
		VertexOrEdge voe = { (void*) vertex, TYPE_VERTEX };
		return voe;
	}

	// search in edges list
	GEdgeNet* edge = find_edge_by_num(num);
	if (edge != NULL) {
		VertexOrEdge voe = { (void*) edge, TYPE_EDGE };
		return voe;
	}

	// not found
	VertexOrEdge voe = { NULL, -1 };
	return voe;
}

/**
 * Change the color of the vertex matching the num
 */
void GraphNet::change_vertex_color(int num, float r, float g, float b) {
	liste_vertices* cur_vertex = this->get_vertices();

	while (cur_vertex != NULL) {
		if (cur_vertex->vertex->get_num() == num) {
			cur_vertex->vertex->color_net[0] = r;
			cur_vertex->vertex->color_net[1] = g;
			cur_vertex->vertex->color_net[2] = b;
			return;
		}
		cur_vertex = cur_vertex->suiv;
	}
}

/**
 * Compute the depth of the given vertex functions of its neighbours
 */
int compute_depth(GVertexNet* vertex, int mode_exploration) {
	int min_depth = vertex->depth;

	list<void*>::iterator itee;
	if (mode_exploration == /*!!!!!!!!!=>*/MODE_EXPLO_IN
			/*<=!!!!!!!!!*/|| mode_exploration == MODE_EXPLO_IN_OUT) {
		for (itee = vertex->edges_out.begin(); itee != vertex->edges_out.end(); ++itee) {
			if (((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_head)->fmm_accepted) {
				if (((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_head)->depth
						< min_depth)
					min_depth
							= ((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_head)->depth;
			}
		}
	}

	if (mode_exploration == /*!!!!!!!!!=>*/MODE_EXPLO_OUT
			/*<=!!!!!!!!!*/|| mode_exploration == MODE_EXPLO_IN_OUT) {
		for (itee = vertex->edges_in.begin(); itee != vertex->edges_in.end(); ++itee) {
			if (((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_tail)->fmm_accepted) {
				if (((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_tail)->depth
						< min_depth)
					min_depth
							= ((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_tail)->depth;
			}
		}
	}

	if (vertex->is_small)
		vertex->depth = min(vertex->depth, min_depth);
	else
		vertex->depth = min(vertex->depth, min_depth + 1);

	return vertex->depth;
}

/**
 * FMM step: add neighbours
 */
bool add_neighbours(GVertexNet* vertex, list<GVertexNet*> *narrow_band,
		int mode_exploration, int depth) {
	bool added = false;

	list<void*>::iterator ite;
	if (mode_exploration == MODE_EXPLO_OUT || mode_exploration
			== MODE_EXPLO_IN_OUT) {
		for (ite = vertex->edges_out.begin(); ite != vertex->edges_out.end(); ++ite) {
			GVertexNet* neighbour = (GVertexNet*) ((GEdgeNet*) (*ite))->vertex_head;
			if (neighbour != vertex && !neighbour->fmm_accepted) {
				if (depth == 1) {
					if (neighbour->is_small && neighbour->depth > 0)
						neighbour->depth = 0;
					else {
						neighbour->depth = 1;
						added = true;
					}
				} else {
					neighbour->depth = depth;
				}
				narrow_band->push_back(neighbour);
			}
		}
	}

	if (mode_exploration == MODE_EXPLO_IN || mode_exploration
			== MODE_EXPLO_IN_OUT) {
		for (ite = vertex->edges_in.begin(); ite != vertex->edges_in.end(); ++ite) {
			GVertexNet* neighbour = (GVertexNet*) ((GEdgeNet*) (*ite))->vertex_tail;
			if (neighbour != vertex && !neighbour->fmm_accepted) {
				if (depth == 1) {
					if (neighbour->is_small && neighbour->depth > 0)
						neighbour->depth = 0;
					else {
						added = true;
						neighbour->depth = 1;
					}
				} else {
					neighbour->depth = depth;
				}
				narrow_band->push_back(neighbour);
			}
		}
	}

	narrow_band->sort();
	narrow_band->unique();

	return added;
}

/**
 * Fast Marching Method algorithm
 */
int fmm(list<GVertexNet*> *narrow_band, int mode_exploration) {
	int max_depth = 0;

	while (narrow_band->size() > 0) {
		// 1) find the smallest depth of the narrow band

		GVertexNet* trial_vertex = NULL;

		list<GVertexNet*>::iterator ite;
		list<GVertexNet*>::iterator trial_ite;

		for (ite = narrow_band->begin(); ite != narrow_band->end(); ++ite) {
			if (trial_vertex == NULL) {
				trial_vertex = *ite;
				trial_ite = ite;
			}

			if ((*ite)->depth < trial_vertex->depth) {
				trial_vertex = *ite;
				trial_ite = ite;
			}
		}

		// 2) erase the trial form the narrow_band list

		narrow_band->erase(trial_ite);
		trial_vertex->fmm_accepted = true;

		// 3)

		add_neighbours(trial_vertex, narrow_band, mode_exploration, INFINITE);

		// 4) for each neightbour of trial:
		//       compute depth using accepted vertices

		list<void*>::iterator itee;
		if (mode_exploration == MODE_EXPLO_OUT || mode_exploration
				== MODE_EXPLO_IN_OUT) {
			for (itee = trial_vertex->edges_out.begin(); itee
					!= trial_vertex->edges_out.end(); ++itee) {
				if (!((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_head)->fmm_accepted) {
					max_depth = max(
							max_depth,
							compute_depth(
									(GVertexNet*) ((GEdgeNet*) (*itee))->vertex_head,
									mode_exploration));
				}
			}
		}

		if (mode_exploration == MODE_EXPLO_IN || mode_exploration
				== MODE_EXPLO_IN_OUT) {
			for (itee = trial_vertex->edges_in.begin(); itee
					!= trial_vertex->edges_in.end(); ++itee) {
				if (!((GVertexNet*) ((GEdgeNet*) (*itee))->vertex_tail)->fmm_accepted) {
					max_depth = max(
							max_depth,
							compute_depth(
									(GVertexNet*) ((GEdgeNet*) (*itee))->vertex_tail,
									mode_exploration));
				}
			}
		}
	}

	return max_depth;
}

/**
 * Actually set visibility of vertices and connections
 * after depth computation.
 */
void GraphNet::apply_visibility_depth(int depth) {

	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {
		cur_vertex->vertex->set_visible(
				cur_vertex->vertex->selected || (!cur_vertex->vertex->is_small
						&& cur_vertex->vertex->depth <= depth)
						|| (cur_vertex->vertex->is_small
								&& cur_vertex->vertex->depth < depth));
		cur_vertex = cur_vertex->suiv;
	}

	liste_edges* cur_edge = this->get_edges();
	GVertexNet *tail, *head;
	while (cur_edge != NULL) {
		tail = (GVertexNet*) cur_edge->edge->vertex_tail;
		head = (GVertexNet*) cur_edge->edge->vertex_head;
		cur_edge->edge->set_visible(
				(cur_edge->edge)->super_visible || (tail->get_visible()
						&& head->get_visible()));

		cur_edge = cur_edge->suiv;
	}
}

/**
 * Select the given list of vertices
 */
void GraphNet::select(list<GVertexNet*> *selected_vertices, int &niveau_exploration,
		int mode_exploration, bool maxdepth) {
	// Initialization of the depth computation
	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {
		cur_vertex->vertex->set_selected(false);
		cur_vertex->vertex->fmm_accepted = false;
		cur_vertex->vertex->depth = INFINITE;
		cur_vertex = cur_vertex->suiv;
	}

	if (selected_vertices->size() > 0) {
		// Initial condition of the FMM
		list<GVertexNet*>::const_iterator iter;
		bool added = false;
		list<GVertexNet*> narrow_band;
		for (iter = selected_vertices->begin(); iter
				!= selected_vertices->end(); iter++) {
			(*iter)->depth = 0;
			(*iter)->fmm_accepted = true;
			bool added2 = add_neighbours((*iter), &narrow_band,
					mode_exploration, 1);
			added = added || added2;

			(*iter)->set_selected(true);
		}
		// FMM iterations
		int m_depth = fmm(&narrow_band, mode_exploration);
		if (m_depth == 0 && added == true)
			m_depth = 1;
		if (maxdepth) {
			niveau_exploration = m_depth;
			apply_visibility_depth(m_depth);
		} else {
			if (niveau_exploration > m_depth)
				niveau_exploration = m_depth;
			apply_visibility_depth(niveau_exploration);
		}
	}

}
/**
 * Transform the list of VertexOrEdge into a list of Vertices.
 * A selected edge is replaced by its head and tail.
 * Then call select with the Transformed list.
 */
void GraphNet::select(list<VertexOrEdge> *selection, int &niveau_exploration,
		int mode_exploration, bool maxdepth) {

	// Unselect edges

	liste_edges* cur_edge = get_edges();
	while (cur_edge != NULL) {
		cur_edge->edge->super_visible = false;
		cur_edge = cur_edge->suiv;
	}

	// The edges of the selection are replaced by their head and tail

	list<GVertexNet*> selected_vertices;

	list<VertexOrEdge>::const_iterator iter_sel;
	for (iter_sel = selection->begin(); iter_sel != selection->end(); iter_sel++) {
		if ((*iter_sel).type == TYPE_EDGE) {
			// add its head and tail
			selected_vertices.push_back(
					(GVertexNet*) ((GEdgeNet*) ((*iter_sel).voe))->vertex_head);
			selected_vertices.push_back(
					(GVertexNet*) ((GEdgeNet*) ((*iter_sel).voe))->vertex_tail);

			((GEdgeNet*) ((*iter_sel).voe))->super_visible = true;
		} else {
			// add the vertex
			selected_vertices.push_back((GVertexNet*) (*iter_sel).voe);
		}
	}

	// here => selected_vertices is filled

	select(&selected_vertices, niveau_exploration, mode_exploration, maxdepth);
}

/**
 * Draw a magnified vertex
 */
void GraphNet::draw_zoomed_vertex(int color_mode, float* shape_color,
		float* host_color, GVertexNet* zoomed_vertex) {
	if (zoomed_vertex != NULL) {
		glScalef(ZOOM_FACTOR, ZOOM_FACTOR, 1.0f);
		glTranslatef(-(1.0f - 1.0f / ZOOM_FACTOR) * zoomed_vertex->get_pos_x(),
				-(1.0f - 1.0f / ZOOM_FACTOR) * zoomed_vertex->get_pos_y(), 0.0f);
		zoomed_vertex->draw(false, color_mode, shape_color, host_color);
		glTranslatef(+(1.0f - 1.0f / ZOOM_FACTOR) * zoomed_vertex->get_pos_x(),
				+(1.0f - 1.0f / ZOOM_FACTOR) * zoomed_vertex->get_pos_y(), 0.0f);
		glScalef(1.0f / ZOOM_FACTOR, 1.0f / ZOOM_FACTOR, 1.0f);
	}
}

/**
 * Graph drawing
 */
bool GraphNet::draw(bool fast, int color_mode, bool only_distant_conn,
                    float* shape_color, float* host_color, GVertexNet* zoomed_vertex,
                    GEdgeNet* zoomed_edge, 
                    Traces * traces) {
	// Drawing the vertices

	liste_vertices* cur_vertex = this->get_vertices();
	while (cur_vertex != NULL) {

		glPushName(cur_vertex->vertex->get_num());

                if(traces) {             
                  int state = traces->getModuleState(*cur_vertex->vertex->get_id());
                  // select iff running
                  cur_vertex->vertex->set_selected(state == 1); 
                }

		if (zoomed_edge != NULL) {
			if ((GVertexNet*) zoomed_edge->vertex_tail == cur_vertex->vertex
					|| (GVertexNet*) zoomed_edge->vertex_head
							== cur_vertex->vertex)
				cur_vertex->vertex->draw(fast, color_mode, shape_color,
						host_color);
			else
				cur_vertex->vertex->draw(fast, color_mode, shape_color,
						host_color, false);
		} else {
			cur_vertex->vertex->draw(fast, color_mode, shape_color, host_color);
		}
		glPopName();

		cur_vertex = cur_vertex->suiv;
	}

	// Drawing the edges


	liste_edges* cur_edge = this->get_edges();
	while (cur_edge != NULL) {

                if(traces) {             
                  // the edges do not know what ports there are linking, so let's just pass "" for the time being
                  double t = traces->getLinkState(*cur_edge->edge->vertex_tail->get_id(), "",
                                                  *cur_edge->edge->vertex_head->get_id(), "");
                  // select iff running
                  cur_edge->edge->set_message_t(t); 
                }

		glPushName(cur_edge->edge->get_num());
		if ((!only_distant_conn && (zoomed_edge == NULL || zoomed_edge
				== cur_edge->edge))
				|| (only_distant_conn
						&& ((*(((GVertexNet*) cur_edge->edge->vertex_head)->get_host())
								!= *(((GVertexNet*) cur_edge->edge->vertex_tail)->get_host())
								&& zoomed_edge == NULL) || zoomed_edge
								== cur_edge->edge)))

		{
			cur_edge->edge->draw(fast, color_mode);
		} else {
			cur_edge->edge->draw(fast, color_mode, false);
		}
		glPopName();
		cur_edge = cur_edge->suiv;
	}
	// Drawing the zoomed vertex
	if (zoomed_vertex != NULL)
		draw_zoomed_vertex(color_mode, shape_color, host_color, zoomed_vertex);

	return true;
}
