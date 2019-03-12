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
 * File: ./src/GVertexNet.cpp                                         *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include "../../QGLViewer/qglviewer.h"
#include "GVertexNet.h"
#include "utils.h"

using namespace std;

#define ETAT_0 0
#define ETAT_RECORD 1
#define ETAT_LIGNE_TEXTE 2
#define ETAT_LIGNE_IDENTIFIANT 3

float GVertexNet::get_pos_x() {
	return this->pos_x;
}
float GVertexNet::get_pos_y() {
	return this->pos_y;
}
string* GVertexNet::get_id() {
	return &(this->id);
}
int GVertexNet::get_num() {
	return this->num;
}

void GVertexNet::set_selected(bool sel) {
	this->selected = sel;
}
void GVertexNet::set_visible(bool visible) {
	this->visible = visible;
}
bool GVertexNet::get_visible() {
	return this->visible;
}
string* GVertexNet::get_host() {
	return &(this->host);
}

/**
 * Vertex constructor
 */
GVertexNet::GVertexNet(float posx, float posy, float width, float height,
		string label, string host, int shape, list<s_record>*record_info,
		unsigned char r, unsigned char g, unsigned char b, unsigned char a,
		string id, int num) {
	this->record_info = record_info;
	this->pos_x = posx;
	this->pos_y = posy;
	this->width = width;
	this->height = height;
	this->label = label;
	this->host = host;
	this->shape = shape;
	this->id = id;
	this->num = num;
	this->selected = false;
	this->visible = true;
	this->color_net[0] = (float) r / 255.0f;
	this->color_net[1] = (float) g / 255.0f;
	this->color_net[2] = (float) b / 255.0f;
	this->color_net[3] = (float) a / 255.0f;
	this->host_id = 0;

	if (shape == SHAPE_CIRCLE) {
		this->is_small = true;
	} else {
		this->is_small = false;
	}
}

/**
 * Desctructor
 */
GVertexNet::~GVertexNet() {

}

/**
 * Drawing
 * Call the drawing function corresponding to the vertex type.
 */
void GVertexNet::draw(bool fast, int color_mode, float* shape_color,
		float* host_color, bool and_visible) {
	float alpha;
	if (this->visible && and_visible)
		alpha = 1.0f;
	else
		alpha = ALPHA_INVISIBLE;

	switch (this->shape) {
	case SHAPE_BOX:
		this->draw_box(fast, color_mode, alpha, shape_color + SHAPE_BOX * 3,
				host_color + this->host_id * 3);
		break;
	case SHAPE_DIAMOND:
		this->draw_diamond(fast, color_mode, alpha,
				shape_color + SHAPE_DIAMOND * 3, host_color + this->host_id * 3);
		break;
	case SHAPE_CIRCLE:
		this->draw_circle(fast, color_mode, alpha,
				shape_color + SHAPE_CIRCLE * 3, host_color + this->host_id * 3);
		break;
	case SHAPE_MRECORD:
		this->draw_mrecord(fast, color_mode, alpha,
				shape_color + SHAPE_MRECORD * 3, host_color + this->host_id * 3);
		break;
	}
}

/**
 * Draw box
 */
void GVertexNet::draw_box(bool fast, int color_mode, float alpha, float* color,
		float* host_color, bool and_visible) {

	if (selected) {
		glColor4f(0.0f, 0.0f, 0.0f, alpha);
		glLineWidth(SELECTED_WIDTH);
		glBegin( GL_LINE_LOOP);
		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y - this->height / 2.0f);
		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y + this->height / 2.0f);
		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y + this->height / 2.0f);
		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y - this->height / 2.0f);
		glEnd();
	}

	if (color_mode == 0)
		glColor4f(color[0], color[1], color[2], alpha);
	else if (color_mode == 1)
		glColor4f(host_color[0], host_color[1], host_color[2], alpha);
	else if (color_mode == 2)
		glColor4f(color_net[0], color_net[1], color_net[2], alpha);

	glBegin( GL_QUADS);
	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y - this->height / 2.0f);
	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y + this->height / 2.0f);
	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y + this->height / 2.0f);
	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y - this->height / 2.0f);
	glEnd();

	if (fast || !(this->visible && and_visible))
		return; // Complete drawing =========================>

	glColor4f(0.0f, 0.0f, 0.0f, alpha);

	glLineWidth(1.0f);
	glBegin( GL_LINE_LOOP);
	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y - this->height / 2.0f);
	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y + this->height / 2.0f);
	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y + this->height / 2.0f);
	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y - this->height / 2.0f);
	glEnd();

	glLineWidth(1.0);
	if (this->label != "")
		renderStrokeString(this->pos_x, this->pos_y, 0.0f, this->label,
				FONT_SIZE);
}

/**
 * Draw diamond
 */
void GVertexNet::draw_diamond(bool fast, int color_mode, float alpha,
		float* color, float* host_color, bool and_visible) {

	if (selected) {
		glColor4f(0.0f, 0.0f, 0.0f, alpha);
		glLineWidth(SELECTED_WIDTH);
		glBegin( GL_LINE_LOOP);
		glVertex2f(this->pos_x, this->pos_y + this->height / 2.0f);

		glVertex2f(this->pos_x + this->width / 2.0f, this->pos_y);

		glVertex2f(this->pos_x, this->pos_y - this->height / 2.0f);

		glVertex2f(this->pos_x - this->width / 2.0f, this->pos_y);
		glEnd();
	}

	if (color_mode == 0)
		glColor4f(color[0], color[1], color[2], alpha);
	else if (color_mode == 1)
		glColor4f(host_color[0], host_color[1], host_color[2], alpha);
	else if (color_mode == 2)
		glColor4f(color_net[0], color_net[1], color_net[2], alpha);

	glBegin( GL_QUADS);
	glVertex2f(this->pos_x, this->pos_y + this->height / 2.0f);

	glVertex2f(this->pos_x + this->width / 2.0f, this->pos_y);

	glVertex2f(this->pos_x, this->pos_y - this->height / 2.0f);

	glVertex2f(this->pos_x - this->width / 2.0f, this->pos_y);
	glEnd();

	if (fast || !(this->visible && and_visible))
		return; // Complete drawing =========================>

	glColor4f(0.0f, 0.0f, 0.0f, alpha);

	glLineWidth(1.0f);
	glBegin( GL_LINE_LOOP);
	glVertex2f(this->pos_x, this->pos_y + this->height / 2.0f);

	glVertex2f(this->pos_x + this->width / 2.0f, this->pos_y);

	glVertex2f(this->pos_x, this->pos_y - this->height / 2.0f);

	glVertex2f(this->pos_x - this->width / 2.0f, this->pos_y);
	glEnd();

	glLineWidth(1.0);
	if (this->label != "")
		renderStrokeString(this->pos_x, this->pos_y, 0.0f, this->label,
				FONT_SIZE);
}

/**
 * Draw circle
 */
void GVertexNet::draw_circle(bool fast, int color_mode, float alpha, float* color,
		float* host_color, bool and_visible) {
	float nb_points;
	float pi = 3.141593f;

	if (fast)
		nb_points = 6.0f;
	else
		nb_points = 12.0f;

	if (selected) {
		glColor4f(0.0f, 0.0f, 0.0f, alpha);
		glLineWidth(SELECTED_WIDTH);
		glBegin( GL_LINE_LOOP);
		for (int i = 0; i < nb_points; i++) {
			glVertex2f(
					this->pos_x + cos(2 * pi / nb_points * i) * this->width
							* 0.5f,
					this->pos_y + sin(2 * pi / nb_points * i) * this->height
							* 0.5f);
		}
		glEnd();
	} else {
		glLineWidth(1.0f);
	}

	if (color_mode == 0)
		glColor4f(color[0], color[1], color[2], alpha);
	else if (color_mode == 1)
		glColor4f(host_color[0], host_color[1], host_color[2], alpha);
	else if (color_mode == 2)
		glColor4f(color_net[0], color_net[1], color_net[2], alpha);

	glBegin( GL_POLYGON);
	for (int i = 0; i < nb_points; i++) {
		glVertex2f(
				this->pos_x + cos(2 * pi / nb_points * i) * this->width * 0.5f,
				this->pos_y + sin(2 * pi / nb_points * i) * this->height * 0.5f);
	}
	glEnd();

	if (fast || !(this->visible && and_visible))
		return; // Complete drawing =========================>

	glColor4f(0.0f, 0.0f, 0.0f, alpha);

	glLineWidth(1.0f);

	if (this->label != "")
		renderStrokeString(this->pos_x, this->pos_y, 0.0f, this->label,
				FONT_SIZE);
}

/**
 * Draw mrecord
 */
void GVertexNet::draw_mrecord(bool fast, int color_mode, float alpha,
		float* color, float* host_color, bool and_visible) {

	float d = 0.1 * this->height;

	if (color_mode == 0)
		glColor4f(color[0], color[1], color[2], alpha);
	else if (color_mode == 1)
		glColor4f(host_color[0], host_color[1], host_color[2], alpha);
	else if (color_mode == 2)
		glColor4f(color_net[0], color_net[1], color_net[2], alpha);

	glBegin( GL_POLYGON);
	glVertex2f(this->pos_x - this->width / 2.0f + d,
			this->pos_y - this->height / 2.0f);

	glVertex2f(this->pos_x + this->width / 2.0f - d,
			this->pos_y - this->height / 2.0f);

	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y - this->height / 2.0f + d);

	glVertex2f(this->pos_x + this->width / 2.0f,
			this->pos_y + this->height / 2.0f - d);

	glVertex2f(this->pos_x + this->width / 2.0f - d,
			this->pos_y + this->height / 2.0f);

	glVertex2f(this->pos_x - this->width / 2.0f + d,
			this->pos_y + this->height / 2.0f);

	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y + this->height / 2.0f - d);

	glVertex2f(this->pos_x - this->width / 2.0f,
			this->pos_y - this->height / 2.0f + d);
	glEnd();

	glColor4f(0.0f, 0.0f, 0.0f, alpha);
	if (selected) {
		glLineWidth(SELECTED_WIDTH);
		glBegin( GL_LINE_LOOP);

		glVertex2f(this->pos_x - this->width / 2.0f + d,
				this->pos_y - this->height / 2.0f);

		glVertex2f(this->pos_x + this->width / 2.0f - d,
				this->pos_y - this->height / 2.0f);

		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y - this->height / 2.0f + d);

		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y + this->height / 2.0f - d);

		glVertex2f(this->pos_x + this->width / 2.0f - d,
				this->pos_y + this->height / 2.0f);

		glVertex2f(this->pos_x - this->width / 2.0f + d,
				this->pos_y + this->height / 2.0f);

		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y + this->height / 2.0f - d);

		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y - this->height / 2.0f + d);

		glEnd();
	}

	if (fast || !(this->visible && and_visible))
		return; // Complete drawing =========================>


	glLineWidth(1.0f);
	list<s_record>::const_iterator iter;
	for (iter = this->record_info->begin(); iter != this->record_info->end(); iter++) {

		glColor4f(0.0f, 0.0f, 0.0f, alpha);
		glBegin( GL_LINES);

		if ((*iter).corner == 0) {

		} else if ((*iter).corner == 1) {

			glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).LLy);

			glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).URy);

		} else if ((*iter).corner == 2) {

		} else if ((*iter).corner == 3) {

			glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).LLy);

			glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).URy);
		} else {
			if ((*iter).line != 1) {

				glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).LLy);

				glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).URy);

			} else {
				glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).LLy);

				glVertex2f(this->pos_x + (*iter).URx, this->pos_y + (*iter).LLy);

				glVertex2f(this->pos_x + (*iter).LLx, this->pos_y + (*iter).URy);

				glVertex2f(this->pos_x + (*iter).URx, this->pos_y + (*iter).URy);
			}

		}
		glEnd();

		renderStrokeString(this->pos_x + 0.5 * ((*iter).URx + (*iter).LLx),
				this->pos_y + 0.5 * ((*iter).URy + (*iter).LLy), 0.0f,
				(*iter).text, FONT_SIZE);

	}

	glColor4f(0.0f, 0.0f, 0.0f, alpha);

	if (!selected) {
		glLineWidth(1.0f);

		glBegin( GL_LINE_LOOP);

		glVertex2f(this->pos_x - this->width / 2.0f + d,
				this->pos_y - this->height / 2.0f);

		glVertex2f(this->pos_x + this->width / 2.0f - d,
				this->pos_y - this->height / 2.0f);

		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y - this->height / 2.0f + d);

		glVertex2f(this->pos_x + this->width / 2.0f,
				this->pos_y + this->height / 2.0f - d);

		glVertex2f(this->pos_x + this->width / 2.0f - d,
				this->pos_y + this->height / 2.0f);

		glVertex2f(this->pos_x - this->width / 2.0f + d,
				this->pos_y + this->height / 2.0f);

		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y + this->height / 2.0f - d);

		glVertex2f(this->pos_x - this->width / 2.0f,
				this->pos_y - this->height / 2.0f + d);

		glEnd();
	}

}
