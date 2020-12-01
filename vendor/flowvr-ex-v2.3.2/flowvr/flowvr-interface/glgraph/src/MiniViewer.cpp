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
 * File: ./src/MiniViewer.cpp                                      *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include "MiniViewer.h"
#include "interface.h"
#include "ui_myInterface.h"
//Added by qt3to4:
#include <QWheelEvent>
#include <QMouseEvent>

using namespace std;

/*
 * Convert a pixel pos into a 3D point with z=0
 */
qglviewer::Vec MiniViewer::convertPoint(QPoint pos) {
	qglviewer::Vec orig;
	qglviewer::Vec dir;
	camera()->convertClickToLine(pos, orig, dir);
	float c = -orig.z / dir.z;

	return qglviewer::Vec(orig.x + dir.x * c, orig.y + dir.y * c, 0.0f);
}

/*
 * Event: wheel
 * We transmit to the main viewer
 */
void MiniViewer::wheelEvent(QWheelEvent * const event) {
	parent->Viewer1->wheelEvent(event);
	return;
}

/*
 * Event: mouse move
 * We change the scene center in the main viewer
 */
void MiniViewer::mouseMoveEvent(QMouseEvent* const e) {
	qglviewer::Vec point;
	Viewer* mainViewer = parent->Viewer1;

	mainViewer->setSceneCenter(convertPoint(e->pos()));
	mainViewer->camera()->centerScene();
	mainViewer->updateGL();
	//mainViewer->draw_cross();
	refresh();
}

/*
 * Event: mouse pressed
 * We change the scene center in the main viewer
 */
void MiniViewer::mousePressEvent(QMouseEvent* e) {
	qglviewer::Vec point;
	parent->Viewer1->setSceneCenter(convertPoint(e->pos()));
	parent->Viewer1->camera()->centerScene();
	parent->Viewer1->updateGL();
}

/*
 * Constructor
 */
MiniViewer::MiniViewer(QWidget *parent, const char *name) :
	QGLViewer(parent, name) {
	this->parent = (Ui_myInterface*) parent;
	this->setFocusPolicy(Qt::NoFocus);
}

MiniViewer::MiniViewer(QSplitter *parent, const char* name) :
	QGLViewer(parent, name) {
	this->parent = (Ui_myInterface*) parent;
	this->setFocusPolicy(Qt::NoFocus);
}

MiniViewer::MiniViewer(Ui_myInterface *parent, const char* name) :
	QGLViewer((QWidget*) parent, name) {
	this->parent = parent;
	this->setFocusPolicy(Qt::NoFocus);
}

/*
 * Initialization
 */
void MiniViewer::init() {
	glClearColor(1.0f, 1.0f, 1.0f, 0.0);
	glDisable( GL_DEPTH_TEST);
}

/*
 * Refresh
 * Public function called by the main viewer.
 */
void MiniViewer::refresh() {
	draw();
}

/*
 * Drawing
 */
void MiniViewer::draw() {
	if (this->parent->Viewer1->graph != NULL) {
		float radius;
		/*
		this->parent->Viewer1->graph->getBoundingBox(
				&(this->parent->Viewer1->graph->leftBound),
				&(this->parent->Viewer1->graph->rightBound),
				&(this->parent->Viewer1->graph->topBound),
				&(this->parent->Viewer1->graph->bottomBound));
				*/
		radius = 0.5f * max(
				this->parent->Viewer1->graph->bottomBound
						- this->parent->Viewer1->graph->topBound,
				this->parent->Viewer1->graph->rightBound
						- this->parent->Viewer1->graph->leftBound);
		setSceneRadius(radius + 10.0f);
		setSceneCenter(
				qglviewer::Vec(
						0.5f * (this->parent->Viewer1->graph->rightBound
								+ this->parent->Viewer1->graph->leftBound),
						0.5f
								* (this->parent->Viewer1->graph->topBound
										+ this->parent->Viewer1->graph->bottomBound),
						0));
		showEntireScene();

		// Quick and dirty fix to keep the red box above

			this->parent->Viewer1->graph->drawForMiniViewer(parent->Viewer1);
			//this->parent->Viewer1->graph->draw(parent->Viewer1, true, false, false, false);


		glPushMatrix();
		glTranslatef(0.f, 0.f, 50.f);
		glColor3f(1.0f, 0.0f, 0.0f);
		glLineWidth(3.0f);
		glBegin( GL_LINE_LOOP);
		glVertex3f(this->parent->Viewer1->view_boundingbox_tl.x,
				this->parent->Viewer1->view_boundingbox_tl.y,
				this->parent->Viewer1->view_boundingbox_tl.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_br.x,
				this->parent->Viewer1->view_boundingbox_tl.y,
				this->parent->Viewer1->view_boundingbox_br.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_br.x,
				this->parent->Viewer1->view_boundingbox_br.y,
				this->parent->Viewer1->view_boundingbox_tl.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_tl.x,
				this->parent->Viewer1->view_boundingbox_br.y,
				this->parent->Viewer1->view_boundingbox_br.z);
		glEnd();
		glPopMatrix();

	} else if (this->parent->Viewer1->graphNet != NULL) {
		float radius;
		this->parent->Viewer1->graphNet->get_bounding_box(
				&(this->parent->Viewer1->graphNet->bounding_left),
				&(this->parent->Viewer1->graphNet->bounding_right),
				&(this->parent->Viewer1->graphNet->bounding_top),
				&(this->parent->Viewer1->graphNet->bounding_bottom));
		radius = 0.5f * max(
				this->parent->Viewer1->graphNet->bounding_bottom
						- this->parent->Viewer1->graphNet->bounding_top,
				this->parent->Viewer1->graphNet->bounding_right
						- this->parent->Viewer1->graphNet->bounding_left);
		setSceneRadius(radius + 10.0f);
		setSceneCenter(
				qglviewer::Vec(
						0.5f * (this->parent->Viewer1->graphNet->bounding_right
								+ this->parent->Viewer1->graphNet->bounding_left),
						0.5f
								* (this->parent->Viewer1->graphNet->bounding_top
										+ this->parent->Viewer1->graphNet->bounding_bottom),
						0));
		showEntireScene();
		this->parent->Viewer1->graphNet->draw(true,
				this->parent->Viewer1->color_mode, false,
				this->parent->Viewer1->shape_color,
				this->parent->Viewer1->host_color);

		glColor3f(1.0f, 0.0f, 0.0f);
		glLineWidth(3.0f);
		glBegin( GL_LINE_LOOP);
		glVertex3f(this->parent->Viewer1->view_boundingbox_tl.x,
				this->parent->Viewer1->view_boundingbox_tl.y,
				this->parent->Viewer1->view_boundingbox_tl.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_br.x,
				this->parent->Viewer1->view_boundingbox_tl.y,
				this->parent->Viewer1->view_boundingbox_br.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_br.x,
				this->parent->Viewer1->view_boundingbox_br.y,
				this->parent->Viewer1->view_boundingbox_tl.z);

		glVertex3f(this->parent->Viewer1->view_boundingbox_tl.x,
				this->parent->Viewer1->view_boundingbox_br.y,
				this->parent->Viewer1->view_boundingbox_br.z);
		glEnd();
	}
}

