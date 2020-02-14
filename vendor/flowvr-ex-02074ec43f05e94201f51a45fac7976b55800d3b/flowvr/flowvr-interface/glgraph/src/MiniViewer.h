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
 * File: ./src/MiniViewer.h                                        *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#ifndef MINIVIEWER_H
#define MINIVIEWER_H

#include "../../QGLViewer/qglviewer.h"

#include "interface.h"
//Added by qt3to4:
#include <QWheelEvent>
#include <QMouseEvent>
#include <QSplitter>

class Ui_myInterface;

class MiniViewer: public QGLViewer {
	Q_OBJECT

private:

	qglviewer::Vec convertPoint(QPoint pos);

public:
	Ui_myInterface* parent;
	Graph *graph;
	MiniViewer(QWidget *parent, const char *name);
	MiniViewer(QSplitter *parent, const char* name = "");
	MiniViewer(Ui_myInterface *parent, const char* name = "");
	void refresh();

protected:
	virtual void init();
	virtual void draw();
	virtual void mousePressEvent(QMouseEvent* e);
	virtual void mouseMoveEvent(QMouseEvent* e);
	virtual void wheelEvent(QWheelEvent* e);

};

#endif
