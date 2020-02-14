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
 * File: ./src/interface.h                                         *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 * Hierarchical part: Xavier Martin <xavier.martin@imag.fr>        *
 *                                                                 *
 ******************************************************************/
#ifndef INTERFACE_H
#define INTERFACE_H
// GraphViz

#include "../../QGLViewer/qglviewer.h"
#include <qvariant.h>
#include <qpixmap.h>
#include <qwidget.h>
#include <q3listview.h>
#include <qtoolbutton.h>
#include <q3mainwindow.h>
//Added by qt3to4:
#include <QWheelEvent>
#include <QMouseEvent>
#include <QKeyEvent>
#include <QSplitter>

#include "Graph.h"
#include "GraphNet.h"

#include <unistd.h>

#include <gvc.h>

#define QT3_SUPPORT
#define MODE_EXPLO_IN     1
#define MODE_EXPLO_OUT    2
#define MODE_EXPLO_IN_OUT 3

class Ui_myInterface;

class Traces;
class QTimer; 

class Viewer: public QGLViewer {
	Q_OBJECT

public:

	// vars
	GLuint logo_texture;

	Graph* graph;
	GraphNet* graphNet;
	
	Ui_myInterface* parent;
	qglviewer::Vec view_boundingbox_br, view_boundingbox_tl;
	float* host_color;
	GLuint texture;
	GLuint framebuffer;
	Q3MainWindow* main_window;
	QToolButton* tb_out;
	QToolButton* tb_in;
	QToolButton* tb_in_out;
	QToolButton* tb_color1;
	QToolButton* tb_color2;
	QToolButton* tb_color3;
	QToolButton* tb_select;
	QToolButton* tb_color;
	QToolButton* tb_colorchg;
	QToolButton* tb_maxdepth;
	QToolButton* cluster_view;
	float shape_color[NB_SHAPES * 3];
	bool show_only_distant_con;
	void load_file_2(QString fileName);
	std::string datadir;
	int color_mode;

	// functions
	Viewer(QWidget *parent, const char *name);
	Viewer(QSplitter *parent, const char* name = "");
	//   Viewer(Ui_myInterface *parent, const char* name="");
	void draw2(bool fast);
	virtual void wheelEvent(QWheelEvent* e);
	void displayInfo(const void* info);
	void displayInfo(std::string info);

	void draw_cross();

	signals:

public slots:

	void switch_modes() {
		printf("Switch modes");
	}

	void load_file();
        void load_traces(const char *filename);
	void export_view();
	void toggle_in(bool b);
	void toggle_out(bool b);
	void toggle_in_out(bool b);
	void toggle_color1(bool b);
	void toggle_color2(bool b);
	void toggle_color3(bool b);
	void toggle_select(bool b);
	void reload();
	void toggle_maxdepth(bool b);
	void tb_plus_released();
	void tb_minus_released();
	void tb_help_released();
	void listView1_selectionChanged(Q3ListViewItem* item);
	void listView2_selectionChanged(Q3ListViewItem* item);
	void listViewLinks_selectionChanged(Q3ListViewItem* item);
	void listViewXml_selectionChanged(Q3ListViewItem* item);
	void checkBox1_toggled(bool b);


	void regExpButton_released();
	bool regExpAdlBrowse(Q3ListViewItem* elem, QRegExp exp);

	void toggle_colorchg(bool b);
	void colorButton_released();
	void tab_changed(QWidget* new_tab);
	void regExpReturn();
	void change_to_cluster_view();

	virtual void mouseMoveEvent(QMouseEvent* e);


        // trace visu related slots
        void setTraceTime(int);
        void setTraceTimeFromText(); 
        void playTrace();
        void playTraceEvent();
        void timerStep(); 
        void stopTimer();

protected:
	virtual void init();
	virtual void fastDraw(); // Called while camera is moving
	virtual void draw();
	virtual void postDraw();
	virtual void drawWithNames();
	virtual void postSelection(const QPoint&);
	virtual void keyPressEvent(QKeyEvent* e);
	virtual QString helpString() const;
	virtual QString keyboardString() const;
	virtual QString mouseString() const;
	virtual void mousePressEvent(QMouseEvent* e);
	virtual void aboutQGLViewer();
	virtual void mouseReleaseEvent(QMouseEvent* e);
        void gotoTime(double);
        
private:

	bool graphIsAdl;
	bool netHierarchicalView;

	Component* adlTree;

	// state vars
	std::list<VertexOrEdge> cur_selection;
	GVertexNet* zoomed_vertex;
	GEdgeNet* zoomed_edge;
	QString current_fileName;
	int max_depth;
	Agraph_t* gv_graph;
	QColor cur_color;
	qglviewer::Vec selectedPoint;
	int niveau_exploration;
	int mode_exploration;
	bool selection_mode;
	bool color_changing_mode;
	std::list<std::string> hosts;
	bool shader_enabled;
	bool shader_supported;

        // traces 
        Traces * traces;
        QTimer *traceTimer; 
        double eventT; 

	// parameters
	std::string xsl_file;



	// functions
	void update_selection();
	void fill_tree();
	void fill_tree_links();
	void fill_tree_ids();
	void fill_tree_ids_adl(Q3ListViewItem* parent, GCluster* cl);
	void fill_tree_hosts();
	void fill_tree_xml_adl(Q3ListViewItem* parent, Component* comp);

	void init_camera();
	int init_shader();
	int init_fbo();
	void connections_visible();
	void everything_visible(bool visible);
	qglviewer::Vec convertPoint(QPoint point);
	void selectItem(Q3ListViewItem* item, bool firstOnly, int column, bool extractId=false);
	std::list<VertexOrEdge> get_vertices_or_edges_list(std::list<QString> *strl);
	void zoom_under_cursor(QMouseEvent* e);

	void select_component_adl(qglviewer::Vec p);

	void draw_logo();
	void assign_color_to_hosts();

	bool adl2dot(const char* filename_adl, const char* filename_dot);
};
#endif
