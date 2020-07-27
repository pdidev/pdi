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
 * File: ./src/interface.cpp                                       *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 * Hierarchical part: Xavier Martin <xavier.martin@imag.fr>        *
 *                                                                 *
 ******************************************************************/

// QT
#include <q3accel.h>
#include <q3filedialog.h>
#include <q3listview.h>
#include <qlabel.h>
#include <qtoolbutton.h>
#include <qcursor.h>
#include <qmessagebox.h>
#include <qlineedit.h>
#include <qregexp.h>
#include <qcolordialog.h>
#include <qimage.h>
//Added by qt3to4:
#include <QKeyEvent>
#include <QPixmap>
#include <QMouseEvent>
#include <QWheelEvent>
#include <QTimer>
// OnpenGL
#ifndef NOCG
#include <GL/glew.h>
#endif
#include "interface.h"
// QGLViewer
#include "../../QGLViewer/camera.h"
// Shader
#ifndef NOCG
#include <Cg/cg.h>
#include <Cg/cgGL.h>
#endif
// STL
#include <algorithm>
#include <string>
#include <list>
#include <set>
#include <math.h>
//
#include "utils.h"
#include <ui_myInterface.h>
#include "MiniViewer.h"

// adl tree
#include "adltree.h"

#if USING_CGRAPH
#include <gvc.h>
#else
#endif

#ifdef __APPLE__
# include <GLUT/glut.h>
#else
# include <GL/glut.h>
#endif

//XSLT
#include <libxslt/transform.h>
#include <libxslt/xsltutils.h>
#include "xml_parser.h"
#include "traces.h"


#include <iostream>
#include <fstream>
#include <sstream>

#define QT3_SUPPORT
#define LOGO_FILE "/imgs/flowvr-logo-baseline.png"
#define DEFAULT_XSL_FILE "/xsl/default.xsl"
#define SHADER "/Cg/shader.cg"
#define FBO_W 4096
#define FBO_H 4096
#define ZOOM_LIMIT 100.0f
#define NO_TEXTURE 666
using namespace std;


/* #####################
 * CONSTRUCTOR AND INITS
 */
Viewer::Viewer(QWidget *parent, const char *name)
: QGLViewer(parent, name)
, graph(NULL)
, graphNet(NULL)
, host_color(NULL)
, adlTree(NULL)
, zoomed_vertex(NULL)
, zoomed_edge(NULL)
, gv_graph(NULL)
, show_only_distant_con(false)
, tb_out(NULL)
, tb_in(NULL)
, tb_in_out(NULL)
, tb_color1(NULL)
, tb_color2(NULL)
, tb_color3(NULL)
, tb_select(NULL)
, tb_color(NULL)
, tb_colorchg(NULL)
, tb_maxdepth(NULL)
, cluster_view(NULL)
, selection_mode(false)
, shader_enabled(false)
, shader_supported(false)
, traces(NULL)
, traceTimer(NULL){
	niveau_exploration = 2;
	mode_exploration = MODE_EXPLO_IN_OUT;
	color_mode = 0;

	shape_color[0] = 1.0f;
	shape_color[1] = 0.6f;
	shape_color[2] = 0.6f;

	shape_color[3] = 0.6f;
	shape_color[4] = 0.6f;
	shape_color[5] = 1.0f;

	shape_color[6] = 0.6f;
	shape_color[7] = 0.6f;
	shape_color[8] = 0.6f;

	shape_color[9] = 0.6f;
	shape_color[10] = 1.0f;
	shape_color[11] = 0.6f;

	graphIsAdl = false;
	netHierarchicalView = false;
}

Viewer::Viewer(QSplitter *parent, const char* name)
: QGLViewer(parent, name)
, graph(NULL)
, graphNet(NULL)
, host_color(NULL)
, adlTree(NULL)
, zoomed_vertex(NULL)
, zoomed_edge(NULL)
, gv_graph(NULL)
, show_only_distant_con(false)
, tb_out(NULL)
, tb_in(NULL)
, tb_in_out(NULL)
, tb_color1(NULL)
, tb_color2(NULL)
, tb_color3(NULL)
, tb_select(NULL)
, tb_color(NULL)
, tb_colorchg(NULL)
, tb_maxdepth(NULL)
, cluster_view(NULL)
, selection_mode(false)
, shader_enabled(false)
, shader_supported(false)
, traces(NULL)
, traceTimer(NULL)
{
	niveau_exploration = 2;
	mode_exploration = MODE_EXPLO_IN_OUT;
	color_mode = 0;

	shape_color[0] = 1.0f;
	shape_color[1] = 0.6f;
	shape_color[2] = 0.6f;

	shape_color[3] = 0.6f;
	shape_color[4] = 0.6f;
	shape_color[5] = 1.0f;

	shape_color[6] = 0.6f;
	shape_color[7] = 0.6f;
	shape_color[8] = 0.6f;

	shape_color[9] = 0.6f;
	shape_color[10] = 1.0f;
	shape_color[11] = 0.6f;

	graphIsAdl = false;
	netHierarchicalView = false;
}



/** Needed by: Viewer::init()
 *
 * Create an OpenGL texture from a QImage
 */
GLuint TextureFromQImage(QImage* image) {
	GLuint txtnumber;
	*image = QGLWidget::convertToGLFormat(*image);

	glGenTextures(1, &txtnumber);
	glBindTexture(GL_TEXTURE_2D, txtnumber);
	glTexImage2D(GL_TEXTURE_2D, 0, 3, image->width(), image->height(), 0,
			GL_RGBA, GL_UNSIGNED_BYTE, image->bits());
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

	return txtnumber;
}


/**
 * Initialization of the view
 */
void Viewer::init()
{
	delete graph;
	graph = NULL;
	delete graphNet;
	graphNet = NULL;

	this->zoomed_vertex = NULL;
	this->zoomed_edge = NULL;
	glClearColor(1.0, 1.0, 1.0, 0.0);
	glDisable( GL_DEPTH_TEST);
	camera()->setType(qglviewer::Camera::ORTHOGRAPHIC);
#ifndef NOCG
	shader_supported = (init_shader() != -1);
	init_fbo();
#endif
	this->shader_enabled = false;
	this->show_only_distant_con = false;
	setSelectRegionWidth(10);
	setSelectRegionHeight(10);

	xsl_file = datadir + string("/") + string(DEFAULT_XSL_FILE);

	//setMouseBinding(Qt::LeftButton, TRANSLATE);
	setMouseBinding(Qt::LeftButton, CAMERA, TRANSLATE);
	selection_mode = false;
	color_changing_mode = false;
	max_depth = false;
	cur_color = QColor(255, 0, 0);

	QImage
			logo(
					QString(
							(datadir + std::string("/")
									+ std::string(LOGO_FILE)).c_str()));
	if (!logo.isNull())
		this->logo_texture = TextureFromQImage(&logo);
	else
		this->logo_texture = NO_TEXTURE;
}

/**
 * Camera initialization
 */
void Viewer::init_camera() {
	if (graphIsAdl) {
		setSceneRadius(
				0.6f * max(graph->bottomBound - graph->topBound,
						graph->rightBound - graph->leftBound));
		setSceneCenter(
				qglviewer::Vec(0.5f * (graph->rightBound + graph->leftBound),
						0.5f * (graph->topBound + graph->bottomBound), 0));
		showEntireScene();
	} else {
		setSceneRadius(
				0.6f * max(graphNet->bounding_top - graphNet->bounding_bottom,
						graphNet->bounding_right - graphNet->bounding_left));
		setSceneCenter(
				qglviewer::Vec(
						0.5f * (graphNet->bounding_right + graphNet->bounding_left),
						0.5f * (graphNet->bounding_top + graphNet->bounding_bottom),
						0));
		showEntireScene();
	}
}

/**
 * Create a list of colors and assign one color to each host
 */

void Viewer::assign_color_to_hosts() {
    liste_vertices* cur_vertex = graphNet->get_vertices();
    while (cur_vertex != NULL) {
        hosts.push_back(*(cur_vertex->vertex->get_host()));
        cur_vertex = cur_vertex->suiv;
    }

    hosts.sort();
    hosts.unique();

    int nb_hosts = hosts.size();

    // Generation of the colors

    ColorGenerator colors(nb_hosts);

    int h = 0;
    std::vector<std::string> id_host(nb_hosts);
    host_color = (float*) malloc(sizeof(float) * nb_hosts * 3);
    list<string>::iterator ite;
    for (ite = hosts.begin(); ite != hosts.end(); ++ite) {
        id_host[h] = *ite;
        host_color[h * 3 + 0] = min(colors.get(h, 0) * 0.7f + 0.3f, 1.0f);
        host_color[h * 3 + 1] = min(colors.get(h, 1) * 0.7f + 0.3f, 1.0f);
        host_color[h * 3 + 2] = min(colors.get(h, 2) * 0.7f + 0.3f, 1.0f);
        h++;
    }

    // Assignment of an host id to each vertex

    cur_vertex = graphNet->get_vertices();
    while (cur_vertex != NULL) {
        for (h = 0; h < nb_hosts - 1 && id_host[h]
                != *(cur_vertex->vertex->get_host()); h++)
            ;

        cur_vertex->vertex->host_id = h;
        cur_vertex = cur_vertex->suiv;
    }

}


/* ################
 * MISC: Cg shaders - don't move from here, required by other functions
 */

#ifndef NOCG
/**
 * Shader compilation error
 */
void cgErrorCallback() {
	CGerror err = cgGetError();

	if (err != CG_NO_ERROR) {
		cerr << "cgErrorCallback(): " << cgGetErrorString(err) << endl;
	}
}

CGcontext cgContext;
CGprogram cgProgram;
CGprofile cgFragmentProfile;

/**
 * Initialization of the shader
 */
int Viewer::init_shader() {
	// Setup Cg
	cgSetErrorCallback(cgErrorCallback);
	cgContext = cgCreateContext();

	// Validate Our Context Generation Was Successful
	if (cgContext == 0) {
		fprintf(stderr, "Failed To Create Cg Context\n");
		return -1;
	}

	cgFragmentProfile = cgGLGetLatestProfile(CG_GL_FRAGMENT);

	// Validate Our Profile Determination Was Successful
	if (cgFragmentProfile == CG_PROFILE_UNKNOWN) {
		fprintf(stderr, "Invalid profile type\n");
		return -1;
	}

	cgGLSetOptimalOptions(cgFragmentProfile);

	// Load And Compile The Vertex Shader From File
	cgProgram = cgCreateProgramFromFile(cgContext, CG_SOURCE,
			std::string(this->datadir + string(SHADER)).c_str(),
			cgFragmentProfile, "main", 0);

	// Validate Success
	if (cgProgram == 0) {
		// We Need To Determine What Went Wrong
		CGerror Error = cgGetError();

		// Show A Message Box Explaining What Went Wrong
		fprintf(stderr, "%s \n", cgGetErrorString(Error));
		return -1;
	}

	// Load The Program
	cgGLLoadProgram(cgProgram);

	return 1;
}

/**
 * Initialization of the FrameBuffer Object
 */
int Viewer::init_fbo() {
	GLenum error = glewInit();
	if (GLEW_OK != error) {
		/* Problem: glewInit failed, something is seriously wrong. */
		fprintf(stderr, "Error: %s\n", glewGetErrorString(error));
		return -1;
	}

	if (!GLEW_EXT_framebuffer_object) {
		cerr << "FBO ext missing\n";
		return -1;
	}

	GLenum status;

	glGenFramebuffersEXT(1, &framebuffer);

	// Set up the FBO with one texture attachment
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, framebuffer);
	glGenTextures(1, &texture);
	glBindTexture(GL_TEXTURE_2D, texture);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
	glTexImage2D(GL_TEXTURE_2D, 0, GL_RGB, FBO_W, FBO_H, 0, GL_RGB,
			GL_UNSIGNED_BYTE, NULL);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP);
	glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP);
	glBindTexture(GL_TEXTURE_2D, 0);

	glFramebufferTexture2DEXT(GL_FRAMEBUFFER_EXT, GL_COLOR_ATTACHMENT0_EXT,
			GL_TEXTURE_2D, texture, 0);

	status = glCheckFramebufferStatusEXT(GL_FRAMEBUFFER_EXT);
	glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);

#define test(x) case x: cout << #x << endl; break;

	switch (status) {
	test(GL_FRAMEBUFFER_UNSUPPORTED_EXT)
		;
		//test(GL_FRAMEBUFFER_COMPLETE_EXT);
	test(GL_FRAMEBUFFER_INCOMPLETE_ATTACHMENT_EXT)
		;
	test(GL_FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT_EXT)
		;
	test(GL_FRAMEBUFFER_INCOMPLETE_DIMENSIONS_EXT)
		;
	test(GL_FRAMEBUFFER_INCOMPLETE_FORMATS_EXT)
		;
	test(GL_FRAMEBUFFER_INCOMPLETE_DRAW_BUFFER_EXT)
		;
	test(GL_FRAMEBUFFER_INCOMPLETE_READ_BUFFER_EXT)
		;
	default:
		;//cout << "????" << endl;
	}

	if (status != GL_FRAMEBUFFER_COMPLETE_EXT) {
		cerr << gluErrorString(glGetError()) << endl;
		cerr << "NO FBO\n";
		return -1;
	}

	return 1;
}
#endif



/* ####################
 * MOUSE RELATED EVENTS
 */

/**
 * Something is being focused
 */
void Viewer::zoom_under_cursor(QMouseEvent* e) {
	if (graphIsAdl) {

	} else {
		beginSelection(e->pos());
		drawWithNames();
		endSelection(e->pos());

		VertexOrEdge voe = graphNet->find_by_num(selectedName());

		// A vertex is being focused
		if (voe.type == TYPE_VERTEX) {
			GVertexNet* vertex = (GVertexNet*) voe.voe;
			if (vertex != NULL) {
				this->zoomed_vertex = vertex;
				this->zoomed_edge = NULL;
				graphNet->draw(false, color_mode, this->show_only_distant_con,
						shape_color, host_color, this->zoomed_vertex,
						this->zoomed_edge);
				updateGL();
				return;
			}
		}

		// An edge is being focused
		if (voe.type == TYPE_EDGE) {
			GEdgeNet* edge = (GEdgeNet*) voe.voe;
			if (edge != NULL) {
				this->zoomed_vertex = NULL;
				this->zoomed_edge = edge;
				graphNet->draw(false, color_mode, this->show_only_distant_con,
						shape_color, host_color, this->zoomed_vertex,
						this->zoomed_edge);
				updateGL();
				return;
			}
		}

		// Nothing is being focused
		this->zoomed_vertex = NULL;
		this->zoomed_edge = NULL;
		graphNet->draw(false, color_mode, this->show_only_distant_con, shape_color,
				host_color, this->zoomed_vertex, this->zoomed_edge);
		updateGL();
	}
}

/**
 * Focus the element under the cursor if left mouse button is being pressed during normal mode
 */
void Viewer::mouseMoveEvent(QMouseEvent* e) {
	if (graph == NULL && graphNet == NULL)
		return;

	if (e->state() == Qt::RightButton) {
		if (selection_mode || color_changing_mode) {
			return;
		} // Not normal mode, return
		zoom_under_cursor(e);
		return;
	}
	else {
		GLint view[4];
		glGetIntegerv(GL_VIEWPORT, view);
		qglviewer::Vec view_bdbox_tl = convertPoint(QPoint(0, 0));
		qglviewer::Vec view_bdbox_br = convertPoint(
				QPoint(view[2] - 1, view[3] - 1));

		float posx = 0.5f * (view_bdbox_br.x + view_bdbox_tl.x);
		float posy = 0.5f * (view_bdbox_br.y + view_bdbox_tl.y);
		float width = 0.03f * min(view_bdbox_br.x - view_bdbox_tl.x,
				view_bdbox_br.y - view_bdbox_tl.y);

		qglviewer::Vec c;
		c[0] = posx;
		c[1] = posy;
		c[2] = 0;
		this->setSceneCenter(c);
		parent->Viewer2->refresh();
		QGLViewer::mouseMoveEvent(e);
	}
}

/**
 * No more focused object
 */
void Viewer::mouseReleaseEvent(QMouseEvent* e) {
	if (graph == NULL && graphNet == NULL)
		return;

	if (graphIsAdl) {
		// No more focused object
		this->zoomed_vertex = NULL;
		this->zoomed_edge = NULL;

		// Redraw graph
		graph->draw(this);
		updateGL();

		QGLViewer::mouseReleaseEvent(e);
	} else {
		// No more focused object
		this->zoomed_vertex = NULL;
		this->zoomed_edge = NULL;

		// Redraw graph
		graphNet->draw(false, color_mode, this->show_only_distant_con, shape_color,
				host_color, this->zoomed_vertex, this->zoomed_edge);
		updateGL();

		QGLViewer::mouseReleaseEvent(e);
	}
}


/**
 * Handle the mousePressEvent fonctions of current mode
 * Disable the non-ortho movements
 */
void Viewer::mousePressEvent(QMouseEvent* e) {
	if (graph == NULL && graphNet == NULL)
		return;

	if (graphIsAdl) {
		if (e->button() == Qt::RightButton) {
			// Get the position of the click
			/*
			 Can't get it to work properly.

			QPoint qp = e->pos();
			qglviewer::Vec cpos;
			cpos.x = qp.x();
			cpos.y = -qp.y();
			cpos.z = 0;
			//this->camera()->setSceneCenterFromPixel(qp);
			qglviewer::Vec endPoint = this->camera()->worldCoordinatesOf(cpos);
			displayInfo("selecting: "+toStr(endPoint.x)+" "+toStr(endPoint.y));
			//displayInfo("selecting: "+toStr(cpos.x)+" "+toStr(cpos.y));
			select_component_adl(endPoint);
			*/
		}
	} else {
		if ((selection_mode || color_changing_mode) && e->button()
				== Qt::RightButton) {
			// We need yo know the element under the cursor.
			// We call QGLViewer select() function.
			select(e->pos());
			return;
		} else if (e->button() == Qt::RightButton) {
			// Normal mode: magnification of the under-cursor element
			zoom_under_cursor(e);
			return;
		}
	}

	QGLViewer::mousePressEvent(e);
}

/**
 * Check camera.pos.z remains >= ZOOM_LIMIT
 */
void Viewer::wheelEvent(QWheelEvent * const event) {
	if (graph == NULL && graphNet == NULL)
		return;

	if (event->delta() < 0) {
		// zoom in
		qglviewer::Vec cam_pos = camera()->position();
		if (cam_pos.z <= ZOOM_LIMIT)
			return;
	}
	parent->Viewer2->refresh();

	QGLViewer::wheelEvent(event);
	return;
}

/* ajout : implem de la methode paintCell pour Q3ListViewItem */
void paintCell(QPainter * p, const QPalette & cg, int column, int width,
		int align) {

	QColorGroup _cg(cg);
	QColor oldText = _cg.text();
	_cg.setColor(QColorGroup::Text, QColor(230, 0, 0));
	paintCell(p, (QColorGroup&) _cg, column, width, align);
	_cg.setColor(QColorGroup::Text, oldText);
}

/**
 * To be able to write colored text in the tree
 */
class QListViewItemColor: public Q3ListViewItem {
private:
	QColor color;
public:
	QListViewItemColor(Q3ListViewItem *parent, QString label, QColor color) :
		Q3ListViewItem(parent, label) {
		this->color = color;
	}
	~QListViewItemColor() {
	}
	//void paintCell( QPainter * p, const QColorGroup & cg, int column, int width, int align )
	void paintCell(QPainter * p, const QPalette & cg, int column, int width,
			int align) {
		/*
		 QColorGroup _cg( cg );
		 QColor oldText=_cg.text();
		 _cg.setColor( QColorGroup::Text, color );
		 //         Q3ListViewItem::
		 //Q3ListViewItem::paintCell( p, (QColorGroup&)_cg, column, width, align );
		 //Q3CheckListItem::paintCell( p, _cg, column, width, align );
		 //QListViewItemColor::
		 //paintCell( p, _cg, column, width, align );
		 _cg.setColor( QColorGroup::Text, oldText );*/
	}
	//      void paintFocus( QPainter * p, const QPalette & cg, int column, int width, int align )
	void paintFocus(QPainter *, const QPalette &cg, const QRect & r) {
	}

	void paintBranches(QPainter * p, const QPalette & cg, int column,
			int width, int align) {
	}
};




/* #########
 * UTILITIES
 */

/**
 * Merge 2 list<QString>
 */
void merge_lists(list<QString> *l1, list<QString> *l2) {
	list<QString>::const_iterator iter;
	for (iter = l2->begin(); iter != l2->end(); iter++) {
		l1->push_back(*iter);
	}
}

/**
 * Comparison between QString and std::string
 */
bool equal(QString str1, std::string str2) {
	return str1 == QString(str2.c_str());
}

/**
 * Return recursively all the child of parent QListViewItem
 */
list<QString> get_item_child(Q3ListViewItem* parent, int column, bool firstOnly) {
	//if (parent->text(column) != "")
	//	; //NULL)
	{
		list < QString > res;
		res.push_back(parent->text(column));
		return res;
	}

	list < QString > res;
	Q3ListViewItem* enfant = parent->firstChild();
	while (enfant) {
		list < QString > ltmp = get_item_child(enfant, column, firstOnly);
		if (firstOnly && ltmp.size() > 0)
			return ltmp;
		merge_lists(&res, &ltmp);
		enfant = enfant->nextSibling();
	}
	return res;
}

/**
 * Return the list of vertices or edges corresponding to the list of ids
 */
list<VertexOrEdge> Viewer::get_vertices_or_edges_list(list<QString> *strl)
{
   list<VertexOrEdge> res;
   list<QString>::iterator iter;

   // Search in the vertices list
   liste_vertices* cur_vertex = graphNet->get_vertices();
   while(cur_vertex != NULL)
   {
      iter = std::find(strl->begin(), strl->end(), QString((cur_vertex->vertex->get_id()->c_str())));
      if (iter != strl->end())
      {
         strl->erase(iter);
         VertexOrEdge voe = {(void*)cur_vertex->vertex, TYPE_VERTEX};
         res.push_back(voe);
      }
      cur_vertex = cur_vertex->suiv;
   }

   // Check there is anything else to search
   if (strl->size() == 0)
   {
      return res;
   }

   // Search in the edges list
   liste_edges* cur_edge = graphNet->get_edges();
   while(cur_edge != NULL)
   {
      iter = std::find(strl->begin(), strl->end(), QString((cur_edge->edge->get_label()->c_str())));
      if (iter != strl->end())
      {
         strl->erase(iter);
         VertexOrEdge voe = {(void*)cur_edge->edge, TYPE_EDGE};
         res.push_back(voe);
      }
      cur_edge = cur_edge->suiv;
   }

   return res;
}

/**
 * Center the view (and select in selection mode) the elements corresponding to the list items
 */
void Viewer::selectItem(Q3ListViewItem* item, bool firstOnly, int column, bool extractId) {
	if (graphIsAdl) {
		string idStr;
		if (extractId) {
			// {item comes from the XML treeview}
			// XML items have a structure; we need to extract the id field from the name
			string itemStr = string(item->text(0));
			string searchStr = "id='";
			int pos = itemStr.find(searchStr);
			if (pos!=string::npos) {
				idStr = itemStr.substr((int)pos+4);
				// {idStr has the id + the rest of the string}
				int pos2 = idStr.find_first_of("'");
				idStr = idStr.substr(0, pos2);

				// {idStr successfully extracted}
			} else {
				return;
				// {could not extract it}
			}
			// {item comes from the ID or host treeview}
		} else {
			idStr = string(item->text(0));
		}


		//
		// Is it a primitive?
		//
		GNode* candidate;
		// We must eliminate the root cluster


		string searchName;
		if (extractId) {
			searchName = idStr;
		} else {
			searchName = string(item->text(column));
		}


		int stripPos = searchName.find_first_of("/");
		searchName = searchName.substr(stripPos+1);
		//this->displayInfo("searching for: "+searchName);

		candidate = graph->rootCluster->findNodeByName(searchName, this, idStr);


		if (candidate != NULL) {
			//selectedNodes.push_back(candidate);
			setSceneCenter(qglviewer::Vec(candidate->x, candidate->y, 0));
			camera()->centerScene();
			updateGL();
			this->parent->Viewer2->refresh();
			return;
			// {we've snapped to desired position}
		}
		// {apparently, item is not a primitive}

		//
		// Is it a cluster?
		//
		string clusterSearchName;

		if (extractId) {
			clusterSearchName = idStr;
		} else {
			clusterSearchName = string(item->text(column));
			Q3ListViewItem* parent = item->parent();
			while (parent != NULL) {
				clusterSearchName = string(parent->text(column)) + "/" + clusterSearchName;
				parent = parent->parent();
			}
		}
		//this->displayInfo("Searching with "+clusterSearchName);
		GCluster* candidateCluster = graph->rootCluster->findClusterByName(clusterSearchName, this);
		if (candidateCluster != NULL) {
			setSceneCenter(qglviewer::Vec((candidateCluster->x1+candidateCluster->x2)/2, candidateCluster->y2-30, 0));
			camera()->centerScene();
			updateGL();
			this->parent->Viewer2->refresh();
			return;
		}
		// {item is not a cluster either}
		// {done}
	} else {
		// Get the list of selected items
		list < QString > selection = get_item_child(item, column, firstOnly);
		list < VertexOrEdge > vertices_or_edges = get_vertices_or_edges_list(
				&selection);

		if (vertices_or_edges.size() == 0)
			return;

		if (selection_mode) {
			cur_selection = vertices_or_edges;
			update_selection();
		}

		// center the view =>

		// Compute the center of mass
		list<VertexOrEdge>::const_iterator iter;
		float centre_x = 0.0f;
		float centre_y = 0.0f;
		int nb_elements = 0;
		for (iter = vertices_or_edges.begin(); iter != vertices_or_edges.end(); iter++) {
			if ((*iter).type == TYPE_VERTEX) {
				centre_x += ((GVertexNet*) (*iter).voe)->get_pos_x();
				centre_y += ((GVertexNet*) (*iter).voe)->get_pos_y();
				nb_elements++;
			} else if ((*iter).type == TYPE_EDGE) {
				centre_x
						+= ((GVertexNet*) ((GEdgeNet*) (*iter).voe)->vertex_head)->get_pos_x();
				centre_y
						+= ((GVertexNet*) ((GEdgeNet*) (*iter).voe)->vertex_head)->get_pos_y();
				centre_x
						+= ((GVertexNet*) ((GEdgeNet*) (*iter).voe)->vertex_tail)->get_pos_x();
				centre_y
						+= ((GVertexNet*) ((GEdgeNet*) (*iter).voe)->vertex_tail)->get_pos_y();
				nb_elements += 2;
			}
		}
		centre_x /= (float) nb_elements;
		centre_y /= (float) nb_elements;

		// Center the view on the center of mass
		setSceneCenter(qglviewer::Vec(centre_x, centre_y, 0));
		camera()->centerScene();
		updateGL();
	}
}

void Viewer::select_component_adl(qglviewer::Vec p) {
	GNode* selectedNode = graph->rootCluster->findNodeByPosition(p.x, p.y, this);
	if (selectedNode != NULL) {
		displayInfo(toStr(selectedNode->x)+" "+toStr(selectedNode->y));
	}
}


/**
 * Convert a pixel pos into the 3D corresponding coords with z=0.
 */
qglviewer::Vec Viewer::convertPoint(QPoint pos) {
	qglviewer::Vec orig;
	qglviewer::Vec dir;
	camera()->convertClickToLine(pos, orig, dir);
	float c = -orig.z / dir.z;
	return qglviewer::Vec(orig.x + dir.x * c, orig.y + dir.y * c, 0.0f);
}






/* ##############
 * LIST VIEW EVENT HANDLING
 */

/**
 * Event: selection changed in the first tree
 */
void Viewer::listView1_selectionChanged(Q3ListViewItem* item) {
	if (item == NULL)
		return;
	selectItem(item, false, 0);
}

/**
 * Event: selection changed il the second tree
 */
void Viewer::listView2_selectionChanged(Q3ListViewItem* item) {
	if (item == NULL)
		return;
	selectItem(item, false, 0);
}

/**
 * Event: selection changed in the connections tree
 */
void Viewer::listViewLinks_selectionChanged(Q3ListViewItem* item) {
	if (item == NULL)
		return;
	if (item->depth() == 0)
		return;
	selectItem(item, false, 0);
}

/**
 * Event: selection changed il the XML tree
 */
void Viewer::listViewXml_selectionChanged(Q3ListViewItem* item) {
	if (item == NULL)
		return;
	selectItem(item, true, 2, true);
}

/**
 * Event: "distant connections" checkBox toggled
 * Change show_only_distant_con boolean and redraw the graph
 */
void Viewer::checkBox1_toggled(bool b) {
	if (graph != NULL) {
		this->show_only_distant_con = b;
		graph->draw(this);
		updateGL();
	}
}





/* ########################
 * VISIBILITY AND SELECTION
 */

/**
 * Make every vertex and edge visible and unselect them.
 */
void Viewer::everything_visible(bool visible) {
	liste_vertices* cur_vertex = graphNet->get_vertices();
	while (cur_vertex != NULL) {
		cur_vertex->vertex->set_selected(false);
		cur_vertex->vertex->set_visible(visible);
		cur_vertex = cur_vertex->suiv;
	}

	liste_edges* cur_edge = graphNet->get_edges();
	while (cur_edge != NULL) {
		cur_edge->edge->set_visible(visible);
		cur_edge->edge->super_visible = false;
		cur_edge = cur_edge->suiv;
	}
	updateGL();

}

/**
 *
 */
void Viewer::update_selection() {
	graphNet->select(&cur_selection, niveau_exploration, mode_exploration,
			max_depth);
	updateGL();
	parent->statusbar->setText(
			QString("Depth: ") + QString::number(niveau_exploration));
}

/**
 * Post selection QGLViewer function
 * Selection of a vertex, an edge or nothing by clicking
 */
void Viewer::postSelection(const QPoint&) {
	if (graphNet == NULL && graph == NULL)
		return;

	if (graphIsAdl) {
		displayInfo("Aaa");
	} else {
		if (color_changing_mode && selectedName() > 0) {
			// Changing the color of the element
			if (color_mode == 0) {
				int shape = graphNet->find_shape(selectedName());
				if (shape == -1)
					return;
				shape_color[shape * 3 + 0] = (float) cur_color.red() / 255.0f;
				shape_color[shape * 3 + 1] = (float) cur_color.green() / 255.0f;
				shape_color[shape * 3 + 2] = (float) cur_color.blue() / 255.0f;
				fill_tree_ids();
				fill_tree_hosts();
			} else if (color_mode == 1) {
				int host = graphNet->find_host(selectedName());
				if (host == -1)
					return;
				host_color[host * 3 + 0] = (float) cur_color.red() / 255.0f;
				host_color[host * 3 + 1] = (float) cur_color.green() / 255.0f;
				host_color[host * 3 + 2] = (float) cur_color.blue() / 255.0f;
				fill_tree_hosts();
			} else {
				graphNet->change_vertex_color(selectedName(),
						(float) cur_color.red() / 255.0f,
						(float) cur_color.green() / 255.0f,
						(float) cur_color.blue() / 255.0f);
			}
		} else if (selection_mode) {
			// Selecting the element
			if (selectedName() > 0) {
				VertexOrEdge selected_element = graphNet->find_by_num(selectedName());
				if (selected_element.type == -1)
					return;
				cur_selection.clear();
				cur_selection.push_back(selected_element);
				update_selection();

				if (selected_element.type == 0) // the selected element is a vertex
				{
					Q3ListViewItem* item;

					item
							= parent->listView1->findItem(
									QString(
											(((GVertexNet*) selected_element.voe)->get_id()->c_str())),
									2);
					parent->listView1->ensureItemVisible(item);
					parent->listView1->blockSignals(true);
					parent->listView1->setSelected(item, true);
					parent->listView1->blockSignals(false);

					item
							= parent->listView2->findItem(
									QString(
											(((GVertexNet*) selected_element.voe)->get_id()->c_str())),
									2);
					parent->listView2->ensureItemVisible(item);
					parent->listView2->blockSignals(true);
					parent->listView2->setSelected(item, true);
					parent->listView2->blockSignals(false);

					item
							= parent->listViewXml->findItem(
									QString(
											(((GVertexNet*) selected_element.voe)->get_id()->c_str())),
									1);
					parent->listViewXml->ensureItemVisible(item);
					parent->listViewXml->blockSignals(true);
					parent->listViewXml->setSelected(item, true);
					parent->listViewXml->blockSignals(false);

				} else if (selected_element.type == 1) //the selected element is an edge
				{
					Q3ListViewItem
							* item =
									parent->listViewLinks->findItem(
											QString(
													(((GEdgeNet*) selected_element.voe)->get_label()->c_str())),
											1);
					parent->listViewLinks->ensureItemVisible(item);
					parent->listViewLinks->blockSignals(true);
					parent->listViewLinks->setSelected(item, true);
					parent->listViewLinks->blockSignals(false);

					item
							= parent->listViewXml->findItem(
									QString(
											(((GEdgeNet*) selected_element.voe)->get_label()->c_str())),
									1);
					parent->listViewXml->ensureItemVisible(item);
					parent->listViewXml->blockSignals(true);
					parent->listViewXml->setSelected(item, true);
					parent->listViewXml->blockSignals(false);

					((GEdgeNet*) selected_element.voe)->super_visible = true;
				}
				setSelectedName(-1);

			} else {
				cur_selection.clear();
				setSelectedName(-1);
				everything_visible(true);
			}
		}
	}
}

/**
 * Toggle selection mode
 */
void Viewer::toggle_select(bool b) {
	if (b) {
		//parent->setCursor(QCursor(Qt::PointingHandCursor));
		selection_mode = true;
		color_changing_mode = false;
		tb_colorchg->blockSignals(true);
		tb_colorchg->setOn(false);
		tb_colorchg->blockSignals(false);
		parent->statusbar->setText(QString("Selection mode"));
	} else {
		//parent->setCursor(QCursor(Qt::ArrowCursor));
		selection_mode = false;
		everything_visible(true);
	}
}









/* ###############
 * PARSING-RELATED
 */


/**
 * Write a .dot file from a .net.xml file
 */
bool net2dot(const char* filename_net, const char* filename_dot, string xsl_file_p)
{
   std::string xsl_file = xsl_file_p;
   xsltStylesheetPtr stylesheet = xsltParseStylesheetFile((const xmlChar *)(char*)xsl_file.c_str());
   xmlDocPtr file_xml           = xmlParseFile(filename_net);
   if (file_xml == NULL)
   {
      cout << "Error parsing the .net.xml file !" << endl;
      return false;
   }
   xmlDocPtr res                = xsltApplyStylesheet(stylesheet, file_xml, NULL);
   if (res == NULL)
   {
      cout << "Error parsing the .net.xml file !" << endl;
      return false;
   }

   FILE *output_file = fopen(filename_dot,"w");
   xsltSaveResultToFile(output_file, res, stylesheet);
   fclose(output_file);

   xsltCleanupGlobals();
   xmlCleanupParser();

   return true;
}

/**
 * Compute the layout of the graphviz graph g using graphviz library.
 * Write the result in a dot file.
 */
void compute_layout(Agraph_t* g, const char* filename_dot_layout)
{
   static GVC_t *gvc;
   if (gvc) {
	   gvFreeContext(gvc);
   }
   gvc = gvContext();
   gvLayout(gvc, g, (char*)"dot");

   // We save the result into a dot file
   FILE* fout = fopen(filename_dot_layout, "w");
   gvRender(gvc, g, (char*)"dot",fout);
   fclose(fout);
}



bool Viewer::adl2dot(const char* filename_adl, const char* filename_dot) {
	// Parse .adl
	//QMessageBox::information(this, "FlowVR glGraph", "About to parse the adl.");

	Component* tree = parseAdl((char*) filename_adl, this);
	if (tree == NULL) {
		return false;
	} else {

		//QMessageBox::information(this, "FlowVR glGraph", ".adl parsed");

		//this->displayInfo("Rendering nodes");

		//browseNodes(tree, this);

		std::ofstream Fdot;
		Fdot.open(filename_dot);

		Fdot << "digraph G {" << endl << " style=filled;" << endl << " fillcolor=white;" << endl;
		renderNode(tree, Fdot, 0, this);
		Fdot << "}" << endl;

		Fdot.close();

		if (adlTree != NULL)
			delete adlTree;
		this->adlTree = tree;

		xmlCleanupParser();
		return true;
	}
}



/**
 * Event: load file button clicked
 */
void Viewer::load_file() {
	QString fileName = Q3FileDialog::getOpenFileName("./", "Graph (*.xml)",
			this);
	load_file_2(fileName);
}


void ignoreHierarchy(string fileName) {
	std::ifstream infile;
	infile.open(fileName.c_str());
	
	string outfileName = fileName;
	outfileName += ".nohierarchy";
	std::ofstream outfile;
	outfile.open(outfileName.c_str());
	
	for (std::string line; getline(infile, line); ) {
		if (line.size() >= 9 && line.substr(0, 8) != "subgraph"
			&& (line.size() != 1 && line != "}")) {
			// write to outfile only if string isn't subgraph declaration or closure
			outfile << line << std::endl;
		}
	}
	
	// append a closing bracket
	outfile << "}" << std::endl;
	
	outfile.close();
	infile.close();
	
	rename(outfileName.c_str(), fileName.c_str());
}


/**
 * Actually load the given file
 */
void Viewer::load_file_2(QString fileName) {
	if (fileName == "")
		return;

	if (std::ifstream(fileName) == NULL) {
		QMessageBox::warning(this, "File problem",
							"Error: File does not exist!");
	}

	if (fileName.endsWith(".adl.out.xml")) {
		init();
		// adl file

		parent->listView1->clear();
		parent->listView2->clear();
		parent->listViewLinks->clear();
		parent->listViewXml->clear();

		if (!adl2dot(fileName, fileName + ".dot")) {
			QMessageBox::warning(this, "FlowVR glGraph",
					"Error parsing the XML file.\n\nAre you certain that the file is well-formed?");
			return;
		}
		graphIsAdl = true;
		//fileName = fileName + ".dot";

		FILE* file = fopen(fileName + ".dot", "r");

		// Set Graphviz context
		GVC_t* gvc;
		graph_t* g;

		gvc = gvContext();
#if USING_CGRAPH
		g = agread(file, NULL);
#else
		g = agread(file);
#endif

		char dotTable[4] = "dot";
		gvLayout(gvc, g, dotTable);

		// Effectively fills graph with layout information
		attach_attrs(g);

		// Output it for external use
		FILE* file_layout = fopen(fileName + ".layout.dot", "w");
		gvRender(gvc, g, dotTable, file_layout);
		fclose(file_layout);
		fclose(file);
		unlink(fileName + ".dot");
		unlink(fileName + ".layout.dot");
		if (!AUTO_SAVE_DOT) {
			remove(fileName + ".dot");
			remove(fileName + ".layout.dot");
		}


		// Render our final graph representation
		Graph* ggraph = new Graph(g, adlTree, this);

		graph = ggraph;

		gvFreeLayout(gvc, g);
		agclose(g);



		hosts.clear();
		assign_color_to_hosts();
		fill_tree();
		cur_selection.clear();


		graph->getBoundingBox(this);
		init_camera();

		parent->Viewer2->refresh();

		/*
		QMessageBox::information(this, "FlowVR glGraph",
						"Camera initialized and Viewer refreshed.");
						*/

		this->gv_graph = g;

		// window caption
		main_window->setCaption("FlowVR Graph Visualizer: " + fileName);
		current_fileName = fileName;

		// XML Tree fill
		parent->listViewXml->clear();
		Q3ListViewItem* firstItem = new Q3ListViewItem(parent->listViewXml,
						"root");
		firstItem->setOpen(true);
		fill_tree_xml_adl(firstItem, adlTree);
		/*
		QMessageBox::information(this, "FlowVR glGraph",
						"That's it.");
*/

		/*
		 QMessageBox::information(
		 this,
		 "Unsupported format",
				".adl hierarchical graphs are not yet implemented.\n\nYou could use the adl2dot Python script instead:\nhttps://wiki-grimage.imag.fr/trac/browser/trunk/dev/martinx");
		*/
	} else if (fileName.endsWith(".net.xml")) {
		init();
		current_fileName = fileName;
		graphIsAdl = false;

		if (!net2dot(fileName, fileName + ".dot", this->xsl_file)) {
			QMessageBox::information(this, "flowvr-graph",
					"Error parsing the XML file !");
			return;
		}
		fileName = fileName + ".dot";
		// Now, fileName is a dot file
		
		if (!netHierarchicalView)
			ignoreHierarchy(fileName.toStdString());

		FILE* file = fopen(fileName, "r");
		
#if USING_CGRAPH
		Agraph_t* g = agread(file, NULL);
#else
		aginit();
		Agraph_t* g = agread(file); 
#endif
		if (g == NULL) {
			QMessageBox::information(this, "flowvr-graph",
					"Error parsing the initial DOT file !");
			return;
		}
		
		Agnode_t* n = agfstnode(g);
		if (n == NULL) {
			QMessageBox::information(this, "flowvr-glgraph",
					"Empty or corrupt file, could not find a module to display.");
			unlink(fileName);
			return;
		}
		char* p = agget(n, (char*) "pos");
		// Is there layout information in the file ?
		if (!p) {
			// No layout information => we call gvLayout()
			fileName.truncate(fileName.length() - 4);
			compute_layout(g, fileName + "_layout.dot");
		}

		// Remove temporary files
		if (!AUTO_SAVE_DOT) {
			unlink(fileName + ".dot");
			unlink(fileName + "_layout.dot");
		}

		// Creation of the GGraph objet
		graphNet = new GraphNet(g);

		hosts.clear();
		assign_color_to_hosts();
		fill_tree();

		cur_selection.clear();

		parent->Viewer1->graphNet->get_bounding_box(&(graphNet->bounding_left),
				&(graphNet->bounding_right), &(graphNet->bounding_top),
				&(graphNet->bounding_bottom));
		init_camera();
		parent->Viewer2->refresh();

		this->gv_graph = g;
		// window caption
		main_window->setCaption("Flowvr Graph Visualizer: " + fileName);
		current_fileName = fileName;

		parent->listViewXml->clear();
		Q3ListViewItem* firstItem = new Q3ListViewItem(parent->listViewXml,
				"root");
		firstItem->setOpen(true);
		streamXmlFile(fileName, parent->listViewXml->firstChild());
	} else {
	// Unknown format
		QMessageBox::information(
				this,
				"Unknown format",
				"The format of the XML file you attempted to open could not be identified (based on file extension).\n\nTry opening .adl.out.xml or .net.xml files instead.");
		graphIsAdl = false;
	}
}













/* #############
 * FILLING TREES
 */

/////////////////////////////////////////////////////////////////////////////////
//>>>////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////
// Filling the 3 trees


/**
 * Find an entry in a QListView from a std::string
 */
Q3ListViewItem* find_child_item(Q3ListViewItem* parent, std::string name) {
	Q3ListViewItem* enfant = parent->firstChild();
	while (enfant) {
		if (equal(enfant->text(0), name)) {
			return enfant;
		}
		enfant = enfant->nextSibling();
	}
	return NULL;
}

/**
 * Recursive function for adding an id in the qt tree.
 * The id looks like: "foo/bar/toto"
 */
Q3ListViewItem* add_qlist(Q3ListViewItem* parent, std::string id, QColor color) {
	unsigned int c;
	for (c = 0; c < id.length(); c++) {
		if (id[c] == '/') {
			id[c] = 0;
			Q3ListViewItem * item = find_child_item(parent, std::string(id));
			if (item == NULL) {
				item = new Q3ListViewItem(parent, QString(id.c_str()));
				//              item = new QListViewItemColor( parent, QString(id.c_str()), QColor(0,0,0));
			}
			return add_qlist(item, &(id[c + 1]), color);
		}
	}
	//QListViewItemColor * item = new QListViewItemColor(parent, QString(id.c_str()), color);
	Q3ListViewItem * item = new Q3ListViewItem(parent, QString(id.c_str()));
	return item;
}

/**
 * Fill the tree (QListView) of the modules sorted by host.
 */
void Viewer::fill_tree_hosts() {
	if (graphIsAdl) {
		parent->listView2->clear();
		Q3ListViewItem* item = new Q3ListViewItem(parent->listView2, "hosts");
		item->setOpen(true);

		set<string> hostNames;
		vector< pair<string,Q3ListViewItem*> > hostListPair;
		// Let's look for every host
		graph->rootCluster->fillHostsVector(hostNames);
		for (set<string>::iterator h=hostNames.begin(); h!=hostNames.end(); h++) {
			// For every host, add an item
			Q3ListViewItem* hostItem = new Q3ListViewItem(item, (*h).c_str());
			pair<string,Q3ListViewItem*> pair ((*h), hostItem);
			hostListPair.push_back(pair);
		}
		// {host pair list filled}

		// Fill the pair list
		graph->rootCluster->fillHostPairList(hostListPair);
		// {listview filled}

		// Compute additional information
		// Number of children
		for (vector< pair<string,Q3ListViewItem*> >::iterator pair=hostListPair.begin(); pair!=hostListPair.end(); pair++) {
			Q3ListViewItem* currentItem = (*pair).second;
			ostringstream out;
			out << currentItem->childCount();
			string newLabel;
			if (currentItem->childCount() == 1)
				newLabel = string(currentItem->text(0))+" ("+out.str()+" component)";
			else
				newLabel = string(currentItem->text(0))+" ("+out.str()+" components)";

			currentItem->setText(0, QString(newLabel.c_str()));
		}
	} else {
		parent->listView2->clear();
		Q3ListViewItem* item_hosts = new Q3ListViewItem(parent->listView2, "hosts");
		item_hosts->setOpen(true);
		Q3ListViewItem* new_item;

		QPixmap pm(10, 10);

		// Add the hosts
		int h = 0;
		list<string>::iterator ite;
		for (ite = hosts.begin(); ite != hosts.end(); ++ite) {
			new_item = add_qlist(item_hosts, *ite, QColor(0, 0, 0));
			pm.fill(
					QColor((int) (host_color[h * 3 + 0] * 255.0f),
							(int) (host_color[h * 3 + 1] * 255.0f),
							(int) (host_color[h * 3 + 2] * 255.0f)));
			new_item->setPixmap(0, pm);
			h++;
		}

		// Add the vertices
		liste_vertices* cur_vertex = graphNet->get_vertices();
		while (cur_vertex != NULL) {
			std::string id_host = *(cur_vertex->vertex->get_host()) + std::string(
					"/") + *(cur_vertex->vertex->get_id());
			new_item = add_qlist(
					item_hosts,
					id_host,
					QColor(
							(int) (shape_color[cur_vertex->vertex->shape * 3 + 0]
									* 255.0f * COLOR_DARKENING_IN_LISTS),
							(int) (shape_color[cur_vertex->vertex->shape * 3 + 1]
									* 255.0f * COLOR_DARKENING_IN_LISTS),
							(int) (shape_color[cur_vertex->vertex->shape * 3 + 2]
									* 255.0f * COLOR_DARKENING_IN_LISTS)));
			new_item->setText(0, QString((cur_vertex->vertex->get_id()->c_str())));

			cur_vertex = cur_vertex->suiv;
		}
	}
}


/**
 * Fill the tree (QListView) of the modules sorted by id
 */
void Viewer::fill_tree_ids() {
	if (graphIsAdl) {
		parent->listView1->clear();
		Q3ListViewItem* item = new Q3ListViewItem(parent->listView1, graph->rootCluster->label.c_str());
		item->setOpen(true);
		fill_tree_ids_adl(item, graph->rootCluster);
	} else {
		parent->listView1->clear();
		Q3ListViewItem* item = new Q3ListViewItem(parent->listView1, "graph");
		item->setOpen(true);
		Q3ListViewItem* new_item;

		liste_vertices* cur_vertex = graphNet->get_vertices();
		while (cur_vertex != NULL) {
			new_item = add_qlist(
					item,
					*(cur_vertex->vertex->get_id()),
					QColor(
							(int) (shape_color[cur_vertex->vertex->shape * 3
									+ 0] * 255.0f * COLOR_DARKENING_IN_LISTS),
							(int) (shape_color[cur_vertex->vertex->shape * 3
									+ 1] * 255.0f * COLOR_DARKENING_IN_LISTS),
							(int) (shape_color[cur_vertex->vertex->shape * 3
									+ 2] * 255.0f * COLOR_DARKENING_IN_LISTS)));
			new_item->setText(1, (cur_vertex->vertex->get_host()->c_str()));
			new_item->setText(0, (cur_vertex->vertex->get_id()->c_str()));
			new_item->setOpen(true);

			cur_vertex = cur_vertex->suiv;
		}
	}
}

void Viewer::fill_tree_ids_adl(Q3ListViewItem* parent, GCluster* cl) {
	//
	// Local nodes
	//
	for (list<GNode*>::iterator itNode=cl->localNodes.begin(); itNode != cl->localNodes.end(); itNode++) {
		Q3ListViewItem* item = new Q3ListViewItem(parent, (*itNode)->componentLabel.c_str());
	}
	//
	// Subclusters
	//
	for (list<GCluster*>::iterator itCl=cl->subClusters.begin(); itCl != cl->subClusters.end(); itCl++) {
		// for every sub cluster
		string label = (*itCl)->label.c_str();
		int pos = label.find_first_of("/");
		label = label.substr(pos+1);
		Q3ListViewItem* item = new Q3ListViewItem(parent, label.c_str());
		fill_tree_ids_adl(item, (*itCl));
	}
	//Q3ListViewItem* item = new Q3ListViewItem(parent, cl->id);
}

/**
 * Fill the tree (QListView) of the connections
 */
void Viewer::fill_tree_links() {
	if (graphIsAdl) {
		parent->listViewLinks->clear();
	} else {
		parent->listViewLinks->clear();
		Q3ListViewItem * item = new Q3ListViewItem(parent->listViewLinks, 0);
		item->setOpen(true);
		item->setText(0, "connections");
		Q3ListViewItem * new_item;

		liste_edges* cur_edge = graphNet->get_edges();
		while (cur_edge != NULL) {
			if (cur_edge->edge->style == EDGE_STYLE_DASHED)
				new_item = add_qlist(item, *(cur_edge->edge->get_label()),
						QColor(160, 160, 160));
			else
				new_item = add_qlist(item, *(cur_edge->edge->get_label()),
						QColor(0, 0, 0));

			new_item->setText(0, (cur_edge->edge->get_label()->c_str()));
			cur_edge = cur_edge->suiv;
		}
	}
}

void Viewer::fill_tree_xml_adl(Q3ListViewItem* parent, Component* comp) {
	// #######################
	// Create an item for the current component
	//
	string compStr = comp->type;
	compStr = compStr + " [id='" + comp->id+"']";
	Q3ListViewItem* item = new Q3ListViewItem(parent, compStr.c_str());




	// #######################
	//	Handle it's hosts, dump them in a subitem
	//
	if (!comp->hosts.empty()) {
		// {component has hosts}
		Q3ListViewItem* hosts = new Q3ListViewItem(item, "# hosts");

		// [For each param]
		for (list<Host*>::iterator host=comp->hosts.begin(); host!=comp->hosts.end(); host++) {
			string hostStr = (*host)->value;
			if ((*host)->fromVal != "") {
				hostStr.append(" [from='" + (*host)->fromVal +"']");
			}
			Q3ListViewItem* hostItem = new Q3ListViewItem(hosts, hostStr.c_str());
		}
	}

	// #######################
	//	Handle it's ports, dump them in a subitem
	//
	if (!comp->ports.empty()) {
		// {component has ports}
		Q3ListViewItem* ports = new Q3ListViewItem(item, "# ports");

		// [For each port]
		for (list<Port*>::iterator port=comp->ports.begin(); port!=comp->ports.end(); port++) {
			string portStr = (*port)->id;
			portStr = portStr + " [type=" + (*port)->type + "; msgtype=" + (*port)->msgtype + "; blockstate=" + (*port)->blockstate + "]";
			Q3ListViewItem* portItem = new Q3ListViewItem(ports, portStr.c_str());
		}
	}

	// #######################
	//	Handle it's parameters, dump them in a subitem
	//
	if (!comp->parameters.empty()) {
		// {component has parameters}
		Q3ListViewItem* parameters = new Q3ListViewItem(item, "# parameters");

		// [For each param]
		for (list<Parameter*>::iterator param=comp->parameters.begin(); param!=comp->parameters.end(); param++) {
			string paramStr = (*param)->nodeName;
			paramStr = paramStr + " [value='" + (*param)->value + "'; from='" + (*param)->fromWhere + "']";
			Q3ListViewItem* paramItem = new Q3ListViewItem(parameters, paramStr.c_str());
		}
	}

	// #######################
	//	Handle it's links, dump them in a subitem
	//
	if (!comp->links.empty()) {
		// {component has links}
		Q3ListViewItem* links = new Q3ListViewItem(item, "# links");

		// [For each link]
		for (list<LinkBetweenComponents*>::iterator link=comp->links.begin(); link!=comp->links.end(); link++) {
			string linkStr = "link";
			linkStr = linkStr + " [source='" + (*link)->source + "'; outport='" + (*link)->outport + "'; dest='" + (*link)->dest + "'; inport='" + (*link)->inport + "']";
			Q3ListViewItem* linkItem = new Q3ListViewItem(links, linkStr.c_str());
		}
	}

	// #######################
	// Take care of it's children
	//
	/*
	if (!comp->children.empty()) {
		// {component has children}
		Q3ListViewItem* childrenItem = new Q3ListViewItem(item, "# children");
*/
		// [For each child]
		for (list<Component*>::iterator child=comp->children.begin(); child!=comp->children.end(); child++) {
			fill_tree_xml_adl(item, *child);
		}
	//}
}

/**
 * Filling of the 3 trees
 */
void Viewer::fill_tree() {
	fill_tree_ids();
	fill_tree_hosts();
	fill_tree_links();

	parent->listView1->setFocusPolicy(Qt::NoFocus);
	parent->listView2->setFocusPolicy(Qt::NoFocus);
	parent->listViewLinks->setFocusPolicy(Qt::NoFocus);
	parent->listViewXml->setFocusPolicy(Qt::NoFocus);
}
/////////////////////////////////////////////////////////////////////////////////
//<<<////////////////////////////////////////////////////////////////////////////
/////////////////////////////////////////////////////////////////////////////////






/* #########################
 * DRAWING RELATED FUNCTIONS
 */

/**
 * Draw the graph
 */
void Viewer::draw2(bool fast) {
#ifndef NOCG
	if (shader_enabled) {
		glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, framebuffer);
		glViewport(0, 0, FBO_W, FBO_H);
		glClear( GL_COLOR_BUFFER_BIT);
	}
#endif

	if (graph != NULL)
		graph->draw(this);
	else if (graphNet != NULL)
          graphNet->draw(fast, color_mode, this->show_only_distant_con, shape_color, host_color, this->zoomed_vertex, this->zoomed_edge, traces);


#ifndef NOCG
	if (shader_enabled)
		glBindFramebufferEXT(GL_FRAMEBUFFER_EXT, 0);
#endif

	glViewport(0, 0, width(), height());

	//   int view[4];
	GLint view[4];
	glGetIntegerv(GL_VIEWPORT, view);
	this->view_boundingbox_tl = convertPoint(QPoint(0, 0));
	this->view_boundingbox_br = convertPoint(QPoint(view[2] - 1, view[3] - 1));

#ifndef NOCG
	if (shader_enabled) {
		glPushName(1);

		// Enable texture
		glEnable( GL_TEXTURE_2D);
		glBindTexture(GL_TEXTURE_2D, this->texture);
		// Enable shader
		cgGLEnableProfile(cgFragmentProfile);
		cgGLBindProgram(cgProgram);
		glColor3f(1.0f, 1.0f, 1.0f);

		glBegin( GL_QUADS);
		glTexCoord2f(0.0f, 1.0f);
		glVertex2f(this->view_boundingbox_tl.x, this->view_boundingbox_tl.y);
		glTexCoord2f(1.0f, 1.0f);
		glVertex2f(this->view_boundingbox_br.x, this->view_boundingbox_tl.y);
		glTexCoord2f(1.0f, 0.0f);
		glVertex2f(this->view_boundingbox_br.x, this->view_boundingbox_br.y);
		glTexCoord2f(0.0f, 0.0f);
		glVertex2f(this->view_boundingbox_tl.x, this->view_boundingbox_br.y);
		glEnd();

		// Disable shader
		cgGLUnbindProgram(cgFragmentProfile);
		cgGLDisableProfile(cgFragmentProfile);
		// Disable texture
		glBindTexture(GL_TEXTURE_2D, 0);
		glDisable(GL_TEXTURE_2D);

		glPopName();
	}
#endif
}

/**
 * Draw a cross at the center of the scene
 */
void Viewer::draw_cross() {
	GLint view[4];
	glGetIntegerv(GL_VIEWPORT, view);
	qglviewer::Vec view_bdbox_tl = convertPoint(QPoint(0, 0));
	qglviewer::Vec view_bdbox_br = convertPoint(
			QPoint(view[2] - 1, view[3] - 1));

	float posx = 0.5f * (view_bdbox_br.x + view_bdbox_tl.x);
	float posy = 0.5f * (view_bdbox_br.y + view_bdbox_tl.y);
	float width = 0.015f * min(view_bdbox_br.x - view_bdbox_tl.x,
			view_bdbox_br.y - view_bdbox_tl.y);

	glBlendFunc(GL_ONE_MINUS_DST_COLOR, GL_ZERO);

	glColor4f(0.0, 0.0, 0.0, 1.0);
	glLineWidth(1.0f);
	glBegin( GL_LINES);
		glVertex3f(posx - width, posy, 50);
		glVertex3f(posx + width, posy, 50);
		glVertex3f(posx, posy - width, 50);
		glVertex3f(posx, posy + width, 50);
	glEnd();

	glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
}

/**
 * Drawing in fast mode (while camera is moving)
 */
void Viewer::fastDraw() {
	draw2(true);
	draw_cross();
}

/**
 * Draw the FlowVR logo (before a graph be loaded)
 */
void Viewer::draw_logo() {

	if (this->logo_texture == NO_TEXTURE) {
		return;
	}

	GLint view[4];
	glGetIntegerv(GL_VIEWPORT, view);
	this->view_boundingbox_tl = convertPoint(QPoint(0, 0));
	this->view_boundingbox_br = convertPoint(QPoint(view[2] - 1, view[3] - 1));

	float width = 0.5f * min(
			this->view_boundingbox_br.x - this->view_boundingbox_tl.x,
			this->view_boundingbox_tl.y - this->view_boundingbox_br.y);
	float centre_x = 0.5f * (this->view_boundingbox_br.x
			+ this->view_boundingbox_tl.x);
	float centre_y = 0.5f * (this->view_boundingbox_br.y
			+ this->view_boundingbox_tl.y);

	// Enable texture
	glEnable( GL_TEXTURE_2D);
	glBindTexture(GL_TEXTURE_2D, this->logo_texture);

	glColor4f(1.0f, 1.0f, 1.0f, 0.5f);

	glBegin( GL_QUADS);
		glTexCoord2f(0.0f, 1.0f);
		glVertex2f(centre_x - width * 0.535f, centre_y + width * 0.25f);
		glTexCoord2f(1.0f, 1.0f);
		glVertex2f(centre_x + width * 0.535f, centre_y + width * 0.25f);
		glTexCoord2f(1.0f, 0.0f);
		glVertex2f(centre_x + width * 0.535f, centre_y - width * 0.25f);
		glTexCoord2f(0.0f, 0.0f);
		glVertex2f(centre_x - width * 0.535f, centre_y - width * 0.25f);
	glEnd();

	// Disable texture
	glBindTexture(GL_TEXTURE_2D, 0);
	glDisable(GL_TEXTURE_2D);
}

/**
 * Drawing in complete mode
 */
void Viewer::draw() {
	if (graph != NULL)
		draw2(false);
	else if (graphNet != NULL)
		draw2(false);
	else
		draw_logo();
}

/**
 *
 */
void Viewer::drawWithNames() {
	draw2(true);
}

/**
 * Refresh the mini-view
 */
void Viewer::postDraw() {
	//parent->Viewer2->refresh();
}

/**
 * Exporting the view
 */
void Viewer::export_view() {
	saveSnapshot(false);
}

/**
 * Reload current graph
 */
void Viewer::reload() {
	if ((graph == NULL && graphNet == NULL) || current_fileName == "")
		return;
	load_file_2(current_fileName);
}






/* #######
 * BUTTONS
 */

/**
 * 'Inputs' button toggled
 */
void Viewer::toggle_in(bool b) {
	if (b) {
		tb_out->setOn(false);
		tb_in_out->setOn(false);

		mode_exploration = MODE_EXPLO_IN;
		if (graphNet != NULL && cur_selection.size() > 0 && selection_mode) {
			update_selection();
		}
	} else {
		if (!tb_out->isOn() && !tb_in_out->isOn()) {
			tb_in->blockSignals(true);
			tb_in->setOn(true);
			tb_in->blockSignals(false);
		}
	}
}

/**
 * 'Outputs' button toggled
 */
void Viewer::toggle_out(bool b) {
	if (b) {
		tb_in->setOn(false);
		tb_in_out->setOn(false);

		mode_exploration = MODE_EXPLO_OUT;
		if (graphNet != NULL && cur_selection.size() > 0 && selection_mode) {
			update_selection();
		}
	} else {
		if (!tb_in->isOn() && !tb_in_out->isOn()) {
			tb_out->blockSignals(true);
			tb_out->setOn(true);
			tb_out->blockSignals(false);
		}
	}
}

/**
 * 'Inputs & Outputs' button toggled
 */
void Viewer::toggle_in_out(bool b) {
	if (b) {
		tb_in->setOn(false);
		tb_out->setOn(false);

		mode_exploration = MODE_EXPLO_IN_OUT;
		if (graphNet != NULL && cur_selection.size() > 0 && selection_mode) {
			update_selection();
		}
	} else {
		if (!tb_in->isOn() && !tb_out->isOn()) {
			tb_in_out->blockSignals(true);
			tb_in_out->setOn(true);
			tb_in_out->blockSignals(false);
		}
	}
}

/**
 * 'Max depth' button toggled
 */
void Viewer::toggle_maxdepth(bool b) {
	if (graphNet == NULL || !selection_mode) {
		tb_maxdepth->blockSignals(true);
		tb_maxdepth->setOn(!b);
		tb_maxdepth->blockSignals(false);
		return;
	}

	max_depth = b;
	if (max_depth) {
		update_selection();
		parent->statusbar->setText(QString("Depth: max"));
	}
}

/**
 * 'Plus' button pressed
 */
void Viewer::tb_plus_released() {
	if (!selection_mode)
		return;

	niveau_exploration++;
	if (graphNet != NULL && cur_selection.size() > 0) {
		tb_maxdepth->setOn(false);
		update_selection();
	}
}

/**
 * 'Minus' button pressed
 */
void Viewer::tb_minus_released() {
	if (!selection_mode)
		return;

	if (niveau_exploration <= 0)
		return;

	niveau_exploration--;
	if (graphNet != NULL && cur_selection.size() > 0) {
		tb_maxdepth->setOn(false);
		update_selection();
	}
}

/**
 * 'color:type' button toggled
 */
void Viewer::toggle_color1(bool b) {
	if (b) {
		tb_color2->setOn(false);
		tb_color3->setOn(false);
		color_mode = 0;
		updateGL();
	} else {
		if (!tb_color2->isOn() && !tb_color3->isOn()) {
			tb_color1->blockSignals(true);
			tb_color1->setOn(true);
			tb_color1->blockSignals(false);
		}
	}
}

/**
 * 'color:host' button toggled
 */
void Viewer::toggle_color2(bool b) {
	if (b) {
		tb_color1->setOn(false);
		tb_color3->setOn(false);
		color_mode = 1;
		updateGL();
	} else {
		if (!tb_color1->isOn() && !tb_color3->isOn()) {
			tb_color2->blockSignals(true);
			tb_color2->setOn(true);
			tb_color2->blockSignals(false);
		}
	}
}

/**
 * 'color:XML' button toggled
 */
void Viewer::toggle_color3(bool b) {
	if (b) {
		tb_color1->setOn(false);
		tb_color2->setOn(false);
		color_mode = 2;
		updateGL();
	} else {
		if (!tb_color1->isOn() && !tb_color2->isOn()) {
			tb_color3->blockSignals(true);
			tb_color3->setOn(true);
			tb_color3->blockSignals(false);
		}
	}
}








/**
 * Make the connections visible if their tail & head are visible
 */
void Viewer::connections_visible() {
	liste_edges* cur_edge = graphNet->get_edges();
	while (cur_edge != NULL) {
		cur_edge->edge->set_visible(
				((GVertexNet*) cur_edge->edge->vertex_head)->get_visible()
						&& ((GVertexNet*) cur_edge->edge->vertex_tail)->get_visible());
		cur_edge = cur_edge->suiv;
	}
}

/**
 * Close the children items
 */
void close_all_items(Q3ListViewItem* parent) {
	Q3ListViewItem* enfant = parent->firstChild();
	while (enfant) {
		if (enfant->text(1) == "")//NULL)
		{
			enfant->setOpen(false);
			close_all_items(enfant);
		}
		enfant = enfant->nextSibling();
	}
}



bool Viewer::regExpAdlBrowse(Q3ListViewItem* elem, QRegExp exp) {
	//
	// Is it the current element?
	//
	// Compare
	int pos = exp.search(elem->text(0));
	if (pos > -1) {
		// Match
		selectItem(elem, true, 0);
		// Open up the treeview
		Q3ListViewItem* parent = elem->parent();
		while (parent != NULL) {
			parent->setOpen(true);
			parent = parent->parent();
		}
		return true;
	}



	// {isn't the current element}
	// Grab the children
	Q3ListViewItem* subElem = elem->firstChild();
	while (subElem) {
		if (regExpAdlBrowse(subElem, exp)) {
			return true;
		} else {
			subElem = subElem->nextSibling();
		}
	}
	// {no more children}
	return false;
}

/**
 * Make visible the modules matching the regular expression.
 * It also expands the corresponding items in the lists.
 */
void Viewer::regExpButton_released() {
	if (this->graphNet == NULL && this->graph == NULL)
		return;

	if (graphIsAdl) {
		QRegExp exp(parent->regExpEdit->text());
		exp.setCaseSensitive(false);

		close_all_items(parent->listView1->firstChild());
		close_all_items(parent->listView2->firstChild());

		// Browse through all the nodes
		// match the regex against node id

		// Use the listview
		Q3ListViewItem* root = parent->listView1->firstChild();
		if (!regExpAdlBrowse(root, exp)) {
			displayInfo("No result. Try a different expression.");
		}

	} else {
		QRegExp exp(parent->regExpEdit->text());
		exp.setCaseSensitive(false);

		close_all_items(parent->listView1->firstChild());
		close_all_items(parent->listView2->firstChild());

		int pos;
		liste_vertices* cur_vertex = graphNet->get_vertices();
		while (cur_vertex != NULL) {
			pos = exp.search(QString((cur_vertex->vertex->get_id()->c_str())));
			if (pos > -1) {
				parent->listView1->ensureItemVisible(
						parent->listView1->findItem(
								QString((cur_vertex->vertex->get_id()->c_str())), 2));
				parent->listView2->ensureItemVisible(
						parent->listView2->findItem(
								QString((cur_vertex->vertex->get_id()->c_str())), 2));

			}

			cur_vertex->vertex->set_visible(pos > -1);
			cur_vertex = cur_vertex->suiv;
		}
		connections_visible();
		graphNet->draw(false, color_mode, this->show_only_distant_con, shape_color,
				host_color, this->zoomed_vertex, this->zoomed_edge);
		updateGL();
	}
}

/**
 *
 */
void Viewer::regExpReturn() {
	if (graphNet == NULL)
		return;

	if (selection_mode && cur_selection.size() > 0) {
		update_selection();
	} else {
		everything_visible(true);
		graphNet->draw(false, color_mode, this->show_only_distant_con,
				shape_color, host_color, this->zoomed_vertex, this->zoomed_edge);
		updateGL();
	}
}



/**
 * Event: key pressed
 */
void Viewer::keyPressEvent(QKeyEvent* e) {
	if (graph == NULL && graphNet == NULL)
		return;

	// dot export
	if (e->key() == Qt::Key_D) {
		QString fileName = Q3FileDialog::getSaveFileName("./",
				"Graphviz file (*.dot)", this);
		if (fileName == "")
			return;
		static GVC_t *gvc;
		gvc = gvContext();
		FILE* fout = fopen(fileName, "w");
		gvRender(gvc, this->gv_graph, (char*) "dot", fout);
		fclose(fout);
	}


	
	// Cluster mode
	if (e->key() == Qt::Key_C) {
		if (graphNet == NULL || current_fileName == "")
			return;
		load_file_2(current_fileName);
	}

	// Selection mode
	if (e->key() == Qt::Key_Space) {
		tb_select->setOn(tb_select->isOn() == false);
	}

	// Increasing visibility depth
	if (e->key() == Qt::Key_Plus) {
		tb_plus_released();
	}

	// Decreasing visibility depth
	if (e->key() == Qt::Key_Minus) {
		tb_minus_released();
	}

	// Setting visibility mode = IN
	if (e->key() == Qt::Key_I) {
		tb_in->setOn(true);
	}

	// Setting visibility mode = OUT
	if (e->key() == Qt::Key_O) {
		tb_out->setOn(true);
	}

	// Setting visibility mode == IN & OUT
	if (e->key() == Qt::Key_P) {
		tb_in_out->setOn(true);
	}

	// Going back to the previous view
	if (e->key() == Qt::Key_H) {
		camera()->setOrientation(qglviewer::Quaternion(0.0, 0.0, 0.0, 1.0));
		showEntireScene();
	}

	// Toggle shader
	if (e->key() == Qt::Key_S) {
#ifndef NOCG
		shader_enabled = (shader_enabled == false) && shader_supported;
		updateGL();
#else
		QMessageBox::information( this, "NOCG", "NOCG is defined\nNo shader support.");
#endif
	}

	// Export
	if (e->key() == Qt::Key_E) {
		saveSnapshot(false);
	}

	// XSL script selection
	if (e->key() == Qt::Key_X) {
		QString fileName = Q3FileDialog::getOpenFileName("./",
				"XSL script (*.xsl)", this);
		if (fileName == "")
			return;
		this->xsl_file = std::string(fileName.latin1());
		parent->statusbar->setText(
				QString("New XSL script loaded: ") + fileName);

	}
}






/**
 * Change the color of the element under the cursor
 */
void Viewer::toggle_colorchg(bool b) {
	if (b) {
		//      parent->setCursor(QCursor(Qt::PointingHandCursor));
		selection_mode = false;
		color_changing_mode = true;
		tb_select->blockSignals(true);
		tb_select->setOn(false);
		tb_select->blockSignals(false);
		parent->statusbar->setText(QString("Color modification mode"));
	} else {
		//    parent->setCursor(QCursor(Qt::ArrowCursor));
		color_changing_mode = false;
	}
}

/**
 * Open the color picker
 */
void Viewer::colorButton_released() {
	QColor color = QColorDialog::getColor(cur_color, this, "color dialog");
	if (!color.isValid())
		return;
	QPixmap new_col(20, 20);
	new_col.fill(color);
	tb_color->setIconSet(new_col);
	this->cur_color = color;
}

/**
 * Event: the active tab as changed
 * Update of the color mode
 */
void Viewer::tab_changed(QWidget* new_tab) {
	if (new_tab == parent->tab1) {
		tb_color1->setOn(true);
	} else if (new_tab == parent->tab2) {
		tb_color2->setOn(true);
	}
}






/**
 * change to Cluster view
 */
void Viewer::change_to_cluster_view() {
	if (!graphIsAdl) {
		netHierarchicalView = !netHierarchicalView;
		
		if (graphNet == NULL || current_fileName == "") {
			return;
		}
		else {
			load_file_2(current_fileName);
		}
	} else {
		QMessageBox::information(
						this,
						"Cluster view",
						"You are currently working on an .adl hierarchical graph.\n\nCluster view is an option available on .net.xml graphs to emulate hierarchy.");
	}
}


/**************************************************************
 * Trace handling
 */


#define SLIDER_MAX (1000*1000)

void Viewer::load_traces(const char *filename) {
  Traces *traces = new Traces();
  
  bool load_ok = traces->parse(filename, 1);
  
  if(load_ok) {
    delete this->traces; 
    this->traces = traces; 
    // traces->print();
    repaint();
    parent->Viewer2->refresh();    
    parent->TabPage2->setEnabled(true);
    // can't set times direcly, so just take a fine enough range...
    parent->horizontalSlider->setRange(0, SLIDER_MAX); 
    gotoTime(traces->t0);
  } else {
    QMessageBox::information(this, "Could not load gltrace file", 
                             "see the stderr for more info");
    delete traces; 
  }   

}

void Viewer::gotoTime(double t) {
  traces->t = t; 
    
  // set text and slider
  {
    double dt = t - traces->t0; 
    char buf[50];
    sprintf(buf, "%.6f", dt);
    parent->lineEdit->setText(buf);
    parent->horizontalSlider->setValue((int)(dt * SLIDER_MAX / (traces->t1 - traces->t0)));
  }

  repaint();
}

void Viewer::stopTimer() {
  if(traceTimer) {
    traceTimer->stop(); 
    delete traceTimer; 
    traceTimer = NULL;
  }
}

void Viewer::setTraceTime(int slider_pos) {  
  stopTimer(); 
  double t = traces->t0 + (traces->t1 - traces->t0) * slider_pos / SLIDER_MAX;
  //  printf("%d %.6f %.6f %.6f\n", slider_pos, traces->t0, t, traces->t1);
  gotoTime(t);
}

void Viewer::setTraceTimeFromText() {
  stopTimer(); 
  double dt;
  const char *str = parent->lineEdit->text().toAscii().constData();
  int parse_ok = sscanf(str, "%lf", &dt);  
  if(parse_ok) {
    double t = traces->t0 + dt; 
    gotoTime(t); 
  } else {
    printf("could not parse %s as a number\n", str);
  }
}

void Viewer::playTrace() {
  stopTimer(); 

  traceTimer = new QTimer(); 

  connect(traceTimer, SIGNAL(timeout()), this, SLOT(timerStep()));
  traceTimer->start(50); // 20 Hz
  eventT = -1;
}
 
void Viewer::timerStep() {
  if(eventT < 0) { // normal timer
    if(traces->t + 0.05 > traces->t1) 
      stopTimer(); 
    else 
      gotoTime(traces->t + 0.05); 
  } else {
    eventT += 0.05;
    double t_next = traces->fromEventTime(eventT); 
    if(t_next < 0) stopTimer(); 
    else gotoTime(t_next);
  }
}

void Viewer::playTraceEvent() {
  stopTimer(); 

  traceTimer = new QTimer(); 
  connect(traceTimer, SIGNAL(timeout()), this, SLOT(timerStep()));
  traceTimer->start(50); // 20 Hz
  eventT = traces->toEventTime(traces->t); 
}



/* ############
 * HELP SECTION
 */

/**
 * Load help dialog
 */
void Viewer::tb_help_released() {
	help();
}

/**
 * Help message
 */
QString Viewer::helpString() const {
	QString text("<h2>Flowvr-glGraph</h2>");
	text += "<br>Graph visualizer.<br>Supports .adl.out.xml and .net.xml format.<br><br>------------------<br> FlowVR-Graph<br>GPL Licence<br>COPYRIGHT (C) 2006 by Laboratoire ID and INRIA<br>Author: Antoine Meler (antoine.meler@ensimag.imag.fr).<br><br>------------------<br>FlowVR-glGraph : Hierarchical version<br>Author: Xavier Martin (xavier.martin@imag.fr) 2011.";
	return text;
}

/**
 * Mouse help
 */
QString Viewer::mouseString() const {
	QString text("");
	text += "<table border=1>";
	text += "   <tr>";
	text += "      <td>";
	text += "         Right click";
	text += "      </td>";
	text += "      <td>";
	text += "         <b>Normal mode:</b> Element magnification.<br>";
	text += "         <b>Selection mode:</b> Element selection.<br>";
	text
			+= "         <b>Color modification mode:</b> Element's color modification.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         Left click";
	text += "      </td>";
	text += "      <td>";
	text += "         Move";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         Wheel";
	text += "      </td>";
	text += "      <td>";
	text += "         Zoom in/out";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         Middle";
	text += "      </td>";
	text += "      <td>";
	text += "         Zoom in/out";
	text += "      </td>";
	text += "   </tr>";

	text + "</table>";
	return text;
}

/**
 * Keyboard help
 */
QString Viewer::keyboardString() const {
	QString text("");
	text += "<table border=1>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [Space]+[left click]";
	text += "      </td>";
	text += "      <td>";
	text += "         Select the clicked node or connection.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [+]";
	text += "      </td>";
	text += "      <td>";
	text += "         Increase the visibility depth from the selected node.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [-]";
	text += "      </td>";
	text += "      <td>";
	text += "         Decrease the visibility depth from the selected node.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [I]";
	text += "      </td>";
	text += "      <td>";
	text += "         Visualization of the <b>i</b>nputs only.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [O]";
	text += "      </td>";
	text += "      <td>";
	text += "         Visualization of the <b>o</b>utputs only.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [P]";
	text += "      </td>";
	text += "      <td>";
	text += "         Visualization of both inputs and outputs.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [H]";
	text += "      </td>";
	text += "      <td>";
	text += "         Center the view.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [S]";
	text += "      </td>";
	text += "      <td>";
	text += "         Toggle <b>s</b>hader.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [C]";
	text += "      </td>";
	text += "      <td>";
	text += "         Toggle cluster layout (experimental).";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [E]";
	text += "      </td>";
	text += "      <td>";
	text += "         <b>E</b>xport snapshot.";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [X]";
	text += "      </td>";
	text += "      <td>";
	text += "         Change <b>X</b>SL script (conversion .net.xml to .dot).";
	text += "      </td>";
	text += "   </tr>";
	text += "   <tr>";
	text += "      <td>";
	text += "         [D]";
	text += "      </td>";
	text += "      <td>";
	text += "         Export graph in Graphviz format.";
	text += "      </td>";
	text += "   </tr>";
	text += "</table>";
	return text;
}

/**
 * About
 */
void Viewer::aboutQGLViewer() {
	QMessageBox::information(
			this,
			"About",
			"FlowVR-Graph\nGPL Licence\nCOPYRIGHT (C) 2006 by Laboratoire ID and INRIA\nAuthor: Antoine M�ler (antoine.meler@ensimag.imag.fr).\n\nFlowvVR-glGraph : Hierarchical version (2011)\nAuthor: Xavier Martin (xavier.martin@imag.fr).");
}









void Viewer::displayInfo(const void* info) {
	QMessageBox::information(this, "FlowVR glGraph", (char*)info);
}

void Viewer::displayInfo(string info) {
	QMessageBox::information(this, "FlowVR glGraph", info.c_str());
}

