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
 * File: ./src/main.cpp                                            *
 *                                                                 *
 * Contacts: Antoine M�ler <antoine.meler@ensimag.imag.fr>         *
 *                                                                 *
 ******************************************************************/
#include "icons.h"
#include "interface.h"
#include <ui_myInterface.h>
#include "MiniViewer.h"
#include "traces.h"

#include "Application.h"

#include <QtCore>
#include <QtGui>
#include <QtGui/qtoolbar.h>
#include <qapplication.h>

#include <q3mainwindow.h>
#include <QMainWindow>
#include <qtoolbutton.h>
#include <qpixmap.h>
#include <qimage.h>

# ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif


using namespace std;

int main(int argc, char** argv) {


	// Looking for data directory
	std::string datadir;
	if (access("./share/flowvr/glgraph/xsl", 0) == 0) {
		datadir = "./share/flowvr/glgraph";
	} else if (access("../share/flowvr/glgraph/xsl", 0) == 0) {
		datadir = "../share/flowvr/glgraph";
	} else {
		if (getenv("FLOWVR_PREFIX") != NULL) {
			datadir = std::string(getenv("FLOWVR_PREFIX")) + std::string(
					"/share/flowvr/glgraph");
			if (access(datadir.c_str(), 0) != 0) {
				cout << "Directory ('" << datadir
						<< "') not found ! Trying './share' & '../share' ... not found !"
						<< endl;
				cout
						<< "You cannot run flowvr-graph from this directory. Check FLOWVR_PREFIX or start from compilation directory"
						<< endl;
				return 1;
			}
		} else {
			cout
					<< "FLOWVR_PREFIX is undefined. Trying './share' & '../share'... not found !"
					<< endl;
			cout << "You cannot run flowvr-graph from this directory." << endl;
			return 1;
		}
	};

	// Read command lines arguments.
	Application application(argc, argv);
	



	Q3MainWindow *mw = new Q3MainWindow;
	mw->setCaption("FlowVR Graph Visualizer");
	mw->setObjectName(QString::fromUtf8("myMainWindow"));
	Ui_myInterface *main_widget = new Ui_myInterface((Ui_myInterface&) mw);
	main_widget->setupUi(mw);
	main_widget->Viewer1->datadir = datadir;

	application.mainwidget = main_widget;
	
	QToolBar * toolbar = new QToolBar(main_widget->Viewer1, "Toolbar");
	toolbar->setLabel("Toolbar");

	QImage img;

	// Creation of the toolbar ==>

	// Open
	img.loadFromData(image0_data, sizeof(image0_data), "PNG");
	QPixmap image0 = (QPixmap) img;
	QToolButton * tb_open = new QToolButton(image0, "Open File", QString::null,
			main_widget->Viewer1, SLOT(load_file()), toolbar, "");
	tb_open = tb_open;
	tb_open->resize(30, 30);
	tb_open->setAutoFillBackground(true);

	// Reload
	img.loadFromData(imagereload_data, sizeof(imagereload_data), "PNG");
	QPixmap pix_reload = (QPixmap) img;
	QToolButton * tb_reload = new QToolButton(pix_reload, "Reload",
			QString::null, main_widget->Viewer1, SLOT(reload()), toolbar, "");
	tb_reload->resize(30, 30);
	tb_reload->move(30, 0);
	tb_reload->setAutoFillBackground(true);

	// Export
	img.loadFromData(image1_data, sizeof(image1_data), "PNG");
	QPixmap image1 = (QPixmap) img;
	QToolButton * tb_export = new QToolButton(image1, "Export [E]",
			QString::null, main_widget->Viewer1, SLOT(export_view()), toolbar,
			"");
	tb_export->resize(30, 30);
	tb_export->move(60, 0);
	tb_export->setAutoFillBackground(true);

	// Distant communications
	img.loadFromData(image2_data, sizeof(image2_data), "PNG");
	QPixmap image2 = (QPixmap) img;
	QToolButton * tb_distant = new QToolButton(image2,
			"Show Only Distant Communications", QString::null, NULL, NULL,
			toolbar, "");
	tb_distant->setToggleButton(TRUE);
	QObject::connect( tb_distant, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( checkBox1_toggled(bool) ) );
	tb_distant->resize(30, 30);
	tb_distant->move(90, 0);
	tb_distant->setAutoFillBackground(true);

	// Select
	img.loadFromData(image15_data, sizeof(image15_data), "PNG");
	QPixmap image15 = (QPixmap) img;
	QToolButton * tb_select = new QToolButton(image15, "Select [Space]",
			QString::null, NULL, NULL, toolbar, "");
	tb_select->setToggleButton(TRUE);
	QObject::connect( tb_select, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_select(bool) ) );
	tb_select->resize(30, 30);
	tb_select->move(120, 0);
	tb_select->setAutoFillBackground(true);

	// Minus
	img.loadFromData(image4_data, sizeof(image4_data), "PNG");
	QPixmap image4 = (QPixmap) img;
	QToolButton * tb_minus = new QToolButton(image4, "Depth: minus [-]",
			QString::null, main_widget->Viewer1, SLOT(tb_minus_released()),
			toolbar, "");
	tb_minus->resize(30, 30);
	tb_minus->move(150, 0);
	tb_minus->setAutoFillBackground(true);

	// Plus
	img.loadFromData(image3_data, sizeof(image3_data), "PNG");
	QPixmap image3 = (QPixmap) img;
	QToolButton * tb_plus = new QToolButton(image3, "Depth: plus [+]",
			QString::null, main_widget->Viewer1, SLOT(tb_plus_released()),
			toolbar, "");
	tb_plus->resize(30, 30);
	tb_plus->move(180, 0);
	tb_plus->setAutoFillBackground(true);

	// Show All
	img.loadFromData(imageall_data, sizeof(imageall_data), "PNG");
	QPixmap imageshowall = (QPixmap) img;
	QToolButton * tb_maxdepth = new QToolButton(imageshowall, "Maximum depth",
			QString::null, NULL, NULL, toolbar, "");
	tb_maxdepth->setToggleButton(TRUE);
	QObject::connect( tb_maxdepth, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_maxdepth(bool) ) );
	tb_maxdepth->resize(30, 30);
	tb_maxdepth->move(210, 0);
	tb_maxdepth->setAutoFillBackground(true);

	// In
	img.loadFromData(image12_data, sizeof(image12_data), "PNG");
	QPixmap image12 = (QPixmap) img;
	QToolButton * tb_in = new QToolButton(image12, "Inputs [I]", QString::null,
			NULL, NULL, toolbar, "");
	tb_in->setToggleButton(TRUE);
	QObject::connect( tb_in, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_in(bool) ) );
	tb_in->resize(30, 30);
	tb_in->move(240, 0);
	tb_in->setAutoFillBackground(true);

	//Out
	img.loadFromData(image14_data, sizeof(image14_data), "PNG");
	QPixmap image14 = (QPixmap) img;
	QToolButton * tb_out = new QToolButton(image14, "Outputs [O]",
			QString::null, NULL, NULL, toolbar, "");
	tb_out->setToggleButton(TRUE);
	QObject::connect( tb_out, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_out(bool) ) );
	tb_out->resize(30, 30);
	tb_out->move(270, 0);
	tb_out->setAutoFillBackground(true);

	// In & Out
	img.loadFromData(image13_data, sizeof(image13_data), "PNG");
	QPixmap image13 = (QPixmap) img;
	QToolButton * tb_in_out = new QToolButton(image13, "Inputs & Outputs [P]",
			QString::null, NULL, NULL, toolbar, "");
	tb_in_out->setToggleButton(TRUE);
	QObject::connect( tb_in_out, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_in_out(bool) ) );
	tb_in_out->resize(30, 30);
	tb_in_out->move(300, 0);
	tb_in_out->setAutoFillBackground(true);

	tb_in_out->blockSignals(true);
	tb_in_out->setOn(true);
	tb_in_out->blockSignals(false);

	// Color mode: type
	img.loadFromData(image9_data, sizeof(image9_data), "PNG");
	QPixmap image9 = (QPixmap) img;
	QToolButton * tb_color1 = new QToolButton(image9, "Color: type",
			QString::null, NULL, NULL, toolbar, "");
	tb_color1->setToggleButton(TRUE);
	QObject::connect( tb_color1, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_color1(bool) ) );
	tb_color1->resize(30, 30);
	tb_color1->move(330, 0);
	tb_color1->setAutoFillBackground(true);

	tb_color1->blockSignals(true);
	tb_color1->setOn(true);
	tb_color1->blockSignals(false);

	// Color mode: host
	img.loadFromData(image10_data, sizeof(image10_data), "PNG");
	QPixmap image10 = (QPixmap) img;
	QToolButton * tb_color2 = new QToolButton(image10, "Color: host",
			QString::null, NULL, NULL, toolbar, "");
	tb_color2->setToggleButton(true);
	QObject::connect( tb_color2, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_color2(bool) ) );
	tb_color2->resize(30, 30);
	tb_color2->move(360, 0);
	tb_color2->setAutoFillBackground(true);

	// Color mode: .net.xml
	img.loadFromData(image11_data, sizeof(image11_data), "PNG");
	QPixmap image11 = (QPixmap) img;
	QToolButton * tb_color3 = new QToolButton(image11, "Color: .net.xml",
			QString::null, NULL, NULL, toolbar, "");
	tb_color3->setToggleButton(TRUE);
	QObject::connect( tb_color3, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_color3(bool) ) );
	tb_color3->resize(30, 30);
	tb_color3->move(390, 0);
	tb_color3->setAutoFillBackground(true);

	// color changing mode
	img.loadFromData(imgpot_data, sizeof(imgpot_data), "PNG");
	QPixmap pix_colorchg = (QPixmap) img;
	QToolButton * tb_colorchg = new QToolButton(pix_colorchg, "Change color",
			QString::null, NULL, NULL, toolbar, "");
	tb_colorchg->setToggleButton(true);
	QObject::connect( tb_colorchg, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( toggle_colorchg(bool) ) );
	tb_colorchg->resize(30, 30);
	tb_colorchg->move(420, 0);
	tb_colorchg->setAutoFillBackground(true);

	// Color
	QPixmap pix_color(20, 20);
	pix_color.fill(QColor(255, 0, 0));
	QToolButton* tb_color = new QToolButton(pix_color, "Color", QString::null,
			main_widget->Viewer1, SLOT(colorButton_released()), toolbar, "");
	tb_color->resize(30, 30);
	tb_color->move(450, 0);
	tb_color->setAutoFillBackground(true);

	// Cluster mode
	img.loadFromData(imageClusterView_data, sizeof(imageClusterView_data),
			"PNG");
	QPixmap cluster_view_img = (QPixmap) img;
	QToolButton * cluster_view = new QToolButton(cluster_view_img,
			"cluster view", QString::null, NULL, NULL, toolbar, "");
	cluster_view->setToggleButton(true);
	QObject::connect( cluster_view, SIGNAL( toggled(bool) ), main_widget->Viewer1, SLOT( change_to_cluster_view() ) );
	cluster_view->resize(30, 30);
	cluster_view->move(480, 0);
	cluster_view->setAutoFillBackground(true);

	// Help
	img.loadFromData(image8_data, sizeof(image8_data), "PNG");
	QPixmap image8 = (QPixmap) img;
	QToolButton * tb_help = new QToolButton(image8, "Help", QString::null,
			main_widget->Viewer1, SLOT(tb_help_released()), toolbar, "");
	tb_help->resize(30, 30);
	tb_help->move(510, 0);
	tb_help->setAutoFillBackground(true);


	toolbar->resize(540, 30);

	main_widget->Viewer1->tb_out = tb_out;
	main_widget->Viewer1->tb_in = tb_in;
	main_widget->Viewer1->tb_in_out = tb_in_out;
	main_widget->Viewer1->parent = main_widget;
	main_widget->Viewer1->tb_color1 = tb_color1;
	main_widget->Viewer1->tb_color2 = tb_color2;
	main_widget->Viewer1->tb_color3 = tb_color3;
	main_widget->Viewer1->tb_select = tb_select;
	main_widget->Viewer1->tb_color = tb_color;
	main_widget->Viewer1->tb_colorchg = tb_colorchg;
	main_widget->Viewer1->tb_maxdepth = tb_maxdepth;
	main_widget->Viewer1->cluster_view = cluster_view;
	main_widget->Viewer1->main_window = mw;

	main_widget->Viewer2->parent = main_widget;
	glutInit(&argc, argv);

	// Set the viewer as the application main widget.
	application.setMainWidget(mw);
	
	if (argc > 1) {
		application.startGraphFile = QString(argv[1]);
	}
	if (argc > 2) {
		application.startTraceFile = argv[2];         
	}
	
	// Show: this call should not appear sooner in the code or it may lead to errors (threads reading uinitialized data)
	mw->show();

	// Run main loop.
	return application.exec();
}
