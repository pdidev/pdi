/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *	
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./gldens.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include <stdlib.h>
#include <stdio.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <sys/time.h>
#include <math.h>

// Grid dimensions
int NX;
int NY;

#define DY (NX)
#define IX(i,j) ((i)+(DY*(j)))

class cell
{
public:
  unsigned char u,v;
};

class fcell
{
public:
  float u,v;
};


/* flowvr variables */
#include <flowvr/module.h>
#include <ftl/chunkevents.h>
using namespace flowvr;

/// Input Port for 2D Grids with stamp "N" for size
class GridInputPort : public flowvr::InputPort
{
public:
	GridInputPort(const char* name="grid")
	: InputPort(name),
	stampN("N",flowvr::TypeArray::create(2,flowvr::TypeInt::create()))
	{
		stamps->add(&stampN); // add the stamp N to the stamps list for this port
	}
	flowvr::StampInfo stampN; ///< array of 2 integers for the dimension of the grid
};

ModuleAPI* module = NULL; ///< FlowVR module

/// Input velocity port
GridInputPort portDensity("density"); 
/// Input density port (active only if inputVelocity==true)
GridInputPort portVelocity("velocity");

/// Output position port (active only if outputPosition==true)
OutputPort portPosition("positions");
bool inputVelocity = false;

/// Helper class to construct mouse update chunks
ftl::ChunkEventWriter *mouseMsgs;

// Buffers containing the last received message data on each port
Buffer velocity;
Buffer density;

// Output messages buffer pool
BufferPool positionPool;
// Mouse position messages buffer pool
BufferPool mouseposPool;


/* global variables */

static int dvel;

static fcell * pos = NULL;
static int * quads_index = NULL;
static int quads_nindex;

static int win_id;
static int win_x, win_y;

static unsigned char mouse_buttons;

static int mx, my;


/*
  ----------------------------------------------------------------------
   OpenGL specific drawing routines
  ----------------------------------------------------------------------
*/

// all dimension-dependents inits
static void init_dim ( void )
{
	if (pos != NULL) free(pos);
	pos 	= (fcell *) malloc ( NX*NY*sizeof(fcell) );

	int i,j;
	float hx = 1.0f/(NX-1);
	float hy = 1.0f/(NY-1);

	for ( j=0 ; j<NY ; j++ ) {
		for ( i=0 ; i<NX ; i++ ) {
			pos[IX(i,j)].u = (i-0.5f)*hx;
			pos[IX(i,j)].v = (j-0.5f)*hy;
		}
	}

	if (quads_index != NULL) free(quads_index);
	quads_nindex = (NY-1)*((NX-1)*4);
	quads_index = (int*) malloc(quads_nindex*sizeof(int));
	int ind = 0;

	for ( j=0 ; j<NY-1 ; j++ ) {
		for ( i=0 ; i<NX-1 ; i++ ) {
			//printf("%d %d %d %d\n",
			(quads_index[ind++]=IX(i,j),
			quads_index[ind++]=IX(i+1,j),
			quads_index[ind++]=IX(i+1,j+1),
			quads_index[ind++]=IX(i,j+1));
		}
	}
	quads_nindex=ind;
}

static void init_display ( void )
{
	glViewport ( 0, 0, win_x, win_y );
	glMatrixMode ( GL_PROJECTION );
	glLoadIdentity ();
	gluOrtho2D ( 0.0, 1.0, 0.0, 1.0 );
	glClearColor ( 0.0f, 0.0f, 0.0f, 1.0f );
}

static void pre_display ( void )
{
	glClear ( GL_COLOR_BUFFER_BIT );
}

static void post_display ( void )
{
	glutSwapBuffers ();
}

static void draw_velocity ( void )
{
	int i, j;
	float x, y, hx, hy, dx, dy;

	if (pos==NULL || NX==0 || NY==0) return;

	if (velocity.getSize()!=NX*NY*(int)sizeof(cell))
	{
		std::cout << "Message size not valid: "<<velocity.getSize()<<" <-> "<<NX*NY*sizeof(cell)<<"="<<NX<<"*"<<NY<<"*"<<sizeof(cell)<<std::endl;
		return;
	}

	const cell* vel = velocity.getRead<cell>(0);

	hx = 1.0f/(NX-1);
	hy = 1.0f/(NY-1);
	dx = 4.0f/(256*(NX-1));
	dy = 4.0f/(256*(NY-1));

	glColor3f ( 1.0f, 1.0f, 1.0f );
	glLineWidth ( 1.0f );

	glBegin ( GL_LINES );

	for ( j=0 ; j<NY ; j++ ) {
		y = (j-0.5f)*hy;
		for ( i=0 ; i<NX ; i++ ) {
			x = (i-0.5f)*hx;

			glVertex2f ( x, y );
			glVertex2f ( x+vel[IX(i,j)].u*dx, y+vel[IX(i,j)].v*dy );
		}
	}

	glEnd ();
}

static void draw_density ( void )
{
	int i, j, p;

	if (pos==NULL || NX==0 || NY==0) return;

	if (density.getSize()==NX*NY*(int)sizeof(cell))
	{
		const cell* dens = density.getRead<cell>(0);
		glBegin ( GL_QUADS );
		for ( j=0, p=0 ; j<NY-1 ; j++, p++ ) {
			for ( i=0 ; i<NX-1 ; i++, p++ ) {
				glColor3ub(128,dens[p].u,dens[p].v); glVertex2f( pos[p].u, pos[p].v);
				glColor3ub(0,dens[p+1].u,dens[p+1].v); glVertex2f( pos[p+1].u, pos[p+1].v);
				glColor3ub(0,dens[p+1+DY].u,dens[p+1+DY].v); glVertex2f( pos[p+1+DY].u, pos[p+1+DY].v);
				glColor3ub(0,dens[p+DY].u,dens[p+DY].v); glVertex2f( pos[p+DY].u, pos[p+DY].v);
			}
		}
		glEnd ();
	}
	else if (density.getSize()==NX*NY*(int)sizeof(fcell))
	{
		const fcell* dens = density.getRead<fcell>(0);
		glBegin ( GL_QUADS );
		for ( j=0, p=0 ; j<NY-1 ; j++, p++ ) {
			for ( i=0 ; i<NX-1 ; i++, p++ ) {
				glColor3f(0,dens[p].u,dens[p].v); glVertex2f( pos[p].u, pos[p].v);
				glColor3f(0,dens[p+1].u,dens[p+1].v); glVertex2f( pos[p+1].u, pos[p+1].v);
				glColor3f(0,dens[p+1+DY].u,dens[p+1+DY].v); glVertex2f( pos[p+1+DY].u, pos[p+1+DY].v);
				glColor3f(0,dens[p+DY].u,dens[p+DY].v); glVertex2f( pos[p+DY].u, pos[p+DY].v);
			}
		}
		glEnd ();
	}
	else
	{
		std::cout << "Message size not valid: "<<density.getSize()<<" <-> "<<NX*NY*sizeof(cell)<<"="<<NX<<"*"<<NY<<"*"<<sizeof(cell)<<std::endl;
	}
}

/*
  ----------------------------------------------------------------------
   GLUT callback routines
  ----------------------------------------------------------------------
*/

static void key_func ( unsigned char key, int x, int y )
{
	switch ( key )
	{
		// case 'q':
		// case 'Q':
		//   //free_data ();
		//   exit ( 0 );  
		//   break;

		case 'v':
		case 'V':
		dvel = !dvel;
		break;
	}
}

static void mouse_func ( int button, int state, int x, int y )
{
	mx = x;
	my = y;

	mouse_buttons &= ~(1<<button);
	mouse_buttons |= ((state == GLUT_DOWN)<<button);

}

static void motion_func ( int x, int y )
{
	mx = x;
	my = y;
}

static void reshape_func ( int width, int height )
{
	glutSetWindow ( win_id );
	glutReshapeWindow ( width, height );

	win_x = width;
	win_y = height;
	glViewport ( 0, 0, win_x, win_y );
}

struct timeval t0;
int f0=0;
int nbf=0;

static void idle_func ( void )
{
	// MAIN LOOP (GLUT callback)

	// NEXT ITERATION
	if (!module->wait()) {
		module->close();
		exit(1);
	}

	// DISPLAY STATS
	if (!(nbf%100))
	{
		struct timeval tnow;
		gettimeofday(&tnow,NULL);
		if (nbf>f0)
		{
			double time = (tnow.tv_sec-t0.tv_sec)+(tnow.tv_usec-t0.tv_usec)*0.000001;
			printf("FPS: %6.3f\n",(nbf-f0)/time);
		}
		t0 = tnow;
		f0=nbf;
	}

	// OUTPUT POSITION
	{
		flowvr::MessageWrite msg;
		msg.data = mouseposPool.alloc( module->getAllocator(), sizeof( float[3] ) );
		float * const mouse = msg.data.getWrite<float>();
		// hold the transformed mouse position
		mouse[0] = ((NX==0)?1.0f:(float)(NX-1)/NX) * ((float)(mx))/win_x;
		mouse[1] = ((NY==0)?1.0f:(float)(NY-1)/NY) * ((float)(win_y-1-my))/win_y;
		// button status
		mouse[2] = (float) mouse_buttons; // uchar fits in float
		// put message
		module->put( &portPosition, msg );
	}

	// READ DATA
	Message m;

	module->get(&portDensity,m);
	density = m.data; // store the message's data
	int nx,ny;
	if (m.stamps.read(portDensity.stampN[0],nx) && m.stamps.read(portDensity.stampN[1],ny))
	{ // we successfully read the stamp N: store the new dimensions
		if (nx!=NX || ny!=NY)
		{
			NX=nx;
			NY=ny;
			init_dim(); 
		}
	}

	//if portVelocity is activated then read the message and store the data
	if (inputVelocity)
	{
		module->get(&portVelocity,m);
		velocity = m.data;
	}

	++nbf;

	// ACTIVATE DISPLAY
	glutSetWindow ( win_id );
	glutPostRedisplay ();
}

static void display_func ( void )
{
	pre_display ();

	if ( dvel ) draw_velocity ();
	else		draw_density ();

	post_display ();
}


/*
  ----------------------------------------------------------------------
   open_glut_window --- open a glut compatible window and set callbacks
  ----------------------------------------------------------------------
*/

static void open_glut_window ( void )
{
	glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE );

	glutInitWindowPosition ( 0, 0 );
	glutInitWindowSize ( win_x, win_y );
	win_id = glutCreateWindow ( "flowvr-fluid" );

	glClearColor ( 0.0f, 0.0f, 0.0f, 1.0f );
	glClear ( GL_COLOR_BUFFER_BIT );
	glutSwapBuffers ();
	glClear ( GL_COLOR_BUFFER_BIT );
	glutSwapBuffers ();

	init_display();

	pre_display ();

	glutKeyboardFunc ( key_func );
	glutMouseFunc ( mouse_func );
	glutMotionFunc ( motion_func );
	glutPassiveMotionFunc ( motion_func );
	glutReshapeFunc ( reshape_func );
	glutIdleFunc ( idle_func );
	glutDisplayFunc ( display_func );
}


/*
  ----------------------------------------------------------------------
   main --- main routine
  ----------------------------------------------------------------------
*/

void close_func( void ) {
	if ( module  &&  module->getStatus() ) {
		module->abort();
	}
}

int main ( int argc, char ** argv )
{

	// GLUT INIT + COMMAND LINE PARSING

	glutInit ( &argc, argv );

	if ( argc != 1 && argc!=4 ) {
		fprintf ( stderr, "usage : %s NX NY velocity\n", argv[0] );
		fprintf ( stderr, "where:\n" );
		fprintf ( stderr, "\t NX NY  : grid resolution\n" );
		fprintf ( stderr, "\t velocity : non zero to add velocity input\n" );
		exit ( 1 );
	}

	if ( argc == 1 ) {
		NX = 256;
		NY = 256;
		fprintf ( stderr, "Using defaults : NX=%d NY=%d\n",
		      NX, NY);
	} else {
		NX = atoi(argv[1]);
		NY = atoi(argv[2]);
		inputVelocity = atoi(argv[3]);
	}
	// OUTPUT USAGE INFO

	if (inputVelocity)
		printf ( "\n\n\t Press the 'v' key in visualization window to toggle density/velocity display\n\n" );
	//  printf ( "\t Quit by pressing the 'q' key\n" );
	dvel = 0;

	// FLOWVR INIT

	// create the port list by adding the different input and output ports
	std::vector<Port*> lports;
	lports.push_back(&portPosition);
	lports.push_back(&portDensity);
	if (inputVelocity) lports.push_back(&portVelocity);

	if ((module = initModule(lports)) == NULL) return 1; // if init returns zero an error occured

	// Variable used to write the chunks
	mouseMsgs = new ftl::ChunkEventWriter();

	// APP INIT

	init_dim();

	win_x = 512;
	win_y = 512*(NX==0?1:NY/NX);
	open_glut_window ();

	// GLUT MAIN LOOP

	atexit( close_func );
	glutMainLoop ();

	// APP CLOSE

	module->close(); // module ending

	return 0;
}
