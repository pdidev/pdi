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
*  15/10/2018 Reworked for PDI:       Karol Sierocinski           *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./gldens.cxx                                              *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*  15/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>            *
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

#include <iostream>
#include <pdi.h>

/* global variables */

static int dvel;

static fcell * pos = NULL;
static int * quads_index = NULL;
static int quads_nindex;

static int win_id;
static int win_x, win_y;

static bool mouse_left;
static bool mouse_middle;
static bool mouse_right;

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

	void* velocity_pdi; //need to access velocity before the size of it
	PDI_access("velocity", &velocity_pdi, PDI_IN);

	int velocitySize;
	{
		int* velocitySize_pdi;
		PDI_access("velocitySize", (void**)&velocitySize_pdi, PDI_IN);
		velocitySize = *velocitySize_pdi;
		PDI_release("velocitySize");
	}

	if (velocitySize != NX*NY*(int)sizeof(cell))
	{
		std::cout << "Message size not valid: "<<velocitySize<<" <-> "<<NX*NY*sizeof(cell)<<"="<<NX<<"*"<<NY<<"*"<<sizeof(cell)<<std::endl;
		return;
	}


	{
		cell* vel = (cell*) velocity_pdi;

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
	}

	glEnd ();
	PDI_release("velocity");
}

static void draw_density ( void )
{
	int i, j, p;

	if (pos==NULL || NX==0 || NY==0) return;

	void* density_pdi;  //need to access densify before the size of it
	PDI_access("density", &density_pdi, PDI_IN);

	int densitySize;
	{
		int* densitySize_pdi;
		PDI_access("densitySize", (void**)&densitySize_pdi, PDI_IN);
		densitySize = *densitySize_pdi;
		PDI_release("densitySize");
	}

	if (densitySize == NX*NY*(int)sizeof(cell))
	{
		cell* dens = (cell*) density_pdi;

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
	else if (densitySize == NX*NY*(int)sizeof(fcell))
	{
		fcell* dens = (fcell*) density_pdi;
		
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
		std::cout << "Message size not valid: "<< densitySize <<" <-> "<<NX*NY*sizeof(cell)<<"="<<NX<<"*"<<NY<<"*"<<sizeof(cell)<<std::endl;
	}
	PDI_release("density");
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

	int mouse_left = 0, mouse_middle = 0, mouse_right= 0;
	if (state == GLUT_DOWN) {
		if (button == GLUT_LEFT_BUTTON) {
			mouse_left = 1;
		}
		if (button == GLUT_MIDDLE_BUTTON) {
			mouse_middle = 1;
		}
		if (button == GLUT_RIGHT_BUTTON) {
			mouse_right = 1;
		}
	}
	PDI_expose("left_button", &mouse_left, PDI_OUT);
	PDI_expose("middle_button", &mouse_middle, PDI_OUT);
	PDI_expose("right_button", &mouse_right, PDI_OUT);
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
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	if (!wait) {
		PDI_finalize();
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
		float mouse[2];
		mouse[0] = ((NX==0)?1.0f:(float)(NX-1)/NX) * ((float)(mx))/win_x;
		mouse[1] = ((NY==0)?1.0f:(float)(NY-1)/NY) * ((float)(win_y-1-my))/win_y;
		PDI_expose("pos_xy", mouse, PDI_OUT);
	}

	// READ STAMPS
	{
		int den_stampN_xy[2];
		PDI_expose("den_N_xy", den_stampN_xy, PDI_IN);
		if (den_stampN_xy[0] != NX || den_stampN_xy[1] != NY) {
			NX= den_stampN_xy[0];
			NY= den_stampN_xy[1];
			init_dim();
		}
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
	int status;
	PDI_expose("status", &status, PDI_IN);
	if (status) {
		PDI_event("abort");
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
	}
	// OUTPUT USAGE INFO

	printf ( "\n\n\t Press the 'v' key in visualization window to toggle density/velocity display\n\n" );
	dvel = 0;

	// FLOWVR INIT

	PC_tree_t config = PC_parse_path("config/gldens.yml");
	PDI_init(config);

	// APP INIT

	init_dim();

	win_x = 512;
	win_y = 512*(NX==0?1:NY/NX);
	open_glut_window ();

	// GLUT MAIN LOOP

	atexit( close_func );
	glutMainLoop ();

	// APP CLOSE

	PDI_finalize();

	return 0;
}
