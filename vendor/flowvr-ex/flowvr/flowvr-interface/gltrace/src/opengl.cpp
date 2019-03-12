/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/gltrace/opengl.cpp                                    *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#include "gltrace.h"
#include <math.h>

static int win_id;
static int win_x, win_y;
static int mouse_down;
static int mx, my;
static int mx0, my0;
static double afftime;
static double timearea;
static int time_speed = 0;
static int time_div = -2;
static double lasttime;
static double firsttime;
static void init_display ( void );
static void redisplay();
static void pre_display ( void );
static void post_display ( void );
static void idle_func ( void );
static void key_func ( unsigned char key, int x, int y );
static void mouse_func ( int button, int state, int x, int y );
static void motion_func ( int x, int y );
static void reshape_func ( int width, int height );
// void drawTimelinePart(LineType line);
// void drawMessageTransit(LineType line);
static void display_func ( void );
static void open_glut_window ( void );

Parser* parser;
ObjectMap objects;
EventMap events;
LinkMap links;
// ShotMap shotmap;
// ShotList* shotlist;


static void init_display ( void )
{
  glViewport ( 0, 0, win_x, win_y );
  glMatrixMode ( GL_PROJECTION );
  glLoadIdentity ();
  glOrtho ( 0.0, 1.0, 1.0, 0.0, 2.0f, -2.0f );
  glMatrixMode ( GL_MODELVIEW );
  glLoadIdentity ();
  glClearColor(0, 0, 0, 1.);
  //glClearColor(.25, .25, .25, 1.);
  glDepthFunc(GL_LEQUAL);
}

static void redisplay()
{
  // ACTIVATE DISPLAY
  glutSetWindow ( win_id );
  glutPostRedisplay ();
}

static void pre_display ( void )
{
  glDisable(GL_DEPTH_TEST);
  glClear ( GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT );
  glEnable(GL_DEPTH_TEST);
}

static void post_display ( void )
{
  glutSwapBuffers ();
}

static void idle_func ( void )
{
  if(time_speed!=0)
  {
    afftime += time_speed * pow(2.0,time_div);
    if (afftime < timearea)
    {
      afftime = timearea;
      time_speed = 0;
    }
    if (afftime + timearea > lasttime)
    {
      afftime = lasttime-timearea;
      time_speed = 0;
    }
    if (afftime - timearea <firsttime)
    {
      afftime = firsttime+timearea;
      time_speed = 0;
    }
//     if (time_speed == 0)
//       glutIdleFunc(NULL);
  }

  redisplay();
}

static void key_func ( unsigned char key, int x, int y )
{
  switch ( key )
  {
  case 'q':
  case 'Q':
    parser->ClearAll();
    exit ( 0 );
    break;
  case '+':
    if (afftime == timearea)
      afftime = timearea /= 2;
    else
      timearea /= 2;
    redisplay();
    break;
  case '-':
    if (afftime == timearea)
    {
      afftime = timearea *= 2;
    }
    else
      timearea *= 2;
    if (2 * timearea > lasttime - firsttime) timearea = (lasttime - firsttime)/2;
//     if (afftime < timearea) afftime = timearea;
    if (afftime + timearea > lasttime) afftime = lasttime - timearea;
    if (afftime - timearea < firsttime) afftime = firsttime + timearea;
    redisplay();
    break;

  case '3':
    time_div--;
    break;

  case '9':
    time_div++;
    break;

  case '6':
    time_speed++;
//       if (time_speed == 1)
//       {
//         glutIdleFunc ( idle_func );
//       }
//       else if (time_speed == 0)
//         glutIdleFunc ( NULL );
    break;

  case '4':
    time_speed--;
//       if (time_speed == -1)
//       {
//         glutIdleFunc ( idle_func );
//       }
//       else if (time_speed == 0)
//         glutIdleFunc ( NULL );
    break;

  case '5':
//       glutIdleFunc ( NULL );
    time_speed = 0;
    break;

  }
}

double oldtime = 0;
double newtime = 0;
static void mouse_func ( int button, int state, int x, int y )
{
  mx = x;
  my = y;

  if (button == 0 && state == GLUT_DOWN)
  {
    mx0 = mx;
    my0 = my;
  }

  if (button == 2 && state == GLUT_DOWN)
  {
    double time = (afftime-timearea + mx*2*timearea/win_x);
    oldtime = newtime;
    newtime = time;
//     std::cout << "Time = " << newtime*1000 << " ms, dt = " << (newtime - oldtime)*1000 << " ms\n";
  }

  mouse_down &= ~(1<<button);
  mouse_down |= ((state == GLUT_DOWN)<<button);

	/* handling mouse wheel */
	/* only done whith mouse up */
	if (state == GLUT_UP)
	{
		/* Just mimicing the behavior of the '+' and '-' buttons */
		if (button == GLUT_WHEEL_UP)
		{
			if (afftime == timearea)
      	afftime = timearea /= 2;
    	else
      	timearea /= 2;
    	redisplay();
		}
		else if (button == GLUT_WHEEL_DOWN)
		{
			if (afftime == timearea)
			{
				afftime = timearea *= 2;
			}
			else
				timearea *= 2;
			if (2 * timearea > lasttime - firsttime) timearea = (lasttime - firsttime)/2;
			//     if (afftime < timearea) afftime = timearea;
			if (afftime + timearea > lasttime) afftime = lasttime - timearea;
			if (afftime - timearea < firsttime) afftime = firsttime + timearea;
			redisplay();
		}
	}
}

static void motion_func ( int x, int y )
{
  mx = x;
  my = y;

  if (mouse_down&1)
  {
    int dx = mx-mx0;
    afftime -= dx*(2*timearea)/win_x;
    if (afftime + timearea > lasttime) afftime = lasttime - timearea;
    if (afftime - timearea <firsttime) afftime = firsttime + timearea;
    if (afftime < timearea) afftime = timearea;
    mx0 = mx;
    redisplay();
  }

}

static void reshape_func ( int width, int height )
{
  glutSetWindow ( win_id );
  glutReshapeWindow ( width, height );

  win_x = width;
  win_y = height;
  glViewport ( 0, 0, win_x, win_y );

  redisplay();
}


static void display_func ( void )
{
  pre_display ();


//   Draw toolbar
  float py = 1.0f/win_y;
  float px = 1.0f/win_x;
  float toolbar_y0 = 5*py;
  float toolbar_sy= toolbar_y0+32.0f*py;
  float data_y0 = toolbar_sy+5*py;

  glBegin(GL_QUAD_STRIP);

  glColor3f(1,1,1);
  glVertex3f(0,0,1); glVertex3f(1,0,1);
  glColor3f(.25f,.25f,.25f);
  glVertex3f(0,toolbar_y0,0); glVertex3f(1,toolbar_y0,0);
  glVertex3f(0,toolbar_sy,0); glVertex3f(1,toolbar_sy,0);
  glColor3f(1,1,1);
  glVertex3f(0,data_y0,1); glVertex3f(1,data_y0,1);
  glEnd();

  glBegin(GL_TRIANGLES);
  glColor3f(1,1,1);
  glVertex3f(.5, data_y0 + (5*py), 0);
  glVertex3f(.5 + (5*px), toolbar_sy, 0);
  glVertex3f(.5 - (5*px), toolbar_sy, 0);
  glEnd();


//   Write some info in tool bar
  char text[256];
/*
//   help above keys and mouse
  glColor3f(.50f,.50f,.50f);
  sprintf(text,"Keys: quit 'q', zoom '+', unzoom '-', scroll left '4', scroll rigth '6', speed*2 '9', speed/2 '3', stop scroll '5'");
  glRasterPos3d(.5-((double)strlen(text)+1)*(px*5)/2,toolbar_y0+10*py,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }
  sprintf(text,"Mouse: left button: manualy scroll window, right button: mark times");
  glRasterPos3d(.5-((double)strlen(text)+1)*(px*5)/2,toolbar_y0+20*py,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }
*/
  glColor3f(1,1,1);

//   scale, scrolling speed,
  sprintf(text,"time scale: %g sec, scrolling speed: %g ms/it", timearea*2, (time_speed * pow(2.0,time_div))*1000);
  glRasterPos3d(.5-((double)strlen(text)+1)*(px*4)/2,toolbar_y0+15*py,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }

//   first screen time
  sprintf(text,"<- %f sec",afftime-timearea);
  glRasterPos3d(0,toolbar_sy,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }

//   center screen time
  sprintf(text,"%f sec",afftime);
  glRasterPos3d(0.5-((double)strlen(text)+1)*(px*5)/2,toolbar_sy,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }


//   last screen time
  sprintf(text,"%f sec ->",afftime+timearea);
  glRasterPos3d(1-((double)strlen(text)+1)*(px*5),toolbar_sy,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }

  glPushMatrix();


//   Draw objects
  int firstObj=-1, lastObj=-1;
  for(ObjectMap::iterator it=objects.begin();it!=objects.end();it++)
  {
    if( it->second->display and (it->second->pos < firstObj or firstObj == -1) )
      firstObj = it->second->pos;
    if( it->second->display and (it->second->pos > lastObj or lastObj == -1) )
      lastObj = it->second->pos;
  }

  glTranslated(0,data_y0,0.0);
  glScaled(1.0,(1-data_y0) / (lastObj-firstObj+1),1.0 );
  glTranslated(0.0,firstObj+0.5,0.0);

  for(ObjectMap::iterator it=objects.begin();it!=objects.end();it++)
    it->second->draw();

  glPushMatrix();


//   Draw time line
  glScaled(0.5/(timearea),1.0,1.0);
  glTranslated(timearea-afftime,0.0,0.0);

  double dt = 1;
  if(timearea > 10)
  {
    while( timearea/dt > 10 ) dt*=10;
  }
  if(timearea < 1)
  {
    while( timearea/dt < 1 ) dt/=10;
  }

  glColor3f(.35f,.35f,.35f);
  for(double i = ceil((afftime-timearea)/(2*dt))*2*dt;i<afftime+timearea;i+=2*dt)
  {
    glBegin(GL_LINES);
    glVertex3d(i,firstObj-.5,2);
    glVertex3d(i,lastObj+.5,2);
    glEnd();

    glRasterPos3d(i,lastObj+.5,2);
    sprintf(text,"%g",i);
    for(unsigned int j=0; j<strlen(text); j++)
    {
      glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
    }
  }

//   Draw mouse right button time

  glColor3f(1.f,1.f,1.f);

//   newtime
  glBegin(GL_LINES);
  glVertex3d(newtime,firstObj-.5,-2);
  glVertex3d(newtime,lastObj+.5,-2);
  glEnd();
  glRasterPos3d(newtime,lastObj+.5,-2);
  sprintf(text,"%g",newtime);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }

//   oldtime
  glBegin(GL_LINES);
  glVertex3d(oldtime,firstObj-.5,2);
  glVertex3d(oldtime,lastObj+.5,2);
  glEnd();
  glRasterPos3d(oldtime,lastObj+.5,2);
  sprintf(text,"%g",oldtime);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }

//   newtime - oldtime
  sprintf(text,"<- %g ms ->",fabs(newtime-oldtime)*1000);
  glRasterPos3d( newtime/2 + oldtime/2 - ( (((double)strlen(text)+1)*(px*5)/2) / (.5/timearea) )
                ,firstObj-.5 + (py*10) / ((1-data_y0)/(lastObj-firstObj+1))
                ,-1);
  for(unsigned int j=0; j<strlen(text); j++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[j]);
  }


//   Draw events
  for(EventMap::iterator it=events.begin();it!=events.end();it++)
    it->second->draw();


//   Draw event's links
  for(LinkMap::iterator it=links.begin();it!=links.end();it++)
    it->second->draw();


  glPopMatrix();
  glPopMatrix();

  post_display ();
}

static void open_glut_window ( void )
{
  glutInitDisplayMode ( GLUT_RGBA | GLUT_DOUBLE | GLUT_DEPTH );

  glutInitWindowPosition ( 0, 0 );
  glutInitWindowSize ( win_x, win_y );
  win_id = glutCreateWindow ( "flowvr-gltrace" );

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

void display(Parser* pars)
{
  parser = pars;

  objects = parser->GetObjectMap();
  events = parser->GetEventMap();
  links = parser->GetLinkMap();

  firsttime = parser->GetFirstTime()-.1;
  lasttime = parser->GetLastTime()+.1;

  std::cout << "First time = " << firsttime+.1 << ", Last time = " << lasttime-.1 << std::endl;

  timearea = 10;
  afftime = firsttime + 10;

  win_x = 640;
  win_y = 480;

  char *argv[]={(char*) "\0"};
  int argc=0;
  
  glutInit(&argc, argv);
  
  open_glut_window ();

// GLUT MAIN LOOP
  glutMainLoop ();
}


