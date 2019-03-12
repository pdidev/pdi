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
* File: ./capture.cxx                                             *
*                                                                 *
* Contacts:                                                       *
*     2004  Cyril Nortet <cyril.nortet@lifo.univ-orleans.fr>      *
*     15/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>         *
*                                                                 *
******************************************************************/
#include <stdlib.h>
#include <stdio.h>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <math.h>
#include <unistd.h>   // Give access to the usleep() function used to suspend process execution

#include <pdi.h>

// Application variables :
int win_id;

// FlowVR objects & variables :
enum keys {UP,DOWN,LEFT,RIGHT};
int keyStates[4] = {false,false,false,false}; // array to save the current key state (pressed == true)

/*
  ----------------------------------------------------------------------
   FlowVR related functions
  ----------------------------------------------------------------------
*/
// Clean up FlowVR module :
void CleanFlowVR()
{
	PDI_finalize();
}



// Send the list of keys pressed at the present time :
void SendKeysPressed()
{
	// expose state of the buttons
	PDI_expose("up", &keyStates[UP], PDI_OUT);
	PDI_expose("down", &keyStates[DOWN], PDI_OUT);
	PDI_expose("left", &keyStates[LEFT], PDI_OUT);
	PDI_expose("right", &keyStates[RIGHT], PDI_OUT);
}


/*
  ----------------------------------------------------------------------
   OpenGL specific drawing routines
  ----------------------------------------------------------------------
*/

void InitDisplay(int width, int height)
{
	glViewport(0, 0, width, height);

	glDisable(GL_DEPTH_TEST);

	glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
	glClear(GL_COLOR_BUFFER_BIT);
	glutSwapBuffers();
	glClear(GL_COLOR_BUFFER_BIT);
	glutSwapBuffers();
}



// Draw the specified text over the screen, at (x,y) specified in window coordinates :
void DrawText(int x, int y, char *pText)
{
	glMatrixMode(GL_PROJECTION);
	glPushMatrix();
	glLoadIdentity();
	glOrtho(0.0, glutGet(GLUT_WINDOW_WIDTH), 0.0, glutGet(GLUT_WINDOW_HEIGHT), -1.0, 1.0);
	glMatrixMode(GL_MODELVIEW);
	glPushMatrix();
	glLoadIdentity();
	glColor3f(0.6f, 0.8f, 0.2f);
	glRasterPos2i(x, y);

	for (int iChar=0; pText[iChar]; iChar++)
	glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, pText[iChar]);

	glPopMatrix();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
}



// Draw blank window :
void DisplayFunc()
{
	glClear(GL_COLOR_BUFFER_BIT);

	// Draw instructions :
	char tText[255];

	sprintf((char*)tText, "Focus and use");
	DrawText(2, 80, (char*)tText);
	sprintf((char*)tText, "arrow keys to");
	DrawText(2, 60, (char*)tText);
	sprintf((char*)tText, "rotate pattern");
	DrawText(2, 40, (char*)tText);

	glutSwapBuffers();
}




/*
  ----------------------------------------------------------------------
   GLUT functions
  ----------------------------------------------------------------------
*/


// Message handler for key down events :
void KeyDownSpecialFunc(int key, int x, int y)
{
	switch(key)
	{
		case GLUT_KEY_UP:
			keyStates[UP] = true;
			break;
		case GLUT_KEY_DOWN:
			keyStates[DOWN] = true;
			break;
		case GLUT_KEY_LEFT:
			keyStates[LEFT] = true;
			break;
		case GLUT_KEY_RIGHT:
			keyStates[RIGHT] = true;
			break;
	}
}


// Message handler for key up events :
void KeyUpSpecialFunc(int key, int x, int y)
{
	switch(key)
	{
		case GLUT_KEY_UP:
			keyStates[UP] = false;
			break;
		case GLUT_KEY_DOWN:
			keyStates[DOWN] = false;
			break;
		case GLUT_KEY_LEFT:
			keyStates[LEFT] = false;
			break;
		case GLUT_KEY_RIGHT:
			keyStates[RIGHT] = false;
			break;
	}
}



void ReshapeFunc(int width, int height)
{
	glutSetWindow(win_id);
	glutReshapeWindow(width, height);

	glViewport(0, 0, width, height);
}



// Body of the main loop :
void IdleFunc()
{
	// Send keyboard capture information :

	SendKeysPressed();

	// Limit sampling rate to 100 Hz.
	// A really too fast module is useless and would flood FlowVR network with too many messages.
	usleep(1000);

	// Blocks until new data is available on the input port.
	int wait = 0;
	PDI_expose("wait", &wait, PDI_IN);
	if (!wait)
	{
		CleanFlowVR();
		exit(0);
	}
}



// OpenGlutWindow --- open a glut compatible window and set callbacks
void OpenGlutWindow(int posx, int posy, int width, int height)
{
	glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);

	glutInitWindowPosition(posx, posy);
	glutInitWindowSize(width, height);
	win_id = glutCreateWindow("FlowVR Primes Capture");
	glutIgnoreKeyRepeat(1); // ignore   auto repeat  keystrokes.

	InitDisplay(width, height);

	glutSpecialFunc(KeyDownSpecialFunc);
	glutSpecialUpFunc(KeyUpSpecialFunc);
	glutReshapeFunc(ReshapeFunc);
	glutIdleFunc(IdleFunc);
	glutDisplayFunc(DisplayFunc);
}



/*
  ----------------------------------------------------------------------
   main --- main routine
  ----------------------------------------------------------------------
*/

int main(int argc, char ** argv)
{
	PC_tree_t conf = PC_parse_path("capture.yml");
	PDI_init(conf);

	// Initialize GLUT :
	glutInit(&argc, argv);
	OpenGlutWindow(10, 10, 120, 100);

	// Main execution loop :
	glutMainLoop();

	return 0;
}
