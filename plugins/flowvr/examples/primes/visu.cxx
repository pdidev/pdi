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
* File: ./visu.cxx                                                *
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
#include <sys/time.h>

#include <pdi.h>

// Application variables :
int win_id;

unsigned int *tPrimeNumbers = 0;
unsigned int primeNumbersCount;
unsigned int primeNumbersMaxCount;
unsigned int greatestPrimeNumber; // Greatest prime number computed since then
float *tPrimesCoordinatesOnSpiral = 0; // Coordinates (by pairs) of each computed prime number over the spiral pattern rendered

enum keys {UP,DOWN,LEFT,RIGHT};
int pKeysPressed[4] = {false,false,false,false}; // array to save the current key state (pressed == true)

int lastIterationComputeTime = 0;
int iterationCounterComputes = 0;
int iterationCounterVisu = 0;
int iterationCounterCapture = 0;

double timeElapsedSincePrevIteration = 0.0;

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

// Function used at reception of new prime numbers to pre-compute prime numbers coordinates over the spiral pattern :
void CalculateSpiralCoordinates(int startIndex, int count)
{
	unsigned int primeNumber;
	for (int i=0; i<count; i++)
	{
		primeNumber = tPrimeNumbers[startIndex+i];   // each prime number determine the radius and angle of a given spiral point
		tPrimesCoordinatesOnSpiral[(startIndex+i)*2]   = (float)( (double)primeNumber * cos((double)primeNumber) );
		tPrimesCoordinatesOnSpiral[(startIndex+i)*2+1] = (float)( (double)primeNumber * sin((double)primeNumber) );
	}
}


// Receive prime numbers from compute nodes :
void ReceivePrimeNumbers()
{
	unsigned int *pMsgData = 0;
	// Get message from FlowVR daemon :
	PDI_access("tTempPrimeNumbers_shr", (void**)&pMsgData, PDI_IN);

	int* primeNumbersReceviedSize_ptr;
	// Count the prime numbers received from message size (in bytes) :
	PDI_access("tempPrimeNumbersMaxCount", (void**)&primeNumbersReceviedSize_ptr, PDI_IN);
	int primeNumbersReceviedCount = *primeNumbersReceviedSize_ptr / sizeof(int);
	PDI_release("tempPrimeNumbersMaxCount");

	// Append received prime numbers to the current list :
	if (primeNumbersReceviedCount > 0)
	{
		// The prime numbers buffer is "cyclic" in order to keep no more than 'primeNumbersMaxCount' values at a time :
		if ((primeNumbersCount%primeNumbersMaxCount) + primeNumbersReceviedCount <= primeNumbersMaxCount)
		{
			memcpy((void*)(tPrimeNumbers+(primeNumbersCount%primeNumbersMaxCount)), (void*)pMsgData, primeNumbersReceviedCount*sizeof(unsigned int));
			CalculateSpiralCoordinates(primeNumbersCount%primeNumbersMaxCount, primeNumbersReceviedCount);
		}
		else
		{
			// Cut the set of values in two parts to be copied across the prime numbers buffer :
			unsigned int valuesFirstPart  = primeNumbersMaxCount-(primeNumbersCount%primeNumbersMaxCount);
			unsigned int valuesSecondPart = primeNumbersReceviedCount - valuesFirstPart;
			if (valuesFirstPart > 0)
			{
				memcpy((void*)(tPrimeNumbers+(primeNumbersCount%primeNumbersMaxCount)), (void*)pMsgData, valuesFirstPart*sizeof(unsigned int));
				CalculateSpiralCoordinates(primeNumbersCount%primeNumbersMaxCount, valuesFirstPart);
			}
			memcpy((void*)tPrimeNumbers, (void*)(pMsgData+valuesFirstPart), valuesSecondPart*sizeof(unsigned int));
			CalculateSpiralCoordinates(0, valuesSecondPart);
		}
		primeNumbersCount += primeNumbersReceviedCount;

		// Update value of the greatest prime number received since then :
		for (unsigned int i=0; i<primeNumbersReceviedCount; i++)
			if (pMsgData[i] > greatestPrimeNumber)
				greatestPrimeNumber = pMsgData[i];
	}

	PDI_release("tTempPrimeNumbers_shr");

	PDI_expose("lastIterationComputeTime", &lastIterationComputeTime, PDI_IN);
	PDI_expose("iterationCounterComputes", &iterationCounterComputes, PDI_IN);
}



// Receive prime numbers from compute nodes :
void ReceiveKeysPressed()
{
	PDI_expose("up", &pKeysPressed[UP], PDI_IN);
	PDI_expose("down", &pKeysPressed[DOWN], PDI_IN);
	PDI_expose("left", &pKeysPressed[LEFT], PDI_IN);
	PDI_expose("right", &pKeysPressed[RIGHT], PDI_IN);

	// Counter for the pressed keys
	// unsigned int keysPressedReceviedCount = *up + *down + *left + *right;
	unsigned int keysPressedReceviedCount = pKeysPressed[UP] + pKeysPressed[DOWN] + pKeysPressed[LEFT] + pKeysPressed[RIGHT];

	// Send a trace notification each time capture message is not empty, meaning one or more key(s) is pressed by the user.
	// The number of keys simultaneously pressed constitute the trace data.
	if (keysPressedReceviedCount > 0) {
		PDI_expose("keysPressedReceviedCount", &keysPressedReceviedCount, PDI_OUT);
	}

	// Read predefined 'it' stamp in order to know the iteration state of capture node :
	PDI_expose("iterationCounterCapture", &iterationCounterCapture, PDI_IN);
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

	glMatrixMode(GL_PROJECTION);
	glLoadIdentity();
	gluOrtho2D(-0.5, 0.5, -0.5, 0.5);
	glMatrixMode(GL_MODELVIEW);
	glLoadIdentity();

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
	glColor3f(0.0f, 1.0f, 0.0f);
	glRasterPos2i(x, y);

	for (int iChar=0; pText[iChar]; iChar++)
		glutBitmapCharacter(GLUT_BITMAP_HELVETICA_12, pText[iChar]);

	glPopMatrix();
	glMatrixMode(GL_PROJECTION);
	glPopMatrix();
	glMatrixMode(GL_MODELVIEW);
}


void DrawAxes(GLfloat x1,
	   GLfloat x2,
	   GLfloat y1,
	   GLfloat y2,
	   GLfloat z1,
	   GLfloat z2)
{
	glBegin(GL_LINES);
	glColor3f(1.0,0.0,0.0);//red
	glVertex3f(x1,0.0,0.0);
	glVertex3f(x2,0.0,0.0);
	glColor3f(0.0,1.0,0.0);//green
	glVertex3f(0.0,y1, 0.0);
	glVertex3f(0.0,y2, 0.0);
	glColor3f(1.0,1.0,0.0);//yellow
	glVertex3f(0.0,0.0,z1);
	glVertex3f(0.0,0.0,z2);
	glEnd();
}



void PreDisplay()
{
	glClear(GL_COLOR_BUFFER_BIT);
}



void PostDisplay()
{
	// Count frame rate :

	static int fpsCounter = -1;  // Don't count first frame
	static int fpsCurrent = -1;  // -1 if framerate has not been set yet
	static double accumTime = 0.0;

	// Update counters :
	fpsCounter++;
	accumTime += timeElapsedSincePrevIteration;
	if (accumTime > 1.0)
	{
		fpsCurrent = (int)(((double)fpsCounter) / accumTime);
		accumTime = 0.0;
		fpsCounter = 0;
	}

	//
	GLfloat   a=0.2;
	DrawAxes(-a,a,-a,a,-a,a);

	// Draw infos on the screen :
	char tText[255];
	if (fpsCurrent >= 0)
	{
		sprintf((char*)tText, "FPS : %d", fpsCurrent);
		DrawText(10, 50, (char*)tText);
	}
	sprintf((char*)tText, "Current iteration: [Compute(s): %d]  [Visu: %d]  [Capture: %d]", iterationCounterComputes, iterationCounterVisu, iterationCounterCapture);
	DrawText(10, 35, (char*)tText);
	sprintf((char*)tText, "Prime numbers computed: %d", primeNumbersCount);
	DrawText(10, 20, (char*)tText);
	if (lastIterationComputeTime >= 0)
		sprintf((char*)tText, "Prime compution Time (last iteration): %d.%.03d milliseconds", lastIterationComputeTime/1000, lastIterationComputeTime%1000);
	else
		sprintf((char*)tText, "Prime compution Time (last iteration): Error");
	DrawText(10, 5, (char*)tText);


	// Display keys received.
	int offset = 100;
	sprintf((char*)tText, "Key Pressed:");DrawText(offset, 50, (char*)tText);
	offset += 100;

	if (pKeysPressed[UP]) {
		sprintf((char*)tText, "Up");
		DrawText(offset,50, (char*)tText);
	}
	if (pKeysPressed[DOWN]) {
		sprintf((char*)tText, "Down");
		DrawText(offset+40, 50, (char*)tText);
	}
	if (pKeysPressed[LEFT]) {
		sprintf((char*)tText, "Left");
		DrawText(offset+80, 50, (char*)tText);
	}
	if (pKeysPressed[RIGHT]) {
		sprintf((char*)tText, "Right");
		DrawText(offset+120, 50, (char*)tText);
	}


	// Swap Opengl buffers :

	glutSwapBuffers();
}



// Draw prime numbers pattern :
void DisplayFunc()
{
	float zoom;
	float X, Y;
	unsigned int primeNumberCounter;

	PreDisplay();

	zoom = 1.0f / (greatestPrimeNumber+1.0f);
	primeNumberCounter = 0;

	glBegin(GL_POINTS);

	// 'primeNumbersCount' can excess 'primeNumbersMaxCount' if the limit of drawable points has been reached :
	while ( (primeNumberCounter < primeNumbersCount) && (primeNumberCounter < primeNumbersMaxCount) )
	{
		// Alternate color 
		if ((primeNumberCounter%2)>0){
			glColor3f(1.0f, 0.0f, 0.0f);
		}
		else{
			glColor3f(0.0f, 1.0f, 0.0f);
		}

		X = 0.5f * tPrimesCoordinatesOnSpiral[primeNumberCounter*2] * zoom;
		Y = 0.5f * tPrimesCoordinatesOnSpiral[primeNumberCounter*2+1] * zoom;
		glVertex2f(X, Y);
		primeNumberCounter++;
	}
	glEnd();

	PostDisplay();
}




// Use latest keys state to update viewing conditions :
void ProcessKeysPressed()
{
	glMatrixMode(GL_MODELVIEW);

	double  ratio= 150.0f;

	if (pKeysPressed[UP]) {
		glRotatef(timeElapsedSincePrevIteration*ratio, 1.0f, 0.0f, 0.0f);
	}
	if (pKeysPressed[DOWN]) {
		glRotatef(-timeElapsedSincePrevIteration*ratio, 1.0f, 0.0f, 0.0f);
	}
	if (pKeysPressed[LEFT]) {
		glRotatef(timeElapsedSincePrevIteration*ratio, 0.0f, 1.0f, 0.0f);
	}
	if (pKeysPressed[RIGHT]) {
		glRotatef(-timeElapsedSincePrevIteration*ratio, 0.0f, 1.0f, 0.0f);
	}
}




/*
  ----------------------------------------------------------------------
   GLUT functions
  ----------------------------------------------------------------------
*/

void ReshapeFunc(int width, int height)
{
	glutSetWindow(win_id);
	glutReshapeWindow(width, height);

	glViewport(0, 0, width, height);
}



// Body of the main loop :
void IdleFunc()
{
	timeval time;
	static double timePreviousIt = -1.0;
	double timeCurrentIt = 0.0;

	// Blocks until new data is available on the input port.
	// Note that the synchronisation message on predefined output port "endIt" is sent just after the wait complete.
	// It is better to notify the compute nodes before the rendering task in order them to start as soon as possible a new calculation
	// and thus maximizing coupling.
	int wait;
	PDI_expose("wait", &wait, PDI_IN);
	if (!wait)
	{
		// Clean up and exit :
		if (tPrimeNumbers)
			delete [] tPrimeNumbers;
		CleanFlowVR();
		exit(0);
	}
	else
		// Visu module enters into a new iteration :
		iterationCounterVisu++;

	// Read received data (only if port connected to some producer)
	int keysInConnection;
	PDI_expose("keysInConnection", &keysInConnection, PDI_IN);
	if (keysInConnection) ReceivePrimeNumbers();

	int primesInConnection;
	PDI_expose("primesInConnection", &primesInConnection, PDI_IN);

	if (primesInConnection) ReceiveKeysPressed();

	// Compute time elapsed since previous iteration, in seconds :
	gettimeofday(&time, NULL);
	timeCurrentIt = ((double)time.tv_sec) + ((double)time.tv_usec)*0.000001;
	if (timePreviousIt >= 0.0)
		timeElapsedSincePrevIteration = timeCurrentIt - timePreviousIt;
	else
		timeElapsedSincePrevIteration = 0.0;
	timePreviousIt = timeCurrentIt;

	// Use keys pressed information to update display according to user interaction :
	ProcessKeysPressed();

	// Update display:
	glutSetWindow(win_id);
	glutPostRedisplay();
}



// OpenGlutWindow --- open a glut compatible window and set callbacks
void OpenGlutWindow(int posx, int posy, int width, int height)
{
	glutInitDisplayMode(GLUT_RGBA | GLUT_DOUBLE);

	glutInitWindowPosition(posx, posy);
	glutInitWindowSize(width, height);
	win_id = glutCreateWindow("FlowVR Primes Visu ");

	InitDisplay(width, height);

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
	// Init FlowVR environment :
	PC_tree_t conf = PC_parse_path("visu.yml");
	PDI_init(conf);

	// Set up data structures and variables :
	primeNumbersMaxCount = 1000000;
	tPrimeNumbers = new unsigned int[primeNumbersMaxCount];
	tPrimesCoordinatesOnSpiral = new float [primeNumbersMaxCount*2]; // Coordinates are stored by pairs
	primeNumbersCount = 0;
	greatestPrimeNumber = 0;
	lastIterationComputeTime = -1;
	iterationCounterComputes = iterationCounterVisu = iterationCounterCapture = 0;

	// Initialize GLUT :
	glutInit(&argc, argv);
	OpenGlutWindow(150, 10, 800, 600);

	// Main execution loop :
	glutMainLoop();

	return 0;
}
