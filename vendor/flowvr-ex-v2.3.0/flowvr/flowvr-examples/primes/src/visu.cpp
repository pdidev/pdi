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
* File: ./visu.cpp                                                *
*                                                                 *
* Contacts:                                                       *
*     2004  Cyril Nortet <cyril.nortet@lifo.univ-orleans.fr>      *
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

#include "flowvr/module.h"
#include "ftl/chunkevents.h"
#include <flowvr/trace.h>
#include "flowvr/module.h"

// Application variables :
int win_id;

unsigned int *tPrimeNumbers = 0;
unsigned int primeNumbersCount;
unsigned int primeNumbersMaxCount;
unsigned int greatestPrimeNumber; // Greatest prime number computed since then
float *tPrimesCoordinatesOnSpiral = 0; // Coordinates (by pairs) of each computed prime number over the spiral pattern rendered

unsigned char *pKeysPressed = 0; // List of identifiers of the keys pressed
unsigned int keysPressedCount;   // Number of pressed keys stored in the list 'pKeysPressed'

int lastIterationComputeTime = 0;
int iterationCounterComputes = 0;
int iterationCounterVisu = 0;
int iterationCounterCapture = 0;

double timeElapsedSincePrevIteration = 0.0;

// FlowVR objects & variables :
flowvr::ModuleAPI* pFlowVRModule = 0;
flowvr::InputPort* pPortPrimesIn = 0;
flowvr::InputPort* pPortKeysIn = 0;
flowvr::StampInfo *pStampComputeTime = 0;

//declare new personnal trace object
flowvr::TypedTrace<int>* pMyTraceKeyPressedNotification = new flowvr::TypedTrace<int>("myTraceKeyPressedNotification");

/*
  ----------------------------------------------------------------------
   FlowVR related functions
  ----------------------------------------------------------------------
*/

// Initialize FlowVR module :
int SetupFlowVR()
{

  // Declare user defined ports :

  pPortPrimesIn = new flowvr::InputPort("primesIn");  // Prime numbers from compute nodes
  pPortKeysIn   = new flowvr::InputPort("keysIn");    // Keys pressed from capture node

  //  pStampComputeTime = new flowvr::StampInfo("computationTimeIt", flowvr::TypeInt::create());
  //  pPortPrimesIn->stamps->add(pStampComputeTime);

  std::vector <flowvr::Port*> ports;
  ports.push_back(pPortPrimesIn);
  ports.push_back(pPortKeysIn);

  std::vector <flowvr::Trace*> myPersonnalTraces;
  myPersonnalTraces.push_back(pMyTraceKeyPressedNotification);

  // Initializes the module to the FlowVR daemon, passing the traces vector
  if (!(pFlowVRModule = flowvr::initModule(ports,myPersonnalTraces)))
  {
    std::cerr << "Can't initialize flowVR module !" << std::endl;
    return -1;
  }
  return 0;
}



// Clean up FlowVR module :
void CleanFlowVR()
{
  // Release FlowVR module handler :
  if (pFlowVRModule)
  {
    pFlowVRModule->close();

    delete pFlowVRModule;
    pFlowVRModule = 0;
  }
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
  flowvr::Message msgRead;
  unsigned int *pMsgData = 0;
  unsigned int primeNumbersReceviedCount;

  // Get message from FlowVR daemon :
  pFlowVRModule->get(pPortPrimesIn, msgRead);

  // Count the prime numbers received from message size (in bytes) :
  primeNumbersReceviedCount = msgRead.data.getSize() / sizeof(unsigned int);

  // Get a pointer to the beginning of the message buffer, with type unsigned int :
  pMsgData = (unsigned int *)msgRead.data.getRead<unsigned int>();

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


  // At first iteration only, retrieve the StampInfo for the user defined  stamp 
  if (iterationCounterVisu <= 1 ) 
      pStampComputeTime = (*(pPortPrimesIn->stamps))[std::string("computationTimeIt")];

  // Read the stamp value to get the  cumulated computation time (for the last compute modules iteration) from the associated stamp :
  if (!msgRead.stamps.read(*pStampComputeTime, lastIterationComputeTime))
      lastIterationComputeTime = -1;

  // Read predefined 'it' stamp in order to know the iteration state of compute node :
  msgRead.stamps.read(pPortPrimesIn->stamps->it, iterationCounterComputes);
}



// Receive prime numbers from compute nodes :
void ReceiveKeysPressed()
{

  // Variable that will hold the message that contain the list of chunks
  flowvr::Message msgRead;

  // Counter for the pressed keys
  unsigned int keysPressedReceviedCount;

  // Get message from FlowVR daemon :
  pFlowVRModule->get(pPortKeysIn, msgRead);

  // Count the number of number of pressed keys from message size (in bytes) :
  keysPressedReceviedCount = msgRead.data.getSize() / sizeof(ftl::ChunkEventButton);

  // Update the size of the buffer that will store received data
  if (keysPressedCount != keysPressedReceviedCount)
  {
    if (pKeysPressed)
      delete [] pKeysPressed;
    if (keysPressedReceviedCount > 0)
      pKeysPressed = new unsigned char [keysPressedReceviedCount];
    else
      pKeysPressed = 0;
    keysPressedCount = keysPressedReceviedCount;
  }

  // Send a trace notification each time capture message is not empty, meaning one or more key(s) is pressed by the user.
  // The number of keys simultaneously pressed constitute the trace data.
  if (keysPressedReceviedCount > 0)
      pMyTraceKeyPressedNotification->write((int)keysPressedReceviedCount);

  // Read predefined 'it' stamp in order to know the iteration state of capture node :
  msgRead.stamps.read(pPortKeysIn->stamps->it, iterationCounterCapture);

  int count=0;	// Counter used to fill up the keys vector
  // Looks for each chunk in the list of chunks received
  for (ftl::ChunkIterator it = ftl::chunkBegin(msgRead); it != ftl::chunkEnd(msgRead) ; it++ , count++)
  {

    const ftl::Chunk* c = (const ftl::Chunk*) it;			// Convert the iterator in a chunk base structure
    // if  chunk I/O event and Button  then save key id in key pressed list.
    if (  (c->type &  0x0F) &&  ftl::ChunkEvent::BUTTON )
      pKeysPressed[count] = ((ftl::ChunkEventButton *)c)->key;


  }

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

  for (unsigned int ikey=0; ikey<keysPressedCount; ikey++)
    switch(pKeysPressed[ikey])
      {
      case FLOWVR_KEY_UP:  sprintf((char*)tText, "Up");DrawText(offset,50, (char*)tText); break;
      case FLOWVR_KEY_DOWN: sprintf((char*)tText, "Down");DrawText(offset+40, 50, (char*)tText);break;
      case FLOWVR_KEY_LEFT: sprintf((char*)tText, "Left");DrawText(offset+80, 50, (char*)tText);break;
      case FLOWVR_KEY_RIGHT: sprintf((char*)tText, "Right");DrawText(offset+120, 50, (char*)tText); break;
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

  // Process each key received, and rotate the view accordingly :
  for (unsigned int ikey=0; ikey<keysPressedCount; ikey++)
    switch(pKeysPressed[ikey])
    {
    case FLOWVR_KEY_UP:    glRotatef(timeElapsedSincePrevIteration*ratio, 1.0f, 0.0f, 0.0f); break;
    case FLOWVR_KEY_DOWN:  glRotatef(-timeElapsedSincePrevIteration*ratio, 1.0f, 0.0f, 0.0f); break;
    case FLOWVR_KEY_LEFT:  glRotatef(timeElapsedSincePrevIteration*ratio, 0.0f, 1.0f, 0.0f); break;
    case FLOWVR_KEY_RIGHT: glRotatef(-timeElapsedSincePrevIteration*ratio, 0.0f, 1.0f, 0.0f); break;
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
  if (!pFlowVRModule->wait())
  {
    // Clean up and exit :
    if (tPrimeNumbers)
      delete [] tPrimeNumbers;
    if (pKeysPressed)
      delete [] pKeysPressed;
    CleanFlowVR();
    exit(0);
  }
  else
    // Visu module enters into a new iteration :
    iterationCounterVisu++;

  // Read received data (only if port connected to some producer)
  if (pFlowVRModule->isPortConnected(pPortPrimesIn)) ReceivePrimeNumbers();
  if (pFlowVRModule->isPortConnected(pPortKeysIn)) ReceiveKeysPressed();

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
  if (SetupFlowVR() != 0)
    return -1;

  // Set up data structures and variables :
  primeNumbersMaxCount = 1000000;
  tPrimeNumbers = new unsigned int[primeNumbersMaxCount];
  tPrimesCoordinatesOnSpiral = new float [primeNumbersMaxCount*2]; // Coordinates are stored by pairs
  primeNumbersCount = 0;
  greatestPrimeNumber = 0;
  pKeysPressed = 0;
  keysPressedCount = 0;
  lastIterationComputeTime = -1;
  iterationCounterComputes = iterationCounterVisu = iterationCounterCapture = 0;

  // Initialize GLUT :
  glutInit(&argc, argv);
  OpenGlutWindow(150, 10, 800, 600);

  // Main execution loop :
  glutMainLoop();

  return 0;
}
