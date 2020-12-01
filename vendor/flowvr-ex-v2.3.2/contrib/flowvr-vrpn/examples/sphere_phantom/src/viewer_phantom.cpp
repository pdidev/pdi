/******* COPYRIGHT ************************************************
 *                                                                 *
 *                         FlowVR VRPN                             *
 *                    FlowVR VRPN Coupling Modules                 *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (EA 4022) ALL RIGHTS RESERVED.                                  *
 *                                                                 *
 * This source is covered by the GNU GPL, please refer to the      *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Sebastien Limet,                                             *
 *    Sophie Robert.                                               *
 *                                                                 * 
 *******************************************************************
 *                                                                 *
 * File: ./examples/sphere_joypad/src/viewer_joypad.cpp            *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#include <iostream>

#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
//#include <GL/gl.h>      
//#include <GL/glu.h>      

#include <ftl/chunkevents.h>
#include <ftl/chunkreader.h>
//#include <ftl/quat.h>
    
#include <math.h>

#include "flowvr/module.h"

using namespace std;
GLfloat lmodel_ambient[]={0.2,0.2,0.2,0.2};
GLfloat mat_shininess[] = { 20.0 };
GLfloat light_position[] = { 0.0, 0.0, 1.0, 0.0 };


GLfloat blanc[4] = {1.0,1.0,1.0,0.0};
GLfloat bleu[4]  = {0.0,1.0,1.0,0.0};
GLfloat rouge[4] = {1.0,0.0,0.0,0.0};
GLfloat vert[4]  = {0.0,1.0,0.0,0.0};

GLfloat* ccolor  = blanc; 
GLfloat* s1color = rouge;
GLfloat* s2color = vert;

GLfloat Centre1[3] = {-1.0,0.0,0.0};
GLfloat Centre2[3] = { 1.0,0.0,0.0};

GLfloat Sommet[3] = {0.0, 1.0, 0.0};
GLfloat Cone[3]   = {0.0,0.0,0.0};
GLfloat dep[3]    = {0.0,0.0,0.0};

GLfloat tracker[3] = {0.0,0.0,0.0};
GLfloat tracker_old[3] = {0.0,0.0,0.0};


bool valide[3] = {0, 0, 0};
bool bouton[2] = {0, 0};

bool active = 0;
bool activation = 0;
float sphere1, sphere2;

// For gluLookAt 
GLfloat CameraX = 0.0;
GLfloat CameraY = 0.0;
GLfloat CameraZ = 0.0;

GLfloat CameraPas = 0.001;

GLfloat theta = 0.0;
GLfloat psi = 0.0;
GLfloat theta_active, psi_active;

using namespace flowvr;

ModuleAPI* FlowVRModule = 0;

std::vector <Port*> ports;

InputPort  *In1 = new InputPort("Analog");
Message MIn1;
InputPort  *In2 = new InputPort("Button");
Message MIn2;

void reinit() {    
    for (int i=0; i<3; i++) {
        Centre1[i] =  0.0;
        Centre2[i] =  0.0;
        Sommet[i]  =  0.0;
        Cone[i]    =  0.0;
        dep[i]     =  0.0;
    }
    Centre1[0] = -1.0;
    Centre2[0] =  1.0;
    Sommet[1]  = 1.0;
}


void init() 
{ 
    glClearColor(0.0f, 0.0f, 0.0f, 1.0f);
    glEnable(GL_DEPTH_TEST);
    glLightModelfv(GL_LIGHT_MODEL_AMBIENT,lmodel_ambient);
    glMaterialfv(GL_FRONT, GL_SHININESS, mat_shininess);
    
    glLightfv(GL_FRONT, GL_SPECULAR, vert);
    glLightfv(GL_FRONT, GL_AMBIENT, vert);
    glLightfv(GL_FRONT, GL_SPECULAR, vert);    
    glLightfv(GL_LIGHT0, GL_POSITION, light_position);      
}

void Display(void) 
{
    
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
    glLoadIdentity();
    
    //gluLookAt(5*cosf(theta)*sinf(psi),CameraY+5*sinf(theta),CameraZ+5*cosf(theta)*cosf(psi),0.0,0.0,0.0,5*cosf(theta+1)*sinf(psi),5*sinf(theta+1),5*cosf(theta+1)*cosf(psi));
    gluLookAt(0.0,0.0,5.0,0.0,0.0,0.0,0.0,1.0,0.0);
    
    //    glColor3f(0.5,0.5,0.5);
    
    glBegin(GL_LINES);
    
    // Le carré
    glVertex3f( 4.0,  4.0, -10.0);
    glVertex3f( 4.0, -4.0, -10.0);
    
    glVertex3f( 4.0, -4.0, -10.0);
    glVertex3f(-4.0, -4.0, -10.0);
    
    glVertex3f(-4.0, -4.0, -10.0);
    glVertex3f(-4.0,  4.0, -10.0);
    
    glVertex3f(-4.0,  4.0, -10.0);
    glVertex3f( 4.0,  4.0, -10.0); 
        

    // Les lignes
    glVertex3f( 4.0,  4.0, -10.0);
    glVertex3f( 4.0,  4.0,  10.0);
    
    glVertex3f( 4.0,  2.0, -10.0);
    glVertex3f( 4.0,  2.0,  10.0);

    glVertex3f( 4.0,  0.0, -10.0);
    glVertex3f( 4.0,  0.0,  10.0);

    glVertex3f( 4.0,  -2.0, -10.0);
    glVertex3f( 4.0,  -2.0,  10.0);
    
    glVertex3f( 4.0,  -4.0, -10.0);
    glVertex3f( 4.0,  -4.0,  10.0);



    glVertex3f( -4.0,  4.0, -10.0);
    glVertex3f( -4.0,  4.0,  10.0);
    
    glVertex3f( -4.0,  2.0, -10.0);
    glVertex3f( -4.0,  2.0,  10.0);

    glVertex3f( -4.0,  0.0, -10.0);
    glVertex3f( -4.0,  0.0,  10.0);

    glVertex3f( -4.0,  -2.0, -10.0);
    glVertex3f( -4.0,  -2.0,  10.0);
    
    glVertex3f( -4.0,  -4.0, -10.0);
    glVertex3f( -4.0,  -4.0,  10.0);


    glVertex3f(  4.0,  4.0, -10.0);
    glVertex3f(  4.0,  4.0,  10.0);
    
    glVertex3f(  2.0,  4.0, -10.0);
    glVertex3f(  2.0,  4.0,  10.0);

    glVertex3f(  0.0,  4.0, -10.0);
    glVertex3f(  0.0,  4.0,  10.0);

    glVertex3f( -2.0,  4.0, -10.0);
    glVertex3f( -2.0,  4.0,  10.0);
    
    glVertex3f( -4.0,  4.0, -10.0);
    glVertex3f( -4.0,  4.0,  10.0);

    glVertex3f(  4.0,  -4.0, -10.0);
    glVertex3f(  4.0,  -4.0,  10.0);
    
    glVertex3f(  2.0,  -4.0, -10.0);
    glVertex3f(  2.0,  -4.0,  10.0);

    glVertex3f(  0.0,  -4.0, -10.0);
    glVertex3f(  0.0,  -4.0,  10.0);

    glVertex3f( -2.0,  -4.0, -10.0);
    glVertex3f( -2.0,  -4.0,  10.0);
    
    glVertex3f( -4.0,  -4.0, -10.0);
    glVertex3f( -4.0,  -4.0,  10.0);

    glEnd();   
    
    glRotatef(theta,1.0,0.0,0.0);
    glRotatef(psi,0.0,0.0,1.0);
    
    glPushMatrix();
        
    glTranslatef(Cone[0], Cone[1], Cone[2]);  
    //    glTranslatef(dep[0], dep[1], dep[2]);  
    glTranslatef(0.0,2.0,0.0);
    glRotatef(90.0,1.0,0.0,0.0);
    
    glMaterialfv(GL_FRONT, GL_AMBIENT, ccolor);
    glMaterialfv(GL_FRONT, GL_SPECULAR, ccolor);   
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);         
    glutSolidCone(0.3,1.0,10,10);      
    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    
    glPopMatrix();
    glPushMatrix();
    
    glTranslatef(Centre1[0],Centre1[1],Centre1[2]);    
    glMaterialfv(GL_FRONT, GL_AMBIENT, s1color);
    glMaterialfv(GL_FRONT, GL_SPECULAR, s1color);   
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);       
    glutSolidSphere(1.0, 30, 30); 
    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    
    
    glPopMatrix();  
    glPushMatrix();
    
    glTranslatef(Centre2[0],Centre2[1],Centre2[2]);
    glMaterialfv(GL_FRONT, GL_AMBIENT, s2color);
    glMaterialfv(GL_FRONT, GL_SPECULAR, s2color);   
    glEnable(GL_LIGHTING);
    glEnable(GL_LIGHT0);  
    glutSolidSphere(1.0, 30, 30);    
    glDisable(GL_LIGHTING);
    glDisable(GL_LIGHT0);
    
    glPopMatrix();
    
    glutSwapBuffers();    
}

void Reshape(int Width, int Height)
{
    glViewport(0, 0, Width, Height);
    glMatrixMode(GL_PROJECTION);
    glLoadIdentity();
    
    gluPerspective(90.0,1,0.1,1000.0);
    
    glMatrixMode(GL_MODELVIEW);
    glLoadIdentity();
}

void Idle() {
    
    if (!FlowVRModule->wait()) 
        exit(0);
    
    if (In1->isConnected()) {
        FlowVRModule->get(In1,MIn1);      
        
        for (ftl::ChunkIterator it = ftl::chunkBegin(MIn1); it!=ftl::chunkEnd(MIn1); it++) {
            const ftl::Chunk* c = (const ftl::Chunk*) it;
            
            if ((c->type & 0x0F) == ftl::ChunkEvent::POSITION) {
                const ftl::ChunkEventPosition* pos = (const ftl::ChunkEventPosition*) c;	
                for(int i=0 ; i<3 ; i++) {
                    tracker_old[i] = tracker[i];
                    tracker[i] = 50*pos->Transform[0][i];	
                    if (tracker[i]>tracker_old[i]) {
                        dep[i]=0.0005;
                        valide[i] = 1;
                    }
                    else if (tracker[i]<tracker_old[i]) {
                        dep[i]=-0.0005;
                        valide[i] = 1;
                    }
                    else 
                        valide[i] = 0;

                    cout << dep[i] << " ";
                }
                cout << endl;
            }
        }
    }
    
    theta += 2.0*theta_active;
    psi += 2.0*psi_active;
    
    if (In2->isConnected()) {
        FlowVRModule->get(In2,MIn2);        
        for (ftl::ChunkIterator it = ftl::chunkBegin(MIn2); it != ftl::chunkEnd(MIn2) ; it++) {
            const ftl::Chunk* c = (const ftl::Chunk*) it;             
            if ((c->type & 0x0F) == ftl::ChunkEvent::BUTTON) {	
                const ftl::ChunkEventButton* button = (const ftl::ChunkEventButton*) c;                
                int btn_key = button->key;
                switch(btn_key) {
                case 0 : 
                    if (button->val == 1)
                        active = (active+1)%2;   
                    if (active) 
                        reinit();
                    break;                    
                case 4 : 
                    if (button->val == 1) 
                        bouton[0] = 1;
                    else
                        bouton[0] = 0;
                    break;
                case 5:
                    if (button->val == 1) 
                        bouton[1] = 1;
                    else
                        bouton[1] = 0;
                    break;
                }
            }
        }
    }
    
    if (bouton[0]) 
        CameraZ+=CameraPas;    
    if (bouton[1])
        CameraZ-=CameraPas;        
        
    if (active) {
        for (int i=0; i<3; i++) 
            if (valide[i]) {
                Cone[i] += dep[i];
                Sommet[i] += dep[i];
            }
        ccolor = bleu;
    }
    else
        ccolor = blanc;
        
    if (activation) {
        if (sphere1 <= 1) 
            for (int i=0; i<3; i++) 
                if (valide[i]) Centre1[i] += dep[i];
        if (sphere2 <= 1) 
            for (int i=0; i<3; i++) 
                if (valide[i]) Centre2[i] += dep[i];
    }        
    
    if (active) {
        sphere1 = 0;
        for (int i=0; i<3; i++) sphere1 += (Sommet[i]-Centre1[i])*(Sommet[i]-Centre1[i]);
        sphere2 = 0;
        for (int i=0; i<3; i++) sphere2 += (Sommet[i]-Centre2[i])*(Sommet[i]-Centre2[i]);
        
        if (sphere1 <= 1) {            
            activation = 1;
            s1color = bleu;
        }
        else 
            s1color = rouge;

        if (sphere2 <= 1) {
            activation = 1;
            s2color = bleu;
        }
        else
            s2color = vert;            
    }
    else {
        s1color = rouge;
        s2color = vert;
        activation = 0;
    }
    glutPostRedisplay();    
}

int main(int argc, char** argv) 
{         
    ports.push_back(In1);
    ports.push_back(In2);
    
    FlowVRModule = initModule(ports);
    
    if (!FlowVRModule) 
        return -1;
    
    glutInit (&argc,argv) ; 
    glutInitDisplayMode (GLUT_DOUBLE | GLUT_RGB | GLUT_DEPTH) ; 
    
    glutInitWindowSize (512,512) ;   
    glutInitWindowPosition (250,250) ;     
    glutCreateWindow("Test du joypad") ;     
    init() ;   
    glutReshapeFunc(Reshape);
    glutDisplayFunc (Display);
    glutSetCursor(GLUT_CURSOR_NONE);    
    glutIdleFunc(Idle);        
    glutMainLoop () ;         
   return 0 ; 

}
