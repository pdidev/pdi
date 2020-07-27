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
* File: ./src/SimulatedDevice/Stick.cpp                           *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include "Stick.h"

#include "stdio.h"

Stick::Stick(string theName, int idX, int idY, bool analog):  Element( theName, idX*1000+idY)
{
	idx=idX;
	idy=idY;
	oldAxes[0]=axes[0]=oldAxes[1]=axes[1]=0.0;
	simStick=NULL;
	simReset=NULL;
	simPanel=NULL;
	analogic=analog;
}

Stick::~Stick()
{
}
void Stick::setSimStick(GLUI * glui, GLUI_Panel * panel, GLUI_Update_CB resetStickCB, float speed){
   simPanel=glui->add_panel_to_panel(panel,(char*)this->getName().c_str());
   simStick=glui->add_translation_to_panel(simPanel,"", GLUI_TRANSLATION_XY, axes);
   simStick->set_speed(speed);
   simReset=glui->add_button_to_panel(simPanel,"Release",this->getId(),resetStickCB);
}

GLUI_Translation* Stick::getSimStick(){
  return simStick;
}

GLUI_Button*  Stick::getSimReset(){
  return simReset;
}

GLUI_Panel*  Stick::getSimPanel(){
  return simPanel;
}

void Stick::setAxes( float X, float Y ){

  if (analogic) {
    if ((X<=1.)&&(X>=-1.)&&(Y<=1.)&&(Y>=-1.)){
      oldAxes[0]=axes[0];
      oldAxes[1]=axes[1];
      axes[0]=X;
      axes[1]=Y;
   }
  }
  else {
    if (X>0.) oldAxes[0]=axes[0]=1.;
    if (X<0.) oldAxes[0]=axes[0]=-1.;
    if (X==0.) oldAxes[0]=axes[0]=0.;
    if (Y>0.) oldAxes[1]=axes[1]=1.;
    if (Y<0.) oldAxes[1]=axes[1]=-1.;
    if (Y==0.) oldAxes[0]=axes[0]=0.;
  }
  simStick->set_x(X);
  simStick->set_y(Y);
}

float Stick::getAxeX(){
  if (analogic) {

    // since axes return values between -1 and 1
    // when the translation is out this interval
    // the value is forced to either -1 or 1
    if (axes[0]>1.) {
      axes[0]=1.;
      simStick->set_x(1.);
    }
    if (axes[0]<-1.) {
      axes[0]=-1.;
      simStick->set_x(-1.);
    }
  }
  else {
    // in case of the stick is binary


    if (axes[0]>0.) {
      axes[0]=1.;
      simStick->set_x(1.);
    }
    if (axes[0]<0.) {
      axes[0]=-1.;
      simStick->set_x(-1.);
    }
  }

  oldAxes[0]=axes[0];
  return axes[0];
}


float Stick::getAxeY(){
  if (analogic) {
    // since axes returns values between -1 and 1
    // when the translation is out this interval
    // the value is forced to either -1 or 1
    if (axes[1]>1.) {
      axes[1]=1.;
      simStick->set_y(1.);
    }

    if (axes[1]<-1.) {
      axes[1]=-1.;
      simStick->set_y(-1.);
    }
  }
  else {
    // in case of the stick is binary
    // Pour les y il faut inverser à voir avec Sébastien
    if (axes[1]>0.) {
      axes[1]=-1.;
      simStick->set_y(-1.);
    }
    if (axes[1]<0.) {
      axes[1]=1.;
      simStick->set_y(1.);
    }
  }

  oldAxes[1]=axes[1];
  return axes[1];
}

int Stick::getIdX(){
	return idx;
}

int Stick::getIdY(){
	return idy;
}



int Stick::isModified(){
	return ((oldAxes[0]!=axes[0])||(oldAxes[1]!=axes[1]));
}

int Stick::isXModified(){
	return (oldAxes[0]!=axes[0]);
}

int Stick::isYModified(){
	return (oldAxes[1]!=axes[1]);
}
