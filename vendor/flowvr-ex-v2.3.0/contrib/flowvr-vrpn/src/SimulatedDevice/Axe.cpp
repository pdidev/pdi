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
* File: ./src/SimulatedDevice/Axe.cpp                             *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include "Axe.h"

Axe::Axe()
{
}

Axe::Axe(string theName, int id): Element(theName,id)
{
	type=ELEMENT_SLIDER;
	axe=0.0;
	oldAxe=0.0;
	simAxe=NULL;
	simReset=NULL;
	simPanel=NULL;
}


Axe::~Axe()
{
}

void Axe::setSimAxe(GLUI * glui, GLUI_Panel * panel, GLUI_Update_CB resetAxeCB, float speed){
   simPanel=glui->add_panel_to_panel(panel,(char*)this->getName().c_str());
   simAxe=glui->add_translation_to_panel(simPanel,(char*)"", GLUI_TRANSLATION_X, &axe);
   simAxe->set_speed(speed);
   simReset=glui->add_button_to_panel(simPanel,(char*)"Release",this->getId(),resetAxeCB);
}

GLUI_Translation* Axe::getSimAxe(){
  return simAxe;
}

GLUI_Button* Axe::getSimReset(){
  return simReset;
}

GLUI_Panel* Axe::getSimPanel(){
  return simPanel;
}

float Axe::getAxe()
{
  // since axes returns values between -1 and 1
  // when the translation is out this interval
  // the value is forced to either -1 or 1
  if (axe > 1.) {
    axe=1.;
    simAxe->set_x(1.);
  }
  if (axe < -1.) {
    axe=-1.;
    simAxe->set_x(-1.);
  }
  oldAxe=axe;
  return axe;
}

void Axe::setAxe( float X ){
  if ((X<=1.)&&(X>=-1.)) {
    oldAxe=axe;
    axe=X;
    simAxe->set_x(X);
  }
}

int Axe::isModified(){
	if (oldAxe!=axe) {
		return 1;
	}
	return 0;
}
