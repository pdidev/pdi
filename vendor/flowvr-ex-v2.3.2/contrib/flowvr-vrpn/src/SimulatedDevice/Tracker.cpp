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
* File: ./src/SimulatedDevice/Tracker.cpp                         *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include "Tracker.h"

Tracker::Tracker()
{
}

Tracker::Tracker(string theName, int id) : Element(theName, id)
{
  type = ELEMENT_TRACKER;
  oldPosition[0]=oldPosition[1]=oldPosition[2]=1.0;
  position[0]=position[1]=position[2]=0.0;

  for (int i=0; i<16; i++){
    orientation[i]=oldOrientation[i]=0.0;
  }
  orientation[0]=oldOrientation[0]=orientation[5]=oldOrientation[5]=orientation[10]=oldOrientation[10]=orientation[15]=oldOrientation[15]=1.0;
}

Tracker::~Tracker()
{
}

void Tracker::setSimTracker(GLUI * glui, GLUI_Panel * panel, float speed){
  GLUI_Panel * trackPanel;

  trackPanel=glui->add_panel_to_panel(panel,(char*)this->getName().c_str());
  simTrackerXY=glui->add_translation_to_panel(trackPanel,"X and Y", GLUI_TRANSLATION_XY, position);
   simTrackerXY->set_speed(speed);
   glui->add_column_to_panel(trackPanel,false);
   simTrackerZ=glui->add_translation_to_panel(trackPanel,"Z",GLUI_TRANSLATION_Z, position+2);
   simTrackerZ->set_speed(speed);
   glui->add_column_to_panel(trackPanel,false);
   glui->add_rotation_to_panel(trackPanel,"Orientation",orientation);}


GLUI_Translation* Tracker::getSimTrackerXY(){
  return simTrackerXY;
}

GLUI_Translation* Tracker::getSimTrackerZ(){
  return simTrackerZ;
}

GLUI_Rotation* Tracker::getSimOrientation(){
  return simOrientation;
}

void Tracker::setPosition(float *pos){
	for (int i=0;i<3;i++){
		oldPosition[i]=position[i];
		position[i]=pos[i];
	}
}

float *Tracker::getPosition(){
	for (int i=0;i<3;i++){
		oldPosition[i]=position[i];
	}
	return position;
}

void Tracker::setOrientation(float *ori){
	for (int i=0;i<16;i++){
		oldOrientation[i]=orientation[i];
		orientation[i]=ori[i];
	}
}

float *Tracker::getOrientation(){
	for (int i=0;i<16;i++){
		oldOrientation[i]=orientation[i];
	}
	return orientation;
}







int Tracker::isPosModified(){
	return (oldPosition[0]!=position[0])||(oldPosition[1]!=position[1])||(oldPosition[2]!=position[2]);
}
int Tracker::isOrientationModified(){
	for (int i=0;i<16;i++){
		if (oldOrientation[i]!=orientation[i])
			return 1;
	}
	return 0;
}

int Tracker::isModified(){
  return isPosModified()||isOrientationModified();
}
