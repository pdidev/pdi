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
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING-LIB file for further information.                       *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Sebastien Limet,                                             *
*    Sophie Robert.                                               *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./include/flowvr-vrpn/core/Tracker.h                      *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef TRACKER_H_
#define TRACKER_H_

#include <glui.h>
#include "Element.h"


class Tracker : public Element
{
  // position orientation
  float oldOrientation[16];
  float oldPosition[3];
  float position[3];
  float orientation[16];



  GLUI_Translation* simTrackerXY;
  GLUI_Translation* simTrackerZ;
  GLUI_Rotation* simOrientation;

public:

	Tracker();
	Tracker(string theName, int id);
	virtual ~Tracker();
	void setSimTracker(GLUI * glui, GLUI_Panel * panel, float speed=.001);
	GLUI_Panel* getSimPanel();
	GLUI_Translation* getSimTrackerXY();
	GLUI_Translation* getSimTrackerZ();
	GLUI_Rotation* getSimOrientation();

	void setPosition(float *pos);
	void setOrientation(float *ori);
	float *getPosition();
	float *getOrientation();
	int isPosModified();
	int isOrientationModified();
	int isModified();
};

#endif /*TRACKER_H_*/
