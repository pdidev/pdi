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
* File: ./include/flowvr-vrpn/core/InputDevice.h                  *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef INPUTDEVICE_H_
#define INPUTDEVICE_H_

#include "AllElements.h"
#include <vector>
#include <string>
using namespace std;

struct struct_axes {
  int id;
  float val;
};
struct struct_buttons {
  int id;
  int val;
};

struct struct_trackers {
  int id;
  float pos[3];
  float ori[16];
};

class InputDevice
{
	string name;

  // workspace information
  float workspaceMin[3];
  float workspaceMax[3];


	vector <Button*> theButtons;
	vector <Axe*> theAxes;
	vector <Stick*> theSticks;
	vector <Tracker*> theTrackers;
	
public:
	InputDevice();
	InputDevice(string theName);
	virtual ~InputDevice();
	void setName(string theName);
	string getName();
	void setWorkspace(float *min, float *max);
	float *getWorkspaceMin();
	float *getWorkspaceMax();
	void addButton(Button *b);
	void addAxe(Axe *a);
	void addStick(Stick *s);
	void addTracker(Tracker *t);
	void updateButton(int id, int val);
	void updateAxe(int id, float axeX);
	void updateStick(int id, float axeX, float axeY);
	void updateTracker(int id, float *pos, float *ori);
	
	vector <struct_buttons> getButtonChanges();
	vector <struct_axes> getAnalog();
	vector <struct_trackers> getTrackerChanges();

	vector <Button *> getButtons();
	vector <Axe *> getAxes();
	vector <Stick *> getSticks();
	vector <Tracker *> getTrackers();
	
	int isAnalogModified();
};

#endif /*INPUTDEVICE_H_*/
