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
* File: ./src/SimulatedDevice/InputDevice.cpp                     *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include "InputDevice.h"

InputDevice::InputDevice(){
  name="no name";
}
InputDevice::InputDevice(string theName)
{
  name=theName;
  workspaceMin[0]=workspaceMin[1]=workspaceMin[2]=0.0;
  workspaceMax[0]=workspaceMax[1]=workspaceMax[2]=0.0;
}

InputDevice::~InputDevice()
{
}

void InputDevice::setName(string theName){
  name=theName;
}

string InputDevice::getName(){
  return name;
}


float *InputDevice::getWorkspaceMin(){
  return workspaceMin;
}


float *InputDevice::getWorkspaceMax(){
  return workspaceMax;
}

void InputDevice::setWorkspace(float *min, float *max){
	for (int i=0;i<3;i++){
		workspaceMin[i]=min[i];
		workspaceMax[i]=max[i];
	}
}

void InputDevice::addButton(Button *b){
  theButtons.push_back(b);
}
void InputDevice::addAxe(Axe *a){
  theAxes.push_back(a);
}
void InputDevice::addStick(Stick *s){
  theSticks.push_back(s);
}

void InputDevice::addTracker(Tracker *t) {
  theTrackers.push_back(t);
}

void InputDevice::updateButton(int id, int val){
	vector<Button *>::iterator itVectorData;
	for(itVectorData = theButtons.begin(); itVectorData != theButtons.end(); itVectorData++){
		if ((*(*itVectorData)).getId()==id)
			(*(*itVectorData)).setState(val);
	}
}

void InputDevice::updateAxe(int id, float axeX){
	vector<Axe *>::iterator itVectorData;
	for(itVectorData = theAxes.begin(); itVectorData != theAxes.end(); itVectorData++){
		if ((*(*itVectorData)).getId()==id)
			(*(*itVectorData)).setAxe(axeX);
	}
}

void InputDevice::updateStick(int id, float axeX, float axeY){
	vector<Stick *>::iterator itVectorData;
	for(itVectorData = theSticks.begin(); itVectorData != theSticks.end(); itVectorData++){
		if ((*(*itVectorData)).getId()==id)
			(*(*itVectorData)).setAxes(axeX,axeY);
	}
}

void InputDevice::updateTracker(int id, float *pos, float *ori){
	vector<Tracker *>::iterator itVectorData;
	for(itVectorData = theTrackers.begin(); itVectorData != theTrackers.end(); itVectorData++){
	  if ((*(*itVectorData)).getId()==id) {
	    (*(*itVectorData)).setPosition(pos);
	    (*(*itVectorData)).setOrientation(ori);
	  }
	}
}
	
vector <struct_buttons> InputDevice::getButtonChanges(){
	vector <struct_buttons> res;
	struct_buttons b;

	vector<Button *>::iterator it;
	for(it = theButtons.begin(); it != theButtons.end(); it++){
		if ((*(*it)).isModified()) {
		  b.id=(*(*it)).getId();
		  b.val=(*(*it)).getState();
		  res.push_back(b);
		}
	}
	return res;
}

vector <struct_trackers> InputDevice::getTrackerChanges(){
	vector <struct_trackers> res;
	struct_trackers t;
	float *m;

	vector<Tracker *>::iterator it;
	for(it = theTrackers.begin(); it != theTrackers.end(); it++){
		if ((*(*it)).isModified()) {
		  t.id=(*(*it)).getId();
		  m=(*(*it)).getPosition();
		  for (int i=0;i<3;i++)
		    t.pos[i]=m[i];
		  m=(*(*it)).getOrientation();
		  for (int i=0;i<16;i++)
		    t.ori[i]=m[i];
		  res.push_back(t);
		}
	}
	return res;
}


vector <struct_axes> InputDevice::getAnalog(){
	vector <struct_axes>  res;
	struct_axes a;
	
	// getting the axes	
	vector<Axe *>::iterator it1;
	for(it1 = theAxes.begin(); it1 != theAxes.end(); it1++){
	  a.id=(*(*it1)).getId();
	  a.val=(*(*it1)).getAxe();
	  res.push_back(a);
	}
	
	// getting the axes of sticks
	vector<Stick *>::iterator it2;
	for(it2 = theSticks.begin(); it2 != theSticks.end(); it2++){
	  a.id=(*(*it2)).getIdX();
	  a.val=(*(*it2)).getAxeX();
	  res.push_back(a);
	  a.id=(*(*it2)).getIdY();
	  a.val=(*(*it2)).getAxeY();
	  res.push_back(a);
	}	
	return res;
}

int InputDevice::isAnalogModified(){

  vector<Axe *>::iterator it1;

  for(it1 = theAxes.begin(); it1 != theAxes.end(); it1++){
    if ((*(*it1)).isModified())
      return 1;
  }

  vector<Stick *>::iterator it2;
  for(it2 = theSticks.begin(); it2 != theSticks.end(); it2++){
    if ((*(*it2)).isModified())
      return 1;
  }
  return 0;
}


vector <Button *> InputDevice::getButtons(){
	return theButtons;
}
vector <Axe *> InputDevice::getAxes(){
	return theAxes;
}
vector <Stick *> InputDevice::getSticks(){
	return theSticks;
}

vector <Tracker *> InputDevice::getTrackers(){
	return theTrackers;
}
