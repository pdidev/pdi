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
* File: ./src/SimulatedDevice/Button.cpp                          *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#include <iostream>
#include "Button.h"

Button::Button(string theName, int id) : Element( theName, id)
{
	type=ELEMENT_BUTTON;
	oldState=state=0;
}

Button::~Button()
{
}

void Button::setSimButton(GLUI *glui, GLUI_Panel *panel){
  simButton=glui->add_checkbox_to_panel(panel, (char*)this->getName().c_str(), &state);
}

GLUI_Checkbox* Button::getSimButton(){
  return simButton;
}

void Button::setState(int s)
{
	oldState=state;
	state=s;
}

int Button::getState(){
	oldState=state;
	return state;
}

int Button::isModified(){
	if (oldState!=state) {
		return 1;
	}
	return 0;
}
