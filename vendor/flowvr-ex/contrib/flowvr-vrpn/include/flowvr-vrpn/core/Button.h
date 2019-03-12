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
* File: ./include/flowvr-vrpn/core/Button.h                       *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef BUTTON_H_
#define BUTTON_H_

#include <glui.h>


#include "Element.h"

class Button : public Element
{
	int oldState;
	int state;	
	GLUI_Checkbox* simButton;
public:
	Button(string theName, int id);
	virtual ~Button();
	void setSimButton(GLUI *glui, GLUI_Panel *panel);
	GLUI_Checkbox* getSimButton();
	void setState(int s);
	int getState();
	int isModified();
};

#endif /*BUTTON_H_*/
