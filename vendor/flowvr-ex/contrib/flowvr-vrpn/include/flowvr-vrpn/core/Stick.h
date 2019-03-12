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
* File: ./include/flowvr-vrpn/core/Stick.h                        *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef STICK_H_
#define STICK_H_

#include "Element.h"
#include <glui.h>

#define ANALOG_STICK 1
#define BINARY_STICK 0

class Stick : public Element
{
	int idx;
	int idy;

	bool analogic;
	float oldAxes[2];
	float axes[2];

	GLUI_Panel* simPanel;
	GLUI_Translation* simStick;
	GLUI_Button* simReset;
	
public:
	Stick(string theName, int idX, int idY, bool analog=ANALOG_STICK);
	virtual ~Stick();
	void setSimStick(GLUI * glui, GLUI_Panel * panel, GLUI_Update_CB resetStickCB, float speed=0.01);
	GLUI_Translation* getSimStick();
	GLUI_Button* getSimReset();
	GLUI_Panel* getSimPanel();
	void setAxes( float X, float Y );
	float getAxeX();
	float getAxeY();	
	int getIdX();
	int getIdY();
	int isModified();
	int isXModified();
	int isYModified();
};

#endif /*STICK_H_*/
