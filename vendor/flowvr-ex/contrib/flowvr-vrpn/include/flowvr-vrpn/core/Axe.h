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
* File: ./include/flowvr-vrpn/core/Axe.h                          *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef AXE_H_
#define AXE_H_

#include <glui.h>
#include "Element.h"

class Axe : public Element
{	
	float oldAxe;
	float axe;
	GLUI_Panel* simPanel;
	GLUI_Translation* simAxe;
	GLUI_Button* simReset;
public:
	Axe();
	Axe(string theName, int id);
	virtual ~Axe();
	void setSimAxe(GLUI * glui, GLUI_Panel * panel, GLUI_Update_CB resetAxeCB, float speed=0.05);
	GLUI_Translation* getSimAxe();
	GLUI_Button* getSimReset();
	GLUI_Panel* getSimPanel();
	float getAxe();
	void setAxe( float X );
	int isModified();
};

#endif /*AXE_H_*/
