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
* File: ./include/flowvr-vrpn/core/Element.h                      *
*                                                                 *
* Contacts:                                                       *
*  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
*                                                                 *
******************************************************************/
#ifndef ELEMENT_H_
#define ELEMENT_H_

#include <string>
using namespace std;

#define ELEMENT_BUTTON              0
#define ELEMENT_STICK               1
#define ELEMENT_SLIDER              2
#define ELEMENT_TRACKER             3
#define ELEMENT_TRACKER_ORIENTATION 4

class Element
{
protected:
	string name;
	int type;
	int identifier;

public:
	Element();
	Element(string theName, int id);
	virtual ~Element();
	string getName();
	int getType();
	int getId();
};

#endif /*ELEMENT_H_*/
