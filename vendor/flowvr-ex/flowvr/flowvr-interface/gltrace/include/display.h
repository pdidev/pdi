/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                             Utils                               *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Ronan Gaugne,                                                *
*    Valerie Gouranton,                                           *
*    Loick Lecointre,                                             *
*    Sebastien Limet,                                             *
*    Clement Menier,                                              *
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/flowvr/gltrace/display.h                          *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/

class Display
{
public:
  virtual void draw(int pos)=0;
  virtual void draw(ShotInfo &shot)=0;
  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest)=0;

  virtual flowvr::xml::DOMElement xmlDesc();

  virtual int stringToColor(std::string colorName);

  Display(){}
  virtual ~Display(){}
};



class Display_Null: public Display
{
public:
  virtual void draw(int pos){}
  virtual void draw(ShotInfo &shot){}
  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest){}

  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Null(){}
  virtual ~Display_Null(){}
};



class Display_Object: public Display
{
public:
  int color;
  char colorStr[32];

  virtual void draw(ShotInfo &shot){}
  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest){}

  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Object(flowvr::xml::DOMElement* draw);
  virtual ~Display_Object(){}
};

class Display_Event: public Display
{
public:
  int color;
  char colorStr[32];

  virtual void draw(int pos){}
  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest){}

  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Event(flowvr::xml::DOMElement* draw);
  virtual ~Display_Event(){}
};

class Display_Link: public Display
{
public:

  virtual void draw(int pos){}
  virtual void draw(ShotInfo &shot){}

//   virtual xml::DOMElement xmlDesc();

  Display_Link(flowvr::xml::DOMElement* draw);
  virtual ~Display_Link(){}
};




class Display_Object_Line: public Display_Object
{
public:
  double linewidth;
  double linedepth;

  virtual void draw(int pos);
  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Object_Line(flowvr::xml::DOMElement* draw);
  virtual ~Display_Object_Line(){}
};

class Display_Object_Text: public Display_Object
{
public:
  double dy;
  std::string text;

  virtual void draw(int pos);
  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Object_Text(flowvr::xml::DOMElement* draw);
  virtual ~Display_Object_Text(){}
};



class Display_Event_Line: public Display_Event
{
public:
  double linewidth;

  virtual void draw(ShotInfo &shot);
  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Event_Line(flowvr::xml::DOMElement* draw);
  virtual ~Display_Event_Line(){}
};

class Display_Link_Line: public Display_Link
{
  public:
  int iColor_src;
  int iColor_dest;
  char sColor_src[32];
  char sColor_dest[32];

  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest);
  virtual flowvr::xml::DOMElement xmlDesc();
  Display_Link_Line(flowvr::xml::DOMElement* draw);
  virtual ~Display_Link_Line(){}
};

class Display_Link_ObjLine: public Display_Link
{
  public:
  int iColor;
  char sColor[32];
  double linewidth;
  double linedepth;

  virtual void draw(ShotInfo &shot_src, ShotInfo &shot_dest);

  virtual flowvr::xml::DOMElement xmlDesc();

  Display_Link_ObjLine(flowvr::xml::DOMElement* draw);
  virtual ~Display_Link_ObjLine(){}
};


