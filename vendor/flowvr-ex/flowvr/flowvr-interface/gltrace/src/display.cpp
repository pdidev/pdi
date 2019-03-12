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
* File: src/gltrace/display.cpp                                   *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#include "gltrace.h"

using namespace flowvr;
using namespace flowvr::utils::gltrace; // TODO: Put everything inside namespaces

// class Display: 'abstract class'
xml::DOMElement Display::xmlDesc()
{
  return xml::DOMElement("display");
}

int Display::stringToColor(std::string colorName)
{
  return getColor(colorName);
}


// class Display_Null: public Display
xml::DOMElement Display_Null::xmlDesc()
{
  return xml::DOMElement("null");
}



// class Display_Object: public Display
Display_Object::Display_Object(xml::DOMElement* draw):Display()
{
  const char* col = draw->Attribute("color");

  if( !col )
    col = "GRAY";

  strcpy(colorStr,col);

  color = stringToColor(colorStr);
}

xml::DOMElement Display_Object::xmlDesc()
{
  xml::DOMElement desc = Display::xmlDesc();

  desc.SetAttribute("color",colorStr);

  return desc;
}



// class Display_Object_Line: public Display_Object
Display_Object_Line::Display_Object_Line(xml::DOMElement* draw):Display_Object(draw)
{
  const char* tmp = draw->Attribute("width");

  if( tmp==NULL or !strcmp(tmp,"medium") )
    linewidth = 0.2;
  else if( !strcmp(tmp,"thin") )
    linewidth = 0.1;
  else if( !strcmp(tmp,"thick") )
    linewidth = 0.4;
  else
    linewidth = atof(tmp);

  if( linewidth<=0. or linewidth>0.5 ) linewidth = 0.2;


  tmp = draw->Attribute("depth");
  if( tmp==NULL or !strcmp(tmp,"middle") )
    linedepth = 0.5;
  else if( !strcmp(tmp,"top") )
    linedepth = 0.0;
  else if( !strcmp(tmp,"bottom") )
    linedepth = 1.;
  else
    linedepth = atof(tmp);

  if( linewidth<=0. or linewidth>1. ) linedepth = .5;

}

xml::DOMElement Display_Object_Line::xmlDesc()
{
  xml::DOMElement desc = Display_Object::xmlDesc();
  char tmp[32];

  desc.SetValue("objline");

  sprintf(tmp,"%.2f", linewidth);
  desc.SetAttribute("width",tmp);

  sprintf(tmp,"%.2f", linewidth);
  desc.SetAttribute("depth",tmp);

  return desc;
}

void Display_Object_Line::draw(int pos)
{
  glBegin(GL_QUADS);
    glColor3f(0,0,0);
    glVertex3d(0,pos-linewidth,linedepth+0.5); glVertex3d(1,pos-linewidth,linedepth+0.5);
    glColor3ubv((unsigned char*)&color);
    glVertex3d(1,pos,linedepth+0.5); glVertex3d(0,pos,linedepth+0.5);
    glVertex3d(0,pos,linedepth+0.5); glVertex3d(1,pos,linedepth+0.5);
    glColor3f(0,0,0);
    glVertex3d(1,pos+linewidth,linedepth+0.5); glVertex3d(0,pos+linewidth,linedepth+0.5);
  glEnd();
}




// class Display_Object_Text: public Display_Object
Display_Object_Text::Display_Object_Text(xml::DOMElement* draw):Display_Object(draw)
{
  const char* tmp;

  tmp = draw->Attribute("text");
  if(tmp==NULL) tmp = "";
  text = tmp;

  tmp = draw->Attribute("pos");
  if(tmp==NULL or !strcmp(tmp,"middle"))
    dy = 0.0;
  else if( !strcmp(tmp,"up") )
    dy = 0.3;
  else if( !strcmp(tmp,"down") )
    dy = -0.3;
  else
    dy = atof(tmp);

  if(dy < -0.5 or dy > 0.5) dy = 0.0;
}

xml::DOMElement Display_Object_Text::xmlDesc()
{
  xml::DOMElement desc = Display_Object::xmlDesc();
  char tmp[32];
  sprintf(tmp,"%.2f",dy);

  desc.SetValue("objtext");
  desc.SetAttribute("text",text);
  desc.SetAttribute("pos",tmp);

  return desc;
}

void Display_Object_Text::draw(int pos)
{
  glColor3ubv((unsigned char*)&color);
  glRasterPos3d(0,pos-dy,-1);
  for(unsigned int i=0; i<text.size(); i++)
  {
    glutBitmapCharacter(GLUT_BITMAP_HELVETICA_10,text[i]);
  }
}



// class Display_Event: public Display
Display_Event::Display_Event(xml::DOMElement* draw):Display()
{
  const char* col = draw->Attribute("color");

  if(!col)
    col ="GRAY";

  strcpy(colorStr,col);

  color = stringToColor(colorStr);
}

xml::DOMElement Display_Event::xmlDesc()
{
  xml::DOMElement desc = Display::xmlDesc();

  desc.SetAttribute("color",colorStr);

  return desc;
}



// class DISPLAY_EVENT_LINE: public DISPLAY_EVENT
void Display_Event_Line::draw(ShotInfo &shot)
{
  glColor3ubv((unsigned char*)&color);
  glBegin(GL_LINES);
    glVertex3d(shot.time,shot.pos+linewidth,0.0);
    glVertex3d(shot.time,shot.pos-linewidth,0.0);
  glEnd();
}

Display_Event_Line::Display_Event_Line(xml::DOMElement* draw):Display_Event(draw)
{
  const char* tmp = draw->Attribute("width");

  if( tmp==NULL or !strcmp(tmp,"medium") )
    linewidth = 0.3;
  else if( !strcmp(tmp,"thin") )
    linewidth = 0.2;
  else if( !strcmp(tmp,"thick") )
    linewidth = 0.5;
  else
    linewidth = atof(tmp);

  if( linewidth<=0. or linewidth>0.5 ) linewidth = 0.3;
}

xml::DOMElement Display_Event_Line::xmlDesc()
{
  xml::DOMElement desc = Display_Event::xmlDesc();

  char tmp[32];
  sprintf(tmp,"%.2f",linewidth);

  desc.SetValue("evtline");
  desc.SetAttribute("width",tmp);

  return desc;
}




// class Display_Link: public Display
Display_Link::Display_Link(xml::DOMElement* draw):Display()
{
}




// class Display_Link_Line: public Display
void Display_Link_Line::draw(ShotInfo &shot_src, ShotInfo &shot_dest)
{
  glBegin(GL_LINES);
    glColor3ubv((unsigned char*)&iColor_src);
    glVertex3d(shot_src.time,shot_src.pos,0.0);
    glColor3ubv((unsigned char*)&iColor_dest);
    glVertex3d(shot_dest.time,shot_dest.pos,0.0);
  glEnd();
}

xml::DOMElement Display_Link_Line::xmlDesc()
{
  xml::DOMElement desc = Display_Link::xmlDesc();

  desc.SetValue("lnkline");
  desc.SetAttribute("srccolor", sColor_src);
  desc.SetAttribute("destcolor", sColor_dest);

  return desc;
}

Display_Link_Line::Display_Link_Line(xml::DOMElement* draw):Display_Link(draw)
{
  const char* col = draw->Attribute("srccolor");
  if(!col)
    col = "GRAY";

  strcpy(sColor_src,col);

  iColor_src = stringToColor(sColor_src);

  col = draw->Attribute("destcolor");
  if(!col)
    col = "GRAY";

  strcpy(sColor_dest,col);

  iColor_dest = stringToColor(sColor_dest);
}



// class Display_Link_ObjectLine: public Display
Display_Link_ObjLine::Display_Link_ObjLine(xml::DOMElement* draw):Display_Link(draw)
{
  const char* col = draw->Attribute("color");

  if( !col )
    col = "GRAY";

  strcpy(sColor,col);

  iColor = stringToColor(sColor);

  const char* tmp = draw->Attribute("width");

  if( tmp==NULL or !strcmp(tmp,"medium") )
    linewidth = 0.2;
  else if( !strcmp(tmp,"thin") )
    linewidth = 0.1;
  else if( !strcmp(tmp,"thick") )
    linewidth = 0.4;
  else
    linewidth = atof(tmp);

  if( linewidth<=0. or linewidth>0.5 ) linewidth = 0.2;


  tmp = draw->Attribute("depth");
  if( tmp==NULL or !strcmp(tmp,"middle") )
    linedepth = 0.5;
  else if( !strcmp(tmp,"top") )
    linedepth = 0.0;
  else if( !strcmp(tmp,"bottom") )
    linedepth = 1.;
  else
    linedepth = atof(tmp);

  if( linewidth<=0. or linewidth>1. ) linedepth = 0.5;
}

xml::DOMElement Display_Link_ObjLine::xmlDesc()
{
  xml::DOMElement desc = Display_Link::xmlDesc();
  char tmp[32];

  desc.SetValue("objline");
  desc.SetAttribute("color",sColor);

  sprintf(tmp,"%.2f",linewidth);
  desc.SetAttribute("width",tmp);

  sprintf(tmp,"%.2f",linedepth);
  desc.SetAttribute("depth",tmp);

  return desc;
}

void Display_Link_ObjLine::draw(ShotInfo &shot_src, ShotInfo &shot_dest)
{
  glBegin(GL_QUADS);
    glColor3f(0,0,0);
    glVertex3d(shot_src.time,shot_src.pos-linewidth,linedepth+0.5); glVertex3d(shot_dest.time,shot_dest.pos-linewidth,linedepth+0.5);
    glColor3ubv((unsigned char*)&iColor);
    glVertex3d(shot_dest.time,shot_dest.pos,linedepth+0.5); glVertex3d(shot_src.time,shot_src.pos,linedepth+0.5);
    glVertex3d(shot_src.time,shot_src.pos,linedepth+0.5); glVertex3d(shot_dest.time,shot_dest.pos,linedepth+0.5);
    glColor3f(0,0,0);
    glVertex3d(shot_dest.time,shot_dest.pos+linewidth,linedepth+0.5); glVertex3d(shot_src.time,shot_src.pos+linewidth,linedepth+0.5);
  glEnd();
}



