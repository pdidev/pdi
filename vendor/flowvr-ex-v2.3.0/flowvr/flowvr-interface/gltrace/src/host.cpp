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
* File: src/gltrace/host.cpp                                      *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#include "gltrace.h"

using namespace flowvr;

Host::Host(std::string newname):name(newname)
{
}

Host::~Host()
{
}

xml::DOMElement Host::xmlDesc()
{
  xml::DOMElement res = xml::DOMElement("host");

  for(unsigned int i=0;i<loggerlist.size();i++)
  {
    xml::DOMElement log = xml::DOMElement("logger");
    log.SetAttribute("id",loggerlist[i]);
    res.InsertEndChild(log);
  }
  return res;
}

void Host_Average::addPing(int ID, PingInfo newping)
{
  if(ID>p2)
  {
    P1 = P2;
    P2.clear();
    P2.push_back(newping);
    p1 = p2;
    p2 = ID;
  }
  else if( ID == p2 )
  {
    P2.push_back(newping);
  }
  else if( ID < p2 && ID > p1 )
  {
    P1.clear();
    P1.push_back(newping);
    p1 = ID;
  }
  else if( ID == p1 )
  {
    P1.push_back(newping);
  }
}

void Host_Average::calculateCorrespondance()
{
  double x1=0, x2=0, y1=0, y2=0;

  double nb = P1.size();
  for(int i=0;i<nb;i++)
  {
    x1 += (double)P1[i].reply / nb;
    y1 += ( (P1[i].send / 2) + (P1[i].receive / 2) ) / nb;
  }

  nb = P2.size();
  for(int i=0;i<nb;i++)
  {
    x2 += (double)P2[i].reply / nb;
    y2 += ( (P2[i].send / 2) + (P2[i].receive / 2) ) / nb;
  }

  coeffA = ( y2 - y1 ) / ( x2 - x1 );
  coeffB = y1 - ( coeffA *  x1 );
}

double Host_Average::cycleToTime(cycle_t cycle)
{
  return ( ( (double)cycle * coeffA ) + coeffB );
}

xml::DOMElement Host_Average::xmlDesc()
{
  xml::DOMElement res = Host::xmlDesc();
  char buff[32];

  res.SetAttribute("id",name);
  res.SetAttribute("method","AVERAGE");
  res.SetAttribute("ping1",p1);
  res.SetAttribute("ping2",p2);
  sprintf(buff,"%e",getPrecision());
  res.SetAttribute("Precision",buff);
  sprintf(buff,"%e",coeffA);
  res.SetAttribute("coeffA",buff);
  sprintf(buff,"%e",coeffB);
  res.SetAttribute("coeffB",buff);

  return res;
}

double Host_Average::getPrecision()
{
  double res1=0, res2=0;

  double nb=P1.size();
  for(int i=0;i<nb;i++)
  {
    res1 += ( P1[i].send - P1[i].receive ) / nb;
  }

  nb=P2.size();
  for(int i=0;i<nb;i++)
  {
    res2 += ( P2[i].send - P2[i].receive ) / nb;
  }

  if( res1 > res2 )
    return res1;
  else
    return res2;
}

Host_Average::Host_Average(std::string newname):Host(newname)
{
  coeffA=0;
  coeffB=0;
  p1=0;
  p2=0;
  P1.clear();
  P2.clear();
}

Host_Average::~Host_Average()
{
  P1.clear();
  P2.clear();
}

void Host_Fastest::addPing(int ID, PingInfo newping)
{
  if(ID>p2)
  {
    P1 = P2;
    P2 = newping;
    p1 = p2;
    p2 = ID;
  }
  else if( ID == p2 )
  {
    if( (newping.receive - newping.send) < (P2.receive - P2.send) )
      P2 = newping;
  }
  else if( ID < p2 && ID > p1 )
  {
    P1 = newping;
    p1 = ID;
  }
  else if( ID == p1 )
  {
    if( (newping.receive - newping.send) < (P1.receive - P1.send) )
      P1 = newping;
  }
}

void Host_Fastest::calculateCorrespondance()
{
  double x1 = (double)P1.reply;
  double x2 = (double)P2.reply;
  double y1 = (P1.send/(double)2) + (P1.receive/(double)2);
  double y2 = (P2.send/(double)2) + (P2.receive/(double)2);

  coeffA = ( y2 - y1 ) / ( x2 - x1 );
  coeffB = y1 - ( coeffA * x1 );
}

double Host_Fastest::cycleToTime(cycle_t cycle)
{
  return ( ( (double)cycle * coeffA ) + coeffB );
}

double Host_Fastest::getPrecision()
{
  double res = P1.receive - P1.send;

  if( res < P2.receive - P2.send )
    res = P2.receive - P2.send;

  return res;
}

xml::DOMElement Host_Fastest::xmlDesc()
{
  xml::DOMElement res = Host::xmlDesc();
  char buff[32];

  res.SetAttribute("id",name);
  res.SetAttribute("method","FASTEST");
  res.SetAttribute("ping1",p1);
  res.SetAttribute("ping2",p2);
  sprintf(buff,"%e",getPrecision());
  res.SetAttribute("Precision",buff);
  sprintf(buff,"%e",coeffA);
  res.SetAttribute("coeffA",buff);
  sprintf(buff,"%e",coeffB);
  res.SetAttribute("coeffB",buff);

  return res;
}

Host_Fastest::Host_Fastest(std::string newname):Host(newname)
{
  coeffA=0;
  coeffB=0;
  p1=0;
  p2=0;
}

Host_Fastest::~Host_Fastest()
{
}










