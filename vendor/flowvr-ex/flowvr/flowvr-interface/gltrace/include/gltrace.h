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
* File: include/flowvr/gltrace/gltrace.h                          *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_UTILS_GLTRACE_H
#define FLOWVR_UTILS_GLTRACE_H

#include <vector>
#include <map>
#include <list>
#include <iostream>
#include <sstream>
#include <fstream>
#ifdef __APPLE__
#include <GLUT/glut.h>
#else
#include <GL/glut.h>
#endif
#include <flowvr/common.h>
#include <flowvr/xml.h>

/* Constants for handling mouse wheel */
#if !defined(GLUT_WHEEL_UP)
#  define GLUT_WHEEL_UP   3
#  define GLUT_WHEEL_DOWN 4
#endif

enum type
{
  FASTER=0,  // method of the faster ping
  AVERAGE,  // methode of the average ping
  WAIT,  // connection between a waitBegin and a waitEnd
//   DATATYPE,  // connection between two datatype ports
//   STAMPS,  // connection between two stamps ports
//   MODULE,  // module object
//   FILTER,  // filter object
//   SYNCHRONIZER, // synchronizer object
  OBJECT,
  EVENT,
  LINK,
  UNKNOW  // unknow type
};

typedef unsigned long long cycle_t;

typedef struct
{
  double send;
  cycle_t reply;
  double receive;
} PingInfo;

typedef struct
{
  cycle_t cycle;
  int val;
} TraceInfo;

class ShotInfo
{
public:
  double time;
  int val;
  int pos;

  bool operator < (const ShotInfo &other) const
  {
    return time <= other.time;
  }
};

typedef std::list<int> OwnShotList;
typedef std::list<ShotInfo> ShotList;

#include "host.h"
#include "colors.h"
#include "display.h"
#include "object.h"
#include "trace.h"
#include "event.h"
#include "match.h"
#include "link.h"


typedef std::vector< std::string > FileList;
typedef std::map<std::string,Host*> HostMap;
typedef std::map<std::string,Host*> LoggerMap;
typedef std::map<std::string,Object*> ObjectMap;
typedef std::map<std::string,Trace*> TraceMap;
typedef std::map<std::string,Event*> EventMap;
typedef std::map<std::string,Link*> LinkMap;

#include "parser.h"

// void display(ObjectMap* obj, EventMap* evt, double first, double last);
void display(Parser* parser);

#endif //#if !define FLOWVR_UTILS_GLTRACE_H

