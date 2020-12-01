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
* File: include/flowvr/gltrace/parser.h                           *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
class Parser
{
  bool error;
  std::string errText;
  int errnb;

  flowvr::xml::DOMElement* filelist;
  flowvr::xml::DOMElement* hostlist;
  flowvr::xml::DOMElement* objectlist;
  flowvr::xml::DOMElement* tracelist;
  flowvr::xml::DOMElement* eventlist;
  flowvr::xml::DOMElement* linklist;

  FileList files;
  HostMap hostmap;
  LoggerMap loggermap;
  ObjectMap objectmap;
  TraceMap tracemap;
//   ShotMap shotmap;
  EventMap eventmap;
  LinkMap linkmap;
  OwnShotList shotlist;

  std::map<int,Trace*> shotTOtrace;
  std::map<int,Host*> shotTOhost;

  double firsttime;
  double lasttime;

public:
  Parser(); // 1
  ~Parser(); // 2

  flowvr::xml::DOMElement xmlDesc(); // 25

  bool Error(); // 5
  std::string ErrorText(); // 6
  void ClearError(); // 27

  void ParseConfigFile(const char* filename); // 3
  void ParsePingFile(char* filename); // 25
  void ParseTraceFile(char* filename); // 26

  FileList GetFileList(); // 17
  HostMap GetHostMap(); // 18
  LoggerMap GetLoggerMap(); // 19
  ObjectMap GetObjectMap(); // 20
  TraceMap GetTraceMap(); // 21
//   ShotMap GetShotMap(); // 22
  EventMap GetEventMap(); // 23
  LinkMap GetLinkMap(); // 24
//   ShotList* GetShotList(); // 31

  double GetFirstTime(); // 28
  double GetLastTime(); // 29

  void ClearAll(); // 30

private:
  void err(const std::string &text); // 4

  Match* ParseMatch(flowvr::xml::DOMElement* match); // 6'
  Link* ParseLink(flowvr::xml::DOMElement* link); // 7'
  void ParseLinkList(); // 8'
  Event* ParseEvent(flowvr::xml::DOMElement* event); // 7
  void ParseEventList(); // 8
  Trace* ParseTrace(flowvr::xml::DOMElement* trace); // 9
  void ParseTraceList(); // 10
  Object* ParseObject(flowvr::xml::DOMElement* object, int lastpos); // 11
  void ParseObjectList(); // 12
  Host* ParseHost(flowvr::xml::DOMElement* host); // 13
  void ParseHostList(); // 14
  void ParseFileList(); // 15
  Display* ParseDisplay(flowvr::xml::DOMElement* drawdesc, int type); // 16

  bool ShotUsed(int id)
  {
    for(OwnShotList::iterator it=shotlist.begin();it!=shotlist.end();it++)
      if( *it == id )
        return true;

    return false;
  }
};





