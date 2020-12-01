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
* File: src/gltrace/gltrace.cpp                                   *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/
#include <sys/time.h> 
#include <gltrace.h>

using namespace flowvr;

typedef timeval trace_cycle_t;

Parser::Parser():error(false),errText("****** flowvr-gltrace's ERROR *******\n"),errnb(1)
{}

Parser::~Parser()
{
  Parser::ClearAll();
}

void Parser::ParseConfigFile(const char* filename)
{
  xml::DOMParser parser;
  parser.parse(filename);

  xml::DOMDocument* config = parser.getDocument();

  if( config->Error() )
  {
    err( std::string("Non valid config file ") + filename );
    return;
  }

  xml::DOMElement* gltrace = config->FirstChildElement("gltrace");
  if(gltrace==NULL)
  {
    err( std::string("root tag 'gltrace' not present in ") + filename);
    return;
  }

  filelist = gltrace->FirstChildElement("filelist");
  hostlist = gltrace->FirstChildElement("hostlist");
  objectlist = gltrace->FirstChildElement("objectlist");
  tracelist = gltrace->FirstChildElement("tracelist");
  eventlist = gltrace->FirstChildElement("eventlist");
  linklist = gltrace->FirstChildElement("linklist");

  if( filelist == NULL )
  {
    err( std::string("tag 'filelist' is missing after 'gltrace' in ") + filename) ;
  }
  if( hostlist == NULL )
  {
    err( std::string("tag 'hostlist' is missing after 'gltrace' in ") + filename);
  }
  if( objectlist == NULL )
  {
    err( std::string("tag 'objectlist' is missing after 'gltrace' in ") + filename);
  }
  if( tracelist == NULL )
  {
    err( std::string("tag 'tracelist' is missing after 'gltrace' in ") + filename);
  }
  if( eventlist == NULL )
  {
    err( std::string("tag 'eventlist' is missing after 'gltrace' in ") + filename);
  }
  if( linklist == NULL )
  {
    err( std::string("tag 'linklist' is missing after 'gltrace' in ") + filename);
  }

  if(error) return;


  ParseFileList();

  ParseHostList();

  ParseObjectList();

  ParseTraceList();

  ParseEventList();

  ParseLinkList();
}

void Parser::err(const std::string &text)
{
  char buff[32];
  sprintf(buff,"%d",errnb);
  error = true;
  errText += std::string("\t")+buff+std::string(" - ")+text+std::string("\n");
  errnb++;
}

bool Parser::Error()
{
  return error;
}

std::string Parser::ErrorText()
{
  return errText;
}

Match* Parser::ParseMatch(xml::DOMElement* match)
{
  if( match==NULL )  return NULL;

  Match* matchfunc = NULL;

  if( !strcmp(match->Value(), "SrcFromDest") )
    matchfunc = new Match_SrcFromDest();
  if( !strcmp(match->Value(), "DestFromSrc") )
    matchfunc = new Match_DestFromSrc();
  if( !strcmp(match->Value(), "MultipleDest") )
    matchfunc = new Match_MultipleDest();
  if( !strcmp(match->Value(), "MultipleSrc") )
    matchfunc = new Match_MultipleSrc();

  return matchfunc;
}

Link* Parser::ParseLink(xml::DOMElement* link)
{
  const char* linkname; // required
  const char* sourcename; // required
  const char* destname; //required
  xml::DOMElement* source = link->FirstChildElement("source"); // required
  xml::DOMElement* destination = link->FirstChildElement("destination"); // required
  xml::DOMElement* match = link->FirstChildElement("match"); // required
  xml::DOMElement* display = link->FirstChildElement("lnkdisplay"); // required

//   retreive event name, trace object and trace name
  if( !(linkname = link->Attribute("name")) )
  {
    err( std::string("bad attribute name in link tag" ) );
    return NULL;
  }

// check if this link already exist
  if( linkmap.find(linkname) != linkmap.end() )
  {
    err( std::string("link '")+linkname+std::string("' already define") );
    return NULL;
  }

// check if source and destination events are well define
  if( source == NULL )
  {
    err( std::string("no source tag found in link '")+linkname+std::string("'") );
    return NULL;
  }

  if( !(sourcename = source->Attribute("name")) )
  {
    err( std::string("link '")+linkname+std::string("': bad attribute name in source tag") );
    return NULL;
  }

  if( destination == NULL )
  {
    err( std::string("no destination tag found in link '")+linkname+std::string("'"));
    return NULL;
  }

  if( !(destname = destination->Attribute("name")) )
  {
    err( std::string("link '")+linkname+std::string("': bad attribute name in destination tag") );
    return NULL;
  }

// check if events source and destination exist
  if( eventmap.find( sourcename ) == eventmap.end() )
  {
    err( std::string("source event reference '")+sourcename+std::string("' not found in link '")+linkname+std::string("' defintion"));
    return NULL;
  }

  if( eventmap.find( destname ) == eventmap.end() )
  {
    err( std::string("destination event reference '")+destname+std::string("' not found in link '")+linkname+std::string("' defintion"));
    return NULL;
  }

  if( match == NULL )
  {
    err( std::string("in link '")+linkname+std::string("': no match tag found"));
    return NULL;
  }
  match = match->FirstChildElement();
  if( match == NULL )
  {
    err( std::string("in link '")+linkname+std::string("': no match func define in match tag"));
    return NULL;
  }
  Match* matchfunc = ParseMatch(match);
  if( matchfunc == NULL )
  {
    err( std::string("in link '")+linkname+std::string("': wrong match func definition"));
    return NULL;
  }

  if( display == NULL )
  {
    err( std::string("in link '")+linkname+std::string("': no display tag found"));
    return NULL;
  }

  const char* active = display->Attribute("active");
  if( !active )
  {
    err( std::string("in link '")+linkname+std::string("': no active attribute found in display tag"));
    return NULL;
  }
  if( strcmp(active,"YES") and strcmp(active,"NO") )
  {
    err( std::string("in link '")+linkname+std::string("': unvalid active attribute (")+active+std::string(") found in display tag"));
    return NULL;
  }

  std::cout << "Creating link "<<linkname<<" from "<<sourcename<<" to "<<destname<<std::endl;

// after required attribute check ok, construct new link
  Link* newlink = new Link();
  newlink->name = linkname;
  newlink->source = eventmap[sourcename];
  newlink->destination = eventmap[destname];
  newlink->matchfunc = matchfunc;

// parse display tag
  xml::DOMElement* drawdesc = display->FirstChildElement();
  if( !strcmp(active,"YES") and drawdesc!=NULL )
  {
    newlink->display = true;
  }
  else
  {
    newlink->display = false;
    Display* newdraw = new Display_Null();
    newlink->drawfunc.push_back(newdraw);
  }

  if( !newlink->display )
    return newlink;

// if display is set to active, parse display function list
  while(true)
  {
    if(drawdesc==NULL) break;

    Display* newdraw = ParseDisplay(drawdesc,LINK);
    newlink->drawfunc.push_back(newdraw);
    drawdesc = drawdesc->NextSiblingElement();
  }
  return newlink;
}

void Parser::ParseLinkList()
{
  xml::DOMElement* link = linklist->FirstChildElement("link");

  while(true)
  {
    if(link==NULL) break;

    Link* newlink = ParseLink(link);
    if( newlink != NULL )
      linkmap[newlink->name] = newlink;

    link = link->NextSiblingElement("link");
  }
}

Event* Parser::ParseEvent(xml::DOMElement* event)
{
  const char* eventname;  //  <event name="required">
  const char* tracename;  //    <trace object="required" name="required"/>
  const char* traceobject;//  </event>
  xml::DOMElement* display = event->FirstChildElement("evtdisplay"); // required

//   retreive event name, trace object and trace name
  if( !(eventname = event->Attribute("name")) )
  {
    err( std::string("bad attribute name in event tag") );
    return NULL;
  }

  xml::DOMElement* trace = event->FirstChildElement("trace");
  if( trace == NULL )
  {
    err( std::string("no trace tag found in event '")+eventname+std::string("'"));
    return NULL;
  }

  if( !(tracename = trace->Attribute("name")) )
  {
    err( std::string("event '")+eventname+std::string("': bad attibute name in trace tag") );
    return NULL;
  }

  if( !(traceobject = trace->Attribute("object")) )
  {
    err( std::string("event '")+eventname+std::string("': bad attibute object in trace tag") );
    return NULL;
  }

// trace id is "object:name"
  std::string traceid = std::string(traceobject + std::string(":") + tracename);
// check if this trace is define
  if( tracemap.find( traceid ) == tracemap.end() )
  {
    err( std::string("trace reference '")+traceid+std::string("' not found in event '")+eventname+std::string("' defintion"));
    return NULL;
  }
// check if this event already exist
  if( eventmap.find(eventname) != eventmap.end() )
  {
    err( std::string("event '")+eventname+std::string("' already defined"));
    return NULL;
  }

  if( display == NULL )
  {
    err( std::string("in event '")+eventname+std::string("': no display tag found"));
    return NULL;
  }

  const char* active = display->Attribute("active");
  if( !active )
  {
    err( std::string("in event '")+eventname+std::string("': no active attribute found in display tag"));
    return NULL;
  }

  if( strcmp(active,"YES") and strcmp(active,"NO") )
  {
    err( std::string("in event '")+eventname+std::string("': unvalid active attribute (")+active+std::string(") found in display tag"));
    return NULL;
  }


// after required attribute check ok, construct new event
  Event* newevent = new Event();
  newevent->name = eventname;
  newevent->tracename = traceid;
  newevent->trace = tracemap[traceid];

// check the xml shot list into trace
  std::vector<int> ownedshotlist;
  xml::DOMElement* shot = trace->FirstChildElement("shot");
// if shot list, retreive all trace shots
  if( shot == NULL )
  {
    newevent->ownedshot = tracemap[traceid]->shotlist;
  }
// if shot list, parse it
  else
  {
    while(true)
    {
      if( shot == NULL ) break;

      const char* shotid;
      int id;

      if( !(shotid = shot->Attribute("id")) )
      {
        err( std::string("event '")+eventname+std::string("', trace '")+traceid+std::string("': bad shot id attribute"));
      }
      else if( (id = atoi(shotid)) == 0 )
      {
        err( std::string("event '")+eventname+std::string("', trace '")+traceid+std::string("': bad shot id attribute"));
      }
      else if( !newevent->trace->own(id) )
      {
        err( std::string("shot id = ")+shot->Attribute("id")+std::string(" not below to trace '")+traceid+std::string("'"));
      }
      else
      {
        newevent->ownedshot.push_back(id);  // add to event shot list
      }

      shot = shot->NextSiblingElement("shot");
    }
  }

// parse display tag
  xml::DOMElement* drawdesc = display->FirstChildElement();
  if( !strcmp(active,"YES") and drawdesc!=NULL )
  {
    newevent->display = true;
  }
  else
  {
    newevent->display = false;
    Display* newdraw = new Display_Null();
    newevent->drawfunc.push_back(newdraw);
  }

  if( !newevent->display )
    return newevent;
// if display is set to active, parse display function list

  while(true)
  {
    if(drawdesc==NULL) break;

    Display* newdraw = ParseDisplay(drawdesc,EVENT);
    newevent->drawfunc.push_back(newdraw);
    drawdesc = drawdesc->NextSiblingElement();
  }
  return newevent;
}

void Parser::ParseEventList()
{
  xml::DOMElement* event = eventlist->FirstChildElement("event");

  while(true)
  {
    if(event==NULL) break;

    Event* newevent = ParseEvent(event);
    if( newevent != NULL )
      eventmap[newevent->name] = newevent;

    event = event->NextSiblingElement("event");
  }
}

Trace* Parser::ParseTrace(xml::DOMElement* trace)
{
  const char* traceobject = trace->Attribute("object"); // required
  const char* tracename = trace->Attribute("name");  // required

  if( !traceobject or !tracename )
  {
    err( std::string("bad attribute object or name in trace definition tag") );
    return NULL;
  }

// trace must contain a shot list
  xml::DOMElement* shot = trace->FirstChildElement("shot");
  if(shot==NULL)
  {
    err( std::string("trace '")+tracename+std::string("' containd no shot definition"));
    return NULL;
  }

// trace is define by is name and its traced object
  std::string traceid = std::string(traceobject)+std::string(":")+std::string(tracename);

// check if this trace is already define
  if( tracemap.find(traceid) != tracemap.end() )
  {
    err( std::string("trace '")+traceid+std::string("' already defined"));
    return NULL;
  }

// check if the traced object is define
  if( objectmap.find(traceobject) == objectmap.end() )
  {
    err( std::string("object '")+traceobject+std::string("' not found in trace '")+traceid+std::string("' definition"));
    return NULL;
  }

// all checks are good, create trace
  Trace* newtrace = new Trace();
  newtrace->objectname = traceobject;
  newtrace->name = tracename;
  newtrace->object = objectmap[traceobject];
  newtrace->host = newtrace->object->host;

// parse shot list
  while(true)
  {
    if( shot == NULL ) break;

    const char* shotid;
    int id;
    if( !(shotid = shot->Attribute("id")) )
    {
      err( std::string("in trace '")+traceid+std::string("': bad shot id"));
    }
    else if( (id = atoi(shotid)) == 0 )
    {
      err( std::string("in trace '")+traceid+std::string("': bad shot id"));
    }
    else if( ShotUsed(id) )
    {
      err( std::string("in trace '")+traceid+std::string("': shot id = ")+shotid+std::string(" already defined"));
    }
    else
    {
      newtrace->shotlist.push_back(id);
    }

    shot = shot->NextSiblingElement("shot");
  }

  if( newtrace->shotlist.empty() )
  {
    err( std::string("in trace '")+traceid+std::string("': no valid shot defined"));
    delete newtrace;
    return NULL;
  }

  return newtrace;
}

void Parser::ParseTraceList()
{
  xml::DOMElement* trace = tracelist->FirstChildElement("trace");
  if(trace==NULL)
  {
    err( std::string("tracelist must contain at least one trace definition") );
    return;
  }
// parse trace list
  while(true)
  {
    if(trace==NULL) break;

    Trace* newtrace = ParseTrace(trace);

    if (newtrace != NULL)
    {
      // add trace to define trace map
      tracemap[ std::string(newtrace->objectname)+std::string(":")+std::string(newtrace->name) ] = newtrace;

      // add its shot to the define shot list
      OwnShotList tmp = newtrace->shotlist;
      shotlist.merge(tmp);

      for(OwnShotList::iterator it=newtrace->shotlist.begin();it!=newtrace->shotlist.end();it++)
      {
        shotTOtrace[ *it ] = newtrace;
        shotTOhost[ *it ] = newtrace->host;
      }
    }

    trace = trace->NextSiblingElement("trace");
  }
}

Object* Parser::ParseObject(xml::DOMElement* object, int lastpos)
{
  const char* name = object->Attribute("id"); // required
  const char* host = object->Attribute("host"); // required
  xml::DOMElement* display = object->FirstChildElement("objdisplay"); // required

  // check required attribute
  if( !name or !host )
  {
    err( std::string("bad object attributes 'name' or 'host'") );
    return NULL;
  }

  // check if this object is already define
  if( objectmap.find(name) != objectmap.end() )
  {
    err( std::string("object '")+name+std::string("' already defined"));
    return NULL;
  }

  // check if the host exist
  if( hostmap.find(host) == hostmap.end() )
  {
    err( std::string("in object '")+name+std::string("': host '")+host+std::string("' not found") );
    return NULL;
  }

  if( display == NULL )
  {
    err( std::string("in object '")+name+std::string("': no display tag found"));
    return NULL;
  }

  const char* active = display->Attribute("active");
  if( !active )
  {
    err( std::string("in object '")+name+std::string("': no active attribute found in display tag"));
    return NULL;
  }
  if( strcmp(active,"YES") and strcmp(active,"NO") )
  {
    err( std::string("in object '")+name+std::string("': invalid active attribute (")+active+std::string(") found in display tag"));
    return NULL;
  }

  // all checks are good
  Object* newobject = new Object();
  newobject->name = object->Attribute("id");
  newobject->hostname = object->Attribute("host");

  // parse display tag
  const char* position = display->Attribute("pos"); // implied

  int pos;
  if(!position)
    pos = lastpos + 1;
  else
    pos = atoi(position);

  xml::DOMElement* drawdesc = display->FirstChildElement();
  if( !strcmp(active,"YES") and drawdesc!=NULL )
  {
    newobject->display = true;
    newobject->pos = pos;
  }
  else
  {
    newobject->display = false;
    Display* newdraw = new Display_Null();
    newobject->drawfunc.push_back(newdraw);
  }

  if( !active )
    return newobject;

  while(true)
  {
    if(drawdesc==NULL) break;

    Display* newdraw = ParseDisplay(drawdesc,OBJECT);
    newobject->drawfunc.push_back(newdraw);
    drawdesc = drawdesc->NextSiblingElement();
  }

  return newobject;
}

void Parser::ParseObjectList()
{
  xml::DOMElement* object = objectlist->FirstChildElement();
  if(object == NULL)
  {
    err( std::string("objectlist must contain at least one object definition") );
    return;
  }

  int lastpos=-1;
  while(true)
  {
    if( object==NULL ) break;

    Object* newobject = ParseObject(object, lastpos);
    if( newobject != NULL )
    {
      newobject->host = hostmap[newobject->hostname];

      if( newobject->display and (newobject->pos > lastpos) )
        lastpos = newobject->pos;

      objectmap[newobject->name] = newobject;
    }

    object = object->NextSiblingElement();
  }
}

Host* Parser::ParseHost(xml::DOMElement* host)
{
  const char* id = host->Attribute("id"); // required
  const char* method = host->Attribute("method"); // required
  xml::DOMElement* logger = host->FirstChildElement("logger"); // required

  if(!id or !method)
  {
    err( std::string("bad host attribute 'id' or 'method'") );
    return NULL;
  }

  if(logger == NULL)
  {
    err( std::string(" in host '")+id+std::string("': no logger definition found'"));
    return NULL;
  }

  if( hostmap.find(id) != hostmap.end() )
  {
    err( std::string("host '")+id+std::string("' already defined"));
    return NULL;
  }

  Host* newhost;

  if( !strcmp(method,"FASTEST") )
  {
    newhost = new Host_Fastest(id);
  }
  else if( !strcmp(method,"AVERAGE") )
  {
    newhost = new Host_Average(id);
  }
  else
  {
    err( std::string("host '")+id+std::string("', method definition '")+method+std::string("' is not valid") );
    return NULL;
  }

  while(true)
  {
    if(logger==NULL) break;

    const char* log = logger->Attribute("id");

    if(!log)
    {
      err( std::string("in host '")+id+std::string("': bad logger attribute"));
    }
    else if( loggermap.find(log) != loggermap.end() )
    {
      err( std::string("in host '")+id+std::string("': logger '")+log+std::string("' already defined"));
    }
    else
    {
      newhost->loggerlist.push_back(log);
      loggermap[log] = newhost;
    }

    logger = logger->NextSiblingElement("logger");
  }

  if(newhost->loggerlist.empty() )
  {
    err( std::string("in host ')")+id+std::string("': no valid logger defined"));
    delete newhost;
    return NULL;
  }

  return newhost;
}

void Parser::ParseHostList()
{
  xml::DOMElement* host = hostlist->FirstChildElement("host");
  if(host==NULL)
  {
    err( std::string("hostlist must contain at least one host definition\n") );
    return;
  }

  while(true)
  {
    if(host==NULL) break;

    Host* newhost = ParseHost(host);

    if(newhost!=NULL)
    {
      hostmap[newhost->name] = newhost;
    }

    host = host->NextSiblingElement("host");
  }
}

void Parser::ParseFileList()
{
  xml::DOMElement* pingresults = filelist->FirstChildElement("pingresults");
  if( pingresults == NULL )
  {
    err( std::string("no ping-results file define in 'filelist' tag") );
    return;
  }

  files.push_back( pingresults->Attribute("file") );
  if( files.back().empty() )
  {
    err( std::string("bad attribute 'file' in ping-results definition tag") );
    return;
  }

  xml::DOMElement* tracefile = filelist->FirstChildElement("tracefile");
  if( tracefile == NULL )
  {
    err( std::string("no tracefile defined in 'filelist' tag") );
    return;
  }

  while(true)
  {
    if( tracefile == NULL ) break;

    files.push_back( tracefile->Attribute("file") );
    if( files.back().empty() )
    {
      err( std::string("bad attribute 'file' in trace file definition tag") );
      return;
    }

    tracefile = tracefile->NextSiblingElement("tracefile");
  }
}

Display* Parser::ParseDisplay(xml::DOMElement* drawdesc, int type)
{
  bool object = ( type == OBJECT );
  bool event = ( type == EVENT );
  bool link = ( type == LINK );

  const char* val = drawdesc->Value();
  Display* newdraw;

  if( !strcmp(val,"null") )
  {
    newdraw = new Display_Null();
  }
  if( !strcmp(val,"objline") and object )
  {
    newdraw = new Display_Object_Line(drawdesc);
  }
  else if( !strcmp(val,"objtext") and object )
  {
    newdraw = new Display_Object_Text(drawdesc);
  }
  else if( !strcmp(val,"evtline") and event )
  {
    newdraw = new Display_Event_Line(drawdesc);
  }
  else if( !strcmp(val,"objline") and link )
  {
    newdraw = new Display_Link_ObjLine(drawdesc);
  }
  else if( !strcmp(val,"lnkline") and link )
  {
    newdraw = new Display_Link_Line(drawdesc);
  }
  else if( object )
  {
    newdraw = new Display_Null();
    err( std::string("'")+val+std::string("' is not a valid object draw function" ) );
  }
  else if( event )
  {
    newdraw = new Display_Null();
    err( std::string("'") +val+std::string("' is not a valid event draw function"));
  }
  else if( link )
  {
    newdraw = new Display_Null();
    err( std::string("'")+val+std::string("' is not a valid link draw function"));
  }
  else
  {
    newdraw = new Display_Null();
    err( std::string("Internal error: undefined type '") );
  }

  return newdraw;
}

FileList Parser::GetFileList()
{
  return files;
}

HostMap Parser::GetHostMap()
{
  return hostmap;
}

LoggerMap Parser::GetLoggerMap()
{
  return loggermap;
}

ObjectMap Parser::GetObjectMap()
{
  return objectmap;
}

TraceMap Parser::GetTraceMap()
{
  return tracemap;
}

EventMap Parser::GetEventMap()
{
  return eventmap;
}

LinkMap Parser::GetLinkMap()
{
  return linkmap;
}

xml::DOMElement Parser::xmlDesc()
{
  xml::DOMElement gltrace = xml::DOMElement("gltrace");

  xml::DOMElement file = xml::DOMElement("filelist");
  xml::DOMElement host = xml::DOMElement("hostlist");
  xml::DOMElement object = xml::DOMElement("objectlist");
  xml::DOMElement trace = xml::DOMElement("tracelist");
  xml::DOMElement event = xml::DOMElement("eventlist");
  xml::DOMElement link = xml::DOMElement("linklist");

  xml::DOMElement tmp = xml::DOMElement("pingresults");
  tmp.SetAttribute("file",files[0]);
  file.InsertEndChild(tmp);
  for(unsigned int i=1;i<files.size();i++)
  {
    xml::DOMElement tmp = xml::DOMElement("tracefile");
    tmp.SetAttribute("file",files[i]);
    file.InsertEndChild(tmp);
  }


  for(HostMap::iterator it=hostmap.begin();it!=hostmap.end();it++)
    host.InsertEndChild(it->second->xmlDesc());


  for(ObjectMap::iterator it=objectmap.begin();it!=objectmap.end();it++)
    object.InsertEndChild(it->second->xmlDesc());


  for(TraceMap::iterator it=tracemap.begin();it!=tracemap.end();it++)
    trace.InsertEndChild(it->second->xmlDesc());


  for(EventMap::iterator it=eventmap.begin();it!=eventmap.end();it++)
    event.InsertEndChild(it->second->xmlDesc());

  for(LinkMap::iterator it=linkmap.begin();it!=linkmap.end();it++)
    link.InsertEndChild(it->second->xmlDesc());

  gltrace.InsertEndChild(file);
  gltrace.InsertEndChild(host);
  gltrace.InsertEndChild(object);
  gltrace.InsertEndChild(trace);
  gltrace.InsertEndChild(event);
  gltrace.InsertEndChild(link);

  return gltrace;
}

void Parser::ParsePingFile(char* filename)
{
  std::ifstream file;
  file.open(filename,std::ios::in);
  if(!file.is_open())
  {
    err( std::string("Can't open the ping result file '")+filename+std::string("'"));
    return;
  }

  firsttime = 0;
  lasttime = 0;

  std::string line;
  while (getline(file,line))
  {
    xml::DOMParser parser;
    if (parser.parseString(line.c_str()))
    {
      err( std::string("(")+filename+std::string(") parsing line: ")+line);
      file.close();
      return;
    }

    xml::DOMElement* result = parser.getDocument()->RootElement()->FirstChildElement();
    if (result == NULL)
    {
    }
    else if( !strcmp(result->getNodeName(),"ping") )
    {
      xml::DOMElement* send = result->FirstChildElement("send");
      xml::DOMElement* reply = result->FirstChildElement("reply");
      xml::DOMElement* receive = result->FirstChildElement("receive");

      if(send!=NULL && reply!=NULL && receive!=NULL)
      {
        int id = atoi( result->Attribute("id") );
        const char* logname = result->Attribute("logger");

        PingInfo newping;

        newping.reply = atoll( reply->Attribute("cycle") );
        newping.send = atof( send->Attribute("sec") ) + ( atof( send->Attribute("usec") ) / 1000000 );
        newping.receive = atof( receive->Attribute("sec") ) + ( atof( receive->Attribute("usec") ) / 1000000 );

        if( firsttime == 0 ) firsttime = newping.send;

        newping.send -= firsttime;
        newping.receive -= firsttime;

        if( loggermap.find(logname) == loggermap.end() )
        {
          err( std::string("ping results: undeclared logger : ")+logname);
          file.close();
          return;
        }
        else
        {
          loggermap[logname]->addPing(id,newping);
        }
      }
    }
  }
  file.close();

  for(HostMap::iterator it=hostmap.begin();it!=hostmap.end();it++)
    it->second->calculateCorrespondance();

  return;
}

void Parser::ParseTraceFile(char* filename)
{
  std::ifstream file;

  file.open(filename,std::ios::in|std::ios::binary);
  if(!file.is_open())
  {
    err( std::string("Can't open trace file '")+filename+std::string("'"));
    return;
  }

  int size;
  int id;
  trace_cycle_t cycle;
  int val;

  double firsttime = -1.0f;

  while(file.good())
  {
    id = 0; size = 0; val = 0;


    file.read((char*)&size,sizeof(unsigned int));
    file.read((char*)&cycle,sizeof(trace_cycle_t));
    file.read((char*)&id,4);
    size -= (sizeof(unsigned int) + sizeof(trace_cycle_t) + 4);
    if(size > 0)
    {
       file.read((char*)&val,size);
    }
    else
    {
        val = 0;
    }
    //std::cout << "size = " << size << " cycle = " << cycle.tv_sec << " sec " << cycle.tv_usec << " usec id = " << id << " val = " << val << std::endl;

    if(id==0)
    {
      file.close();
      return;
    }




    if( !ShotUsed(id) )
    {
      char buff[32];
      sprintf(buff,"%d",id);
      std::cerr<< "trace file: id=" << buff << " match with no define shot"  << std::endl;
      file.close();
      return;
    }
    else
    {
//    double time = shotTOhost[id]->cycleToTime(cycle);
   double time = ((double) cycle.tv_sec) + ((double) (cycle.tv_usec) / 1000000.0); 
    if(firsttime < 0)
    {
        firsttime = time;
    }

   time -= firsttime; 

    ShotInfo shotinfo;
    shotinfo.time = time;
    shotinfo.val = val;

    shotinfo.pos = shotTOtrace[id]->object->pos;

    for(EventMap::iterator it=eventmap.begin();it!=eventmap.end();it++)
      if(it->second->own(id))
        it->second->shotlist.push_back(shotinfo);

    }
  }
  file.close();

  return;
}

void Parser::ClearError()
{
  error = false;
  errText = "";
  errnb = 1;
}

double Parser::GetFirstTime()
{
  double res = 0;
  double tmp = res;

  for(EventMap::iterator it=eventmap.begin();it!=eventmap.end();it++)
  {
    tmp = it->second->getFirstEvent();
    if( (tmp < res) or (res == 0) )
      res = tmp;
  }

  return res;
}

double Parser::GetLastTime()
{
  double res = 0;
  double tmp = res;

  for(EventMap::iterator it=eventmap.begin();it!=eventmap.end();it++)
  {
    tmp = it->second->getLastEvent();
    if( (tmp > res) or (res == 0) )
      res = tmp;
  }

  return res;
}

void Parser::ClearAll()
{
  files.clear();

  for(HostMap::iterator it=hostmap.begin();it!=hostmap.end();it++)
    delete it->second;

  hostmap.clear();
  loggermap.clear();

  for(ObjectMap::iterator it=objectmap.begin();it!=objectmap.end();it++)
    delete it->second;

  objectmap.clear();

  for(TraceMap::iterator it=tracemap.begin();it!=tracemap.end();it++)
    delete it->second;

  tracemap.clear();

  for(EventMap::iterator it=eventmap.begin();it!=eventmap.end();it++)
    delete it->second;

  eventmap.clear();

  for(LinkMap::iterator it=linkmap.begin();it!=linkmap.end();it++)
    delete it->second;

  linkmap.clear();
}

