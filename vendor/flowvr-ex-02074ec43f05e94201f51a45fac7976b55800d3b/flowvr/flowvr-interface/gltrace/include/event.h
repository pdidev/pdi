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
* File: include/flowvr/gltrace/event.h                            *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/

class Event
{
public:
  std::string name;
  std::string tracename;
  bool display;
  std::vector<Display*> drawfunc;

  OwnShotList ownedshot;

  Trace* trace;

  ShotList shotlist;

// private:
  bool own(int shotID)
  {
    for(OwnShotList::iterator it=ownedshot.begin();it!=ownedshot.end();it++)
      if( *it == shotID )
        return true;

    return false;
  }
// public:
  void draw()
  {
    if( (!display) or (!trace->object->display) )
      return;

    for(ShotList::iterator it=shotlist.begin();it!=shotlist.end();it++)
    {
      for(unsigned int i=0;i<drawfunc.size();i++)
      {
        drawfunc[i]->draw( *it );
      }
    }
  }

  flowvr::xml::DOMElement xmlDesc()
  {
    flowvr::xml::DOMElement res=flowvr::xml::DOMElement("event");

    res.SetAttribute("name",name);

    res.SetAttribute("eventNb",shotlist.size());

    res.InsertEndChild(trace->xmlDesc());

    flowvr::xml::DOMElement disp = flowvr::xml::DOMElement("evtdisplay");
    if(display)
    {
      disp.SetAttribute("active","YES");

      for(unsigned int i=0; i<drawfunc.size(); i++)
        disp.InsertEndChild(drawfunc[i]->xmlDesc());
    }
    else
    {
      disp.SetAttribute("active","NO");
    }
    res.InsertEndChild(disp);

    return res;
  }

  double getFirstEvent()
  {
    shotlist.sort();
    return shotlist.front().time;
  }

  double getLastEvent()
  {
    if( shotlist.empty() ) return 0;

    shotlist.sort();

    return shotlist.back().time;
  }

  Event()
  {
  }

  ~Event()
  {
    for(unsigned int i=0; i<drawfunc.size(); i++)
      delete drawfunc[i];

    shotlist.clear();

    ownedshot.clear();
  };
};
