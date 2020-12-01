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
* File: include/flowvr/gltrace/match.h                            *
*                                                                 *
* Contacts:                                                       *
*  08/2004 Julien Garand                                          *
*                                                                 *
******************************************************************/

class Match
{
public:

  virtual bool init(Event* src, Event* dest)=0;

  virtual bool end()=0;

  virtual bool match()=0;

  virtual ShotInfo& getSrc()=0;

  virtual ShotInfo& getDest()=0;

  virtual flowvr::xml::DOMElement xmlDesc()=0;

  Match()
  {
  }

  virtual ~Match()
  {
  }
};


class Match_SrcFromDest: public Match
{
  ShotList::iterator it_src,
                     it_dest,
                     it_src_end,
                     it_dest_end,
                     tmp;

  bool first;

public:
  virtual bool init(Event* src, Event* dest)
  {
    it_src = src->shotlist.begin();
    it_dest = dest->shotlist.begin();
    it_src_end = src->shotlist.end();
    it_dest_end = dest->shotlist.end();

    first = true;

    return !end();
  }

  virtual bool end()
  {
    return ( (it_src == it_src_end) or (it_dest == it_dest_end) );
  }

  virtual bool match()
  {
    if(first)
      first = false;
    else
      it_dest++;


    if( it_dest == it_dest_end ) return false;

    while( (it_src != it_src_end) and (it_src->val < it_dest->val) ) it_src++;
    /*
    while(true)
    {
      tmp = it_src;
      tmp++;
      if( (tmp != it_src_end) and (tmp->val == it_dest->val) and (tmp->time < it_dest->time) )
        it_src = tmp;
      else
        break;
    }
    */
    return (it_src != it_src_end) && (it_src->val == it_dest->val);
  }

  virtual ShotInfo& getSrc()
  {
    return *it_src;
  }

  virtual ShotInfo& getDest()
  {
    return *it_dest;
  }

  virtual flowvr::xml::DOMElement xmlDesc()
  {
    return flowvr::xml::DOMElement("SrcFromDest");
  }

  Match_SrcFromDest()
  {
  }

  virtual ~Match_SrcFromDest()
  {
  }
};


class Match_DestFromSrc: public Match
{
  ShotList::iterator it_src,
                     it_dest,
                     it_src_end,
                     it_dest_end,
                     tmp;

  bool first;

public:
  virtual bool init(Event* src, Event* dest)
  {
    it_src = src->shotlist.begin();
    it_dest = dest->shotlist.begin();
    it_src_end = src->shotlist.end();
    it_dest_end = dest->shotlist.end();

    first = true;

    return !end();
  }

  virtual bool end()
  {
    return ( (it_src == it_src_end) or (it_dest == it_dest_end) );
  }

  virtual bool match()
  {
    if(first)
      first = false;
    else
      it_src++;


    if( it_src == it_src_end ) return false;

    while( (it_dest != it_dest_end) and (it_src->val != it_dest->val) ) it_dest++;

    while(true)
    {
      tmp = it_dest;
      tmp++;
      if( (tmp != it_dest_end) and (tmp->val == it_src->val) and (tmp->time < it_src->time) )
        it_dest = tmp;
      else
        break;
    }

    return (it_dest != it_dest_end);
  }

  virtual ShotInfo& getSrc()
  {
    return *it_src;
  }

  virtual ShotInfo& getDest()
  {
    return *it_dest;
  }

  virtual flowvr::xml::DOMElement xmlDesc()
  {
    return flowvr::xml::DOMElement("DestFromSrc");
  }

  Match_DestFromSrc()
  {
  }

  virtual ~Match_DestFromSrc()
  {
  }
};


class Match_MultipleDest: public Match
{
  ShotList::iterator it_src,
                     it_dest,
                     it_src_end,
                     it_dest_end,
                     tmp;

  bool first;

public:
  virtual bool init(Event* src, Event* dest)
  {
    it_src = src->shotlist.begin();
    it_dest = dest->shotlist.begin();
    it_src_end = src->shotlist.end();
    it_dest_end = dest->shotlist.end();

    first = true;

    return !end();
  }

  virtual bool end()
  {
    return ( (it_src == it_src_end) or (it_dest == it_dest_end) );
  }

  virtual bool match()
  {
    if(first)
      first = false;
    else
    {
      it_dest++;
      while( (it_dest->val != it_src->val) and !end() )
        it_src++;
    }

    if(end()) return false;

    return true;
  }

  virtual ShotInfo& getSrc()
  {
    return *it_src;
  }

  virtual ShotInfo& getDest()
  {
    return *it_dest;
  }

  virtual flowvr::xml::DOMElement xmlDesc()
  {
    return flowvr::xml::DOMElement("MultipleDest");
  }

  Match_MultipleDest()
  {
  }

  virtual ~Match_MultipleDest()
  {
  }
};


class Match_MultipleSrc: public Match
{
  ShotList::iterator it_src,
                     it_dest,
                     it_src_end,
                     it_dest_end;

  bool first;

public:
  virtual bool init(Event* src, Event* dest)
  {
    it_src = src->shotlist.begin();
    it_dest = dest->shotlist.begin();
    it_src_end = src->shotlist.end();
    it_dest_end = dest->shotlist.end();

    first = true;

    return !end();
  }

  virtual bool end()
  {
    return ( (it_src == it_src_end) or (it_dest == it_dest_end) );
  }

  virtual bool match()
  {
     if(first)
      first = false;
    else
    {
      it_src++;
      while( (it_dest->val != it_src->val) and !end() )
        it_dest++;
    }

    if(end()) return false;

    return true;
  }

  virtual ShotInfo& getSrc()
  {
    return *it_src;
  }

  virtual ShotInfo& getDest()
  {
    return *it_dest;
  }

  virtual flowvr::xml::DOMElement xmlDesc()
  {
    return flowvr::xml::DOMElement("MultipleSrc");
  }

  Match_MultipleSrc()
  {
  }

  virtual ~Match_MultipleSrc()
  {
  }
};
