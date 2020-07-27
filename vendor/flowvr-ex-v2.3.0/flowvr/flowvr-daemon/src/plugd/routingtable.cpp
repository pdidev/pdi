/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Daemon and Base Plugins                     *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA and                                                       *
* Laboratoire d'Informatique Fondamentale d'Orleans               *
* (FRE 2490) ALL RIGHTS RESERVED.                                 *
*                                                                 *
* This source is covered by the GNU GPL, please refer to the      *
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
*    Bruno Raffin,                                                *
*    Sophie Robert,                                               *
*    Emmanuel Melin.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: src/plugd/routingtable.cpp                                *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/plugd/routingtable.h"
#include "flowvr/plugd/actionhandler.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/plugd/object.h"
#include "flowvr/ipc/atomic.h"

namespace flowvr
{

namespace plugd
{

Action::Action(ActionHandler* _handler, int _flags)
  : handler(_handler), flags(_flags)
{
  handlerref = new ipc::MPAtomicInt();
  handlerref->set(1);
  count=0;
}

Action::Action(const Action& clone)
  : handler(clone.handler), flags(clone.flags), handlerref(clone.handlerref)
{
  handlerref->inc();
  count=clone.count;
}

Action::~Action()
{
  if (handlerref->dec_and_test_null())
    handler->remove();
}

void Action::operator=(const Action& clone)
{
  if (handlerref!=clone.handlerref)
  {
    if (handlerref->dec_and_test_null())
      handler->remove();
    handlerref = clone.handlerref;
  }
  handler = clone.handler;
  flags = clone.flags;

  count=clone.count;

}

void Action::doIt(const Message& m, Dispatcher* dispatcher)
{
  if ((flags&STAMPSONLY) && m.data.valid())
  {
    Message m2;
    m2.stamps=m.stamps;
    handler->doIt(m2,dispatcher);
  }
  else
  {
    // Special case: if the action is a redispatch, ignore it
    // (would create an infinite loop...)
    if (handler != ActionHandler::LoopBackActionHandler)
      handler->doIt(m,dispatcher);
  }
}

void Action::inc()
{
  count++;
}

int Action::getCount()
{
  return count;
}

RoutingTable::RoutingTable()
  : routesLock("flowvr.plugd.RoutingTable.routesLock")
{
}

RoutingTable::~RoutingTable()
{
}

bool RoutingTable::addRoute(const std::string& id, const std::string& source, Message::Type msgtype, Action* action)
{
  ipc::ScopedMTLock locker(routesLock,"addRoute");

  IDTable::iterator it = routeFromID.find(id);
  if (it!=routeFromID.end())
    return false; // action ID already exists

  routeFromID.insert(IDTable::value_type(id,Route(source,msgtype,action)));
  
  //routeFromSource[msgtype].insert(SourceTable::value_type(source,action));
  SourceIterator sit = routeFromSource[msgtype].find(source);
  //if at least one action is already linked to this source
  if (sit!=routeFromSource[msgtype].end())
    {
      std::vector<Action*>* vec = sit->second;
      vec->push_back(action); //add the new Action* to the vector
      }
  else
    {
      std::vector<Action*>* vec = new std::vector<Action*>; //create a new vector
      vec->push_back(action); //add the Action*
      routeFromSource[msgtype].insert(SourceTable::value_type(source,vec)); //insert its address in the map
    }
  return true;
}

const std::vector<Action*>* RoutingTable::getAction(const std::string &source,Message::Type msgtype)
{
  SourceIterator sit = routeFromSource[msgtype].find(source);
  if (sit!=routeFromSource[msgtype].end())
  {
    return sit->second;
  }
  else 
  {
    return NULL;
  }
}

Result RoutingTable::getRouteCount(const std::string& id)
{

  ipc::ScopedMTLock locker(routesLock,"CountRoute");
  
  IDTable::iterator it = routeFromID.find(id);
  
  if (it==routeFromID.end())
    {
      return Result(Result::ERROR,"ID not found"); // action ID not found
    }

  Route& route = it->second;

  xml::DOMElement* rep = new xml::DOMElement("messagesent");
  rep->SetAttribute("source",route.source);
  rep->SetAttribute("count",route.action->getCount());

  Result reply(Result::OK,rep);

  
  return reply;
}



bool RoutingTable::removeRoute(const std::string& id)
{
  ipc::ScopedMTLock locker(routesLock,"removeRoute");

  IDTable::iterator it = routeFromID.find(id);
  if (it==routeFromID.end())
    return false; // action ID not found

  Route& route = it->second;

  //find the element in the map containing the source to delete
  SourceIterator sit = routeFromSource[route.msgtype].find(route.source);

  std::vector<Action*>* vec = sit->second;
  std::vector<Action*>::iterator vit = vec->begin();
  while (!((*vit)==route.action) && vit!=vec->end())
    vit++;
  if (vit == vec->end())
    return false; // error
  delete *vit;
  vec->erase(vit);
  
  //if the action deleted was the last for this source then the source is erased from the map
  if (vec->size()==0)
  {
    routeFromSource[route.msgtype].erase(sit);
    delete vec;
  }

  routeFromID.erase(it);

  return true;

}

std::vector<std::string> RoutingTable::getListID(const std::string& id) const
{
  std::vector<std::string> listid;
  IDTable::const_iterator it = routeFromID.begin();

  while (it!=routeFromID.end())
    {
      std::string idroute = it->first;
      
      if (!strncmp(id.c_str(),idroute.c_str(),id.size()))
      {
	listid.push_back(idroute);
      }
      it++;
    }

  return listid;

}

} // namespace plugd

} // namespace flowvr

