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
* File: include/flowvr/plugd/routingtable.h                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_ROUTINGTABLE_H
#define FLOWVR_PLUGD_ROUTINGTABLE_H

#include "flowvr/message.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/xml.h"
#include <map>
#include <vector>


namespace flowvr
{

namespace ipc
{
class MPAtomicInt;
}

namespace plugd
{

class ActionHandler;
class Dispatcher;
class Result;

/// Action is a reference-counted data structure containing the parameters of
/// an action handling.
class Action
{
 public:
  Action(ActionHandler* _handler, int _flags=0);
  Action(const Action& clone);
  ~Action();
  void operator=(const Action& clone);

  void doIt(const Message& m, Dispatcher* dispatcher);

  void inc();

  int getCount();

  enum Flag
  {
    STAMPSONLY = 1<<0, // Only transmit message's stamps and no data
    DEFERHANDLER = 1<<1, // Reserved for future use
  };
 protected:
  ActionHandler* handler;
  int flags;
  ipc::MPAtomicInt* handlerref;
  //modification Lo�ck
  ipc::MPAtomicInt count;
};

/// A routing table store the actions to take for each source
class RoutingTable
{
 public:
  RoutingTable();
  ~RoutingTable();

  /// List of routes: For each source: the pointor to the list of actions to be taken
  typedef std::map<std::string,std::vector<Action*>*> SourceTable;
  /// Map the routes from their ID
  struct Route
  {
    std::string source;
    Message::Type msgtype;
    Action* action;
    Route(const std::string& _source, Message::Type _msgtype, Action* _action)
      : source(_source), msgtype(_msgtype), action(_action)
    {
    }
  };

  typedef std::map<std::string,Route> IDTable;

  bool addRoute(const std::string& id, const std::string& source, Message::Type msgtype, Action* action);
  bool removeRoute(const std::string& id);
  ipc::MTLock& lockIterate()
  {
    return routesLock;
  }

  typedef SourceTable::iterator SourceIterator;

  SourceIterator sourceActionsBegin(const std::string& source, Message::Type msgtype)
  {
    return routeFromSource[msgtype].lower_bound(source);
  }

  SourceIterator sourceActionsEnd(const std::string& source, Message::Type msgtype)
  {
    return routeFromSource[msgtype].upper_bound(source);
  }

  const std::vector<Action*>* getAction(const std::string &source,Message::Type msgtype);

  Result getRouteCount(const std::string& id);

  std::vector<std::string> getListID(const std::string& id) const;

 protected:
  SourceTable routeFromSource[Message::NBTYPES];
  IDTable routeFromID;
  ipc::MTLock routesLock; ///< lock protecting the routes maps

};

} // namespace plugd

} // namespace flowvr

#endif
