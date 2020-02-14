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
* File: include/flowvr/plugins/dispatcherobject.h                 *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_DISPATCHEROBJECT_H
#define FLOWVR_PLUGINS_DISPATCHEROBJECT_H

#include "flowvr/plugd/object.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/ipc/mtlock.h"

namespace flowvr
{

namespace plugd
{

class Dispatcher;

}

namespace plugins
{

class DispatcherObject : public plugd::Object
{
 public:

  /// Constructor.
  DispatcherObject(std::string objID)
    : Object(objID)
  {
  }

  /// Get the dispatcher.
  virtual plugd::Dispatcher* getDispatcher()=0;

};

} // namespace plugins

} // namespace flowvr

#endif
