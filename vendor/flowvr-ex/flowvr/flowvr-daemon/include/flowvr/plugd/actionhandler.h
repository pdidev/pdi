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
* File: include/flowvr/plugd/actionhandler.h                      *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGD_ACTIONHANDLER_H
#define FLOWVR_PLUGD_ACTIONHANDLER_H

#include "flowvr/message.h"

namespace flowvr
{

namespace plugd
{

	class Dispatcher;

	/**
	 *  @brief Abstract interface defining action handlers.
	 *
	 *  @todo fill more documentation on ActionHandlers
	 */
	class ActionHandler
	{
	 public:
	  /// Execute the action processing the specified message.
	  /// The dispatcher parameter is used to dispatch any generated message
	  virtual void doIt(const Message& msg, Dispatcher* dispatcher) = 0;

	  virtual void remove() = 0;

	  static ActionHandler *LoopBackActionHandler;
	 protected:
	  virtual ~ActionHandler() {}
	};

} // namespace plugd

} // namespace flowvr

#endif
