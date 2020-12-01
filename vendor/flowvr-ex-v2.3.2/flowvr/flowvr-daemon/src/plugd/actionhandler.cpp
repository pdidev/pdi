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
* File: src/plugd/actionhandler.cpp                               *
*                                                                 *
* Contacts:                                                       *
*  04/22/2005 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/

#include <flowvr/plugd/actionhandler.h>
#include <flowvr/plugd/dispatcher.h>

namespace flowvr
{
	namespace plugd
	{
		class RedispatchActionHandler : public ActionHandler
		{
		public:
		  virtual ~RedispatchActionHandler() {}

		  virtual void doIt(const Message& msg, Dispatcher* dispatcher)
		  {
			dispatcher->process(msg);
		  }

		  virtual void remove()  { }

		};

		static RedispatchActionHandler loopBackActionHandler;
		ActionHandler* ActionHandler::LoopBackActionHandler = &loopBackActionHandler;

	} // namespace plugd

} // namespace flowvr
