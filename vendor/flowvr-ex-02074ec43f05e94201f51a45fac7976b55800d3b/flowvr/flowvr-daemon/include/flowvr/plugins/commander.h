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
* File: include/flowvr/plugins/commander.h                        *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_PLUGINS_COMMANDER_H
#define FLOWVR_PLUGINS_COMMANDER_H

#include <flowvr/thread.h>
#include <flowvr/plugd/object.h>


#include <flowvr/ipc/mtlock.h>
#include <flowvr/ipc/mtsignal.h>
#include <flowvr/plugd/actionhandler.h>


#include <deque>



namespace flowvr
{
	namespace mem
	{
		class SharedMemoryArea;
	}

namespace plugins
{

	class Commander : public flowvr::plugd::Object,
					  private flowvr::Thread
	{
	public:
	  Commander(std::string objID);
	  /// Destructor.
	  virtual ~Commander();

	  flowvr::mem::SharedMemoryArea* shm;
	  flowvr::MPDaemonHeader* header;
	  flowvr::plugd::Dispatcher* threadDispatcher;
	  std::ostream* commandsOutput;
	  bool closeOutput;


	  virtual plugd::Class* getClass() const;

	  /// Initialization. Returns a XML document containing the result.
	  virtual flowvr::plugd::Result init(flowvr::xml::DOMElement* xmlRoot, plugd::Dispatcher* dispatcher);

	  class ControlMessageHandler : public plugd::ActionHandler,
									private flowvr::Thread
	  {
	  public:

		ControlMessageHandler(Commander* _parent);

		virtual void init();

		virtual ~ControlMessageHandler();

		/// Execute the action processing the specified message.
		/// The dispatcher parameter is used to dispatch any generated message
		virtual void doIt(const Message& msg, plugd::Dispatcher* dispatcher);

		/// Remove this ActionHandler.
		/// This will destroy this instance if ActionHandlers are dynamically
		/// instanciated or it will remove one reference.
		virtual void remove();

		virtual int run();

	  protected:

		/// Object-wide lock
		ipc::MTLock lock;
		ipc::MTSignal signal;
		bool stop;
		typedef std::deque<Message> Queue;
		/// Message buffer
		Queue queue;
		flowvr::plugd::Dispatcher* dispatcher;
		Commander* parent;

		virtual bool waitNewMessage(Message& msg);

		/// Determine the command and process it
		virtual plugd::Result processCommand(xml::DOMElement* root, const std::string& reply);

		/// Process a control message.
		virtual void processControlMessage(const flowvr::Message& msg);

		/// Process a reply message to a controller.
		virtual void processReply(const Message& msg, const std::string& reply);

		/// Process an addobject message.
		virtual plugd::Result processAddObject(xml::DOMElement* root, const std::string& reply);

		/// Process a delobject message.
		virtual plugd::Result processDelObject(xml::DOMElement* root, const std::string& reply);

		/// Process an addroute message.
		virtual plugd::Result processAddRoute(xml::DOMElement* root, const std::string& reply);

		/// Process a delroute message.
		virtual plugd::Result processDelRoute(xml::DOMElement* root, const std::string& reply);

		/// Process a getcountroute message
		virtual plugd::Result processGetRouteCount(xml::DOMElement* root, const std::string& reply);

		/// Process an action message.
		virtual plugd::Result processAction(xml::DOMElement* root,const std::string& reply);

		/// Process a group message
		virtual plugd::Result processGroupAction(xml::DOMElement* root,const std::string& reply);

	  };


	  /// Create an ActionHandler for batch mode action execution.
	  virtual flowvr::plugd::ActionHandler* createAction(flowvr::xml::DOMElement* xmlRoot);

	 protected:
	  virtual void processCmdModInit(flowvr::MPDaemonHeader::Cmd& cmd);
	  virtual void processCmdModExit(flowvr::MPDaemonHeader::Cmd& cmd);
	  virtual void processCmdAbort(flowvr::MPDaemonHeader::Cmd& cmd);

	 public:
	  /// Main thread function.
	  virtual int run();

	};

} // namespace plugins

} // namespace flowvr

#endif
