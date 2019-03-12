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
* File: include/flowvr/daemon.h                                   *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef FLOWVR_DAEMON_H
#define FLOWVR_DAEMON_H

//#include "flowvr/daemondata.h"
#include "flowvr/buffer.h"
//#include "flowvr/stamp.h"
//#include "flowvr/message.h"
//#include "flowvr/thread.h"
//#include "flowvr/ipc/mtlock.h"
//#include "flowvr/ipc/mtsignal.h"
//#include "flowvr/plugd/class.h"
//#include "flowvr/plugd/genclass.h"
//#include "flowvr/plugd/dispatcher.h"
//#include "flowvr/plugd/actionhandler.h"

//#include "flowvr/mem/sharedmemoryarea.h"
//#include "flowvr/mem/sharedmemorymanager.h"

namespace flowvr
{

	namespace daemon
	{

		/// Get the pointer to a "new" module (new meaning registered but not yet assigned to any regulator).
		/// This function also removes the module from the list of new modules.
		BufferWrite getNewModule(std::string name, std::string parent, bool wait=true);

		/// Register a new module (well, you can read the function name too ;))
		void registerNewModule(BufferWrite moduleBuffer);

		extern int verboseLevel; ///< Output verbosity level

	} // namespace daemon

} // namespace flowvr

#endif
