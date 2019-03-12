/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: daemondata.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include <flowvr/daemondata.h>
#include <flowvr/mem/sharedmemorymanager.h>
#include <flowvr/mem/sharedmemoryarea.h>

namespace flowvr
{
	int*    DaemonInterface::m_argc = NULL;
	char*** DaemonInterface::m_argv = NULL;
	int  DaemonInterface::m_defaultDaemonId = flowvr::DefaultMemId;
	bool DaemonInterface::m_bypassEnv = false;


	void DaemonInterface::setDefaultDaemonId( int newId, bool bypassEnv )
	{
		m_defaultDaemonId = newId;
		m_bypassEnv = bypassEnv;
	}

	bool DaemonInterface::setHostName( const std::string &hostname, int ID )
	{
		/// this is an operation bound to the shared memory.
		/// we assume that the daemon was initialized and the daemon-data block waits for
		/// us in the 'main' memory area, which is defined by the shared memory manager.
		mem::SharedMemoryArea* shm = mem::SharedMemoryManager::instance()->getMainMemoryArea();
		MPDaemonHeader* header = (shm == NULL ? NULL : shm->getWrite<MPDaemonHeader> (shm->readHeader()));
		if (header != NULL)
		{
			// assign to daemon header
			header->hostName = hostname;
			header->hostID   = ID;
			return true;
		}
		return false;
	}

	std::string DaemonInterface::getHostName()
	{
		mem::SharedMemoryArea* shm = mem::SharedMemoryManager::instance()->getMainMemoryArea();
		const MPDaemonHeader* header = (shm == NULL ? NULL : shm->getRead<MPDaemonHeader> (shm->readHeader()));
		if (header != NULL)
		{
			return (std::string)header->hostName;
		}
		return "";
	}

	int  DaemonInterface::getDaemonID()
	{
		if(!m_bypassEnv)
		{
			const char* val = getenv("FLOWVR_DAEMON");
			if (val != NULL && val[0] != '\0')
				return atoi(val);
		}

		return m_defaultDaemonId;
	}

	std::string DaemonInterface::getDeamonIDString()
	{
		const char* val = getenv("FLOWVR_DAEMON");
		if(val)
			return std::string(val);
		char id[256];
		sprintf( id, "%d", m_defaultDaemonId );
		return std::string(id);
	}
	
	void DaemonInterface::setMainArguments( int *argc, char*** argv )
	{
		m_argc = argc;
		m_argv = argv;
	}

	int*	DaemonInterface::getArgc()
	{
		return m_argc;
	}

	char*** DaemonInterface::getArgv()
	{
		return m_argv;
	}

}
