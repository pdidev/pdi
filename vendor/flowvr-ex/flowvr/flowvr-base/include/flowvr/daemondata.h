/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                         Base Libraries                          *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * INRIA and                                                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (FRE 2490). ALL RIGHTS RESERVED.                                *
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
 *    Bruno Raffin,                                                *
 *    Sophie Robert,                                               *
 *    Emmanuel Melin.                                              *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: include/flowvr/daemondata.h                               *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#ifndef FLOWVR_DAEMONDATA_H
#define FLOWVR_DAEMONDATA_H

#include "flowvr/buffer.h"
#include "flowvr/message.h"
#include "flowvr/mem/mpdata.h"
#include "flowvr/ipc/mpchannel.h"
#include "flowvr/ipc/atomic.h"
#include "flowvr/id.h"

#include <string>

namespace flowvr
{

/**
 * @brief Daemon Shared Memory Area Header
 *
 * Used to connect to the daemon and verify validity and find communication channels.
 * This struct is just meant to be a memory layout on a blob of memory in the
 * shared memory area allocated for the daemon to operate on.
 * It represents the header-area or the very first bytes in the shared memory segment.
 *
 */
struct MPDaemonHeader
{
	uword versionMaj;
	uword versionMin;
	mem::MPString daemonName; ///< Daemon name (for error/info/log messages)
	mem::MPString hostName;   ///< Host name
	unsigned int hostID;      ///< Host ID (unique ID per host, usually IP address)
	ipc::MPAtomicInt countID; ///< Counter of the already allocated dynamic ID on this host.
	ipc::MPChannel   cmdChan; ///< channel to send commands to the daemon

	/**
	 * @brief command structure undestood by the daemon
	 */
	struct Cmd
	{
		enum ID
		{
			MODINIT = 1, /**< module sends its welcome / init string */
			MODEXIT,     /**< module exited */
			SHMLIST,     /**< shmman asked the list of shared memory areas */
			SHMNEW,      /**< shmman asked the creation of a new area */
			SHUTDOWN,    /**< shutdown daemon command */
			ABORT        /**< shutdown application */
		};
		int id; /**< the ID of the command, @see ID */
		mem::MPBuffer arg; /**< arguments to the command, typically an XML string */
	};

	/**
	 *   @brief head of the command table (array of pointers)
	 *
	 * 	 By design, the command table is at the end of the
	 * 	 MPDeamonHeader, pre-allocated ("declared") with a size of 1.
	 * 	 the flowvrd defines the number of commands in its main
	 * 	 taking care that the shared memory segment that is reserved
	 * 	 is big enough to fit a user defined amount of commands
	 * 	 in the table. This is a side effect, so be aware of that.
	 *
	 *   Meaning: the daemon header grows its tail (statically) upon
	 *   construction. The cmdTable is just the entry point to the
	 *   array.
	 */
	Cmd cmdTable[1];

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// METHODS
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


	/**
	 * @initialize members of the daemon header with default values
	 *
	 *
	 *
	 * @param nbcmd the number of commands to allocate in the cmdTable.
	 *        Note that this number must match the allocation size of the
	 *        deamon header when firing up the daemon. This is a side effect,
	 *        so be aware of that.
	 */
	void init(int nbcmd)
	{
		versionMaj = 0;
		versionMin = 0;
		hostName.init();
		hostName   = "localhost";
		hostID     = 0x7F000001; // 127.0.0.1
		countID    = 1;
		daemonName.init();


		cmdChan.init(nbcmd, "DaemonHeader.cmdChan");
		for (int i = 0; i < nbcmd; ++i)
		{
			cmdTable[i].id = 0;
			cmdTable[i].arg.init();
		}
	}

	/**
	 * @brief default ID generation algorithm
	 *
	 * The daemon header stores a counter that is used to generate IDs for objects
	 * during application run. This ID is unique, as long as this daemon is alive and no wrap-around
	 * occurs. The generator uses the hostID for this machine, so IDs generated by different
	 * daemons (currently there can only be one per machine) are guaranteed to be different.
	 */
	ID generateID()
	{
		unsigned int c = countID.exchange_and_add(1);
		return static_cast<ID> (c) | (static_cast<ID> (hostID) << 32);
	}
    
    size_t size( ) const
    {
        return MPDaemonHeader::size( cmdChan.size() );
    }
    
    static size_t size( int nbcmd )
    {
        return sizeof(MPDaemonHeader) + (nbcmd-1) * sizeof(MPDaemonHeader::Cmd);
    }
};

/**
 * @brief stub class to convert Message objects from modules to daemon queues and back
 *
 */
struct MPMessage
{
	mem::MPBuffer stamps; /**< stamps stored in the shared memory */
	mem::MPBuffer data;   /**< data stored in the shared memory */


	/**
	 * @brief set all members in stamps and data to 0
	 */
	void init()
	{
		stamps.init();
		data.init();
	}

	/**
	 * @brief detach stamps and data
	 */
	void clear()
	{
		stamps.clear();
		data.clear();
	}

	/**
	 * @brief conversion operator of MPMessage to ordinary Message
	 *
	 * This conversion uses the cast operators from MPBuffer, so
	 * it will operator on the heap.
	 */
	operator Message() const
	{
		Message m;
		m.stamps = (Buffer) stamps;
		m.data = (Buffer) data;
		return m;
	}

	/**
	 * @brief conversion operator of MPMessage to ordinary MessagePut
	 *
	 * This conversion uses the cast operators from MPBuffer, so
	 * it will operator on the heap.
	 */
	operator MessagePut() const
	{
		MessagePut m;
		m.stamps = (BufferWrite) stamps; // use cast to buffer write and
		                                 // implicit assignments operator
		                                 // of StampsWrite from BufferWrite
		m.data = (Buffer) data;
		return m;
	}


	/**
	 * @brief assignment operators from ordinary Message to MPMessage
	 *
	 * Uses assignment operator from MPBuffer.
	 */
	void operator=(const Message& m)
	{
		stamps = m.stamps;
		data   = m.data;
	}
};

/**
 * @brief shared memory representation of a Port.
 *
 * This is a representation of the Ports given by the module during initialization
 * within the shared memory. This representation is assigned once and
 * is then just loosely attached to the original Port instance (as index in the port-private).
 *
 * MPPorts are used to store the current message for the module to read
 */
struct MPPort
{
	enum eFlags
	{
		INPUT       = 1 << 0, /**< input type flag */
		OUTPUT      = 1 << 1, /**< output type flag */
		CONTROL     = 1 << 2, /**< @todo document that type */
		CONNECTED   = 1 << 3, /**< state flag */
		NONBLOCKING = 1 << 4  /**< state flag non-blocking */
	};

	mem::MPString  name; /**< copied from original port */
	int            flags; /**< bitmask of flags, see eFlags */
	mem::MPString  dataType;   ///< Data type specification
	mem::MPString  stampList;  ///< Stamps specification
	int            stampListUpdateIt; ///< Iteration of last stampList update
	MPMessage      current;           ///< Current data
	int            nextnum;

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// METHODS
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/**
	 * @brief call init() on all members and initialize all others
	 *
	 * stampListUpdateIt is set to -1, flags are set to 0
	 */
	void init()
	{
		name.init();
		flags = 0;
		dataType.init();
		stampList.init();
		stampListUpdateIt = -1;
		current.init();
		nextnum = 0;
	}

	/**
	 * @brief call clear on all MPBuffer elements in the MPPort
	 *
	 * Numerical fields are left untouched.
	 */
	void clear()
	{
		name.clear();
		dataType.clear();
		stampList.clear();
		current.clear();
	}
};

/**
 * @brief the module header within the daemon / shared memory
 *
 * Modules register with the daemon by sending the shared-memory segment (as MPBuffer)
 * an instance of this descriptor as argument to MPDaemonHeader::MODINIT.
 *
 * Modules defined two channels
 * - a command channel (direction from module to daemon).
 *   This channel is used to do the state control of the module within the daemon.
 * - an action channel (direction from daemon to module).
 *   This channel is used to tell the module what to do next.
 *
 *
 */
struct MPModuleDescription
{
	/**
	 * @brief module flags
	 *
	 * Currently modules can be marked as being controllers, not much more.
	 * @todo more documentation on daemon level controllers.
	 */
	enum eFlag
	{
		CONTROLLER = 1, /**< flag: mark this module as being a controller */
	};

	/**
	 * @brief command ids for the command channel
	 */
	struct Cmd
	{
		enum ID
		{
			INVALID = 0, /**< invalid command */
			/**
			 * @brief the module wants to close
			 *
			 * CLOSE has no arguments.
			 */
			CLOSE,
			/**
			 *  @brief the module initiated a wait.
			 *
			 *  WAIT has no arguments.
			 */
			WAIT,
			/**
			 * @brief the module initiated a put
			 *
			 * arg is set to contain the message that was put
			 */
			PUT,
			/**
			 * @brief the module changed one stamp list
			 *
			 * arg is set to contain the xml description of the new list
			 */
			STAMP,
			/**
			 * @brief the regulator must perform a hwloc cpuset
			 *
			 * arg.data is set to contain the cpuset as a string
			 */
			CPUSET
		} id;
		MPMessage arg;   /**< arguments to the command (PUT) */
	};

	/**
	 * @brief the number of actions possible
	 *
	 * See Action::ID table.
	 */
	enum
	{
		NBACTIONS = 3
	};


	/**
	 * @brief memory layout of an action.
	 *
	 * Actions are method calls within the module on behalf of the daemon.
	 * The module waits after each command for a responding action to be
	 * taken. The daemon sends an XML string along with more arguments to
	 * the module for doing the action.
	 *
	 * Note that DOACTION is defined to be a very 'soft-action', so the behavior of
	 * the ModuleAPI when receiving the action is determined by the ModuleAPI alone.
	 *
	 * Note also that there is currently no way to respond to the daemon any result
	 * created by doAction().
	 */
	struct Action
	{
		enum ID
		{
			INVALID = 0,
			/**
			 *  @brief inport saturation occurred, end module wait, return to module scope
			 *
			 *  This action has no arguments.
			 */
			ENDWAIT,

			/**
			 * @brief perform an action determined by the XML string passed as argument.
			 *
			 * Both, XML and arg should be passed to the doAction() method of the module implementation.
			 */
			DOACTION,

			/**
			 * @brief the daemon wants the module to stop iterating
			 *
			 * There are no arguments to that command.
			 */
			CLOSE,
		} id;
		mem::MPString xml; /**< XML string passed by DOACTION */
		mem::MPBuffer arg; /**< arg structure / additional data used by DOACTION */
	};


	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// MEMORY LAYOUT
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/// @name module version to check a good match (unused)
	/// @{
	uword versionMaj;
	uword versionMin;
	/// @}

	/// @name naming and debug stuff
	/// @{
	mem::MPString name;
	mem::MPString parent;
	mem::MPString traceDesc; ///< XML Description of the available traces
    /// @}

	/// @name stats and state
	/// @{
	int flags;
	int status;
	int iteration;
	/// @}

	/// @name command channel
	/// The command channel slots are assigned to the very tail of this descriptor.
	/// The offset is used to reflect the first byte of the command array in the
	/// shared memory segment.
	/// @{
	int cmdOffset; ///< buffer offset for Cmd table
	ipc::MPChannel cmdChan;
	/// @}

	/// @name action channel
	/// @{
	ipc::MPChannel actionChan;
	Action actions[NBACTIONS];
	/// @}


	/// @name ports array
	/// The ports array is at the tail of this structure, as it has to be grown and
	/// populated by the creator of this MPModuleDescription. This is a side-effect,
	/// so be aware of that.

	/// @{
	int    nbPorts;  /**< the number of ports in the port array */
	MPPort ports[0]; /**< the head of the port array */
	/// @}

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// METHODS
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/**
	 * @brief write default values to this module descriptor
	 *
	 * @param nbports the number of ports to initialize. This number must
	 *        match with the total size of the shared memory segment that
	 *        was allocated for this module descriptor. This is a side-effect,
	 *        so be aware of that.
	 * @param nbcmd the number of commands (for the command channel) to
	 *        initialize. Typically this is number of ports + 1.
	 *        The command channels are placed behind all ports, and just
	 *        the offset relative to the beginning of this descriptor is
	 *        stored
	 * @param moduleName the module name to copy to the descriptor field
	 */
	void init(const std::string& moduleName, int nbcmd, int nbports)
	{
		versionMaj = 0;
		versionMin = 0;
		name.init();
		name = moduleName;
		parent.init();
		flags = 0;
		status = 0;
		iteration = 0;

		traceDesc.init();

#ifdef DEBUG
		std::string s=moduleName+".cmdChan";
		cmdChan.init(nbcmd,s.c_str());
#else
		cmdChan.init(nbcmd);
#endif
		// calculate offset of cmd array right behind the port array
		cmdOffset = sizeof(MPModuleDescription) + nbports * sizeof(MPPort);

		// interpret as pointer to commands
		Cmd* cmdTable = (Cmd*) ((char*) this + cmdOffset);

		// init all command slots
		for (int c = 0; c < nbcmd; c++)
		{
			cmdTable[c].id = Cmd::INVALID;
			cmdTable[c].arg.init();
		}


		// init ports
		nbPorts = nbports;
		for (int p = 0; p < nbports; p++)
			ports[p].init();

		// init action channels and action channel slots
#ifdef DEBUG
		{
			std::string s=moduleName+".actionChan";
			actionChan.init(NBACTIONS,s.c_str());
		}
#else
		actionChan.init(NBACTIONS);
#endif

		// mark all slots as invalid and init xml and arg string properly
		for (int a = 0; a < NBACTIONS; a++)
		{
			actions[a].id = Action::INVALID;
			actions[a].xml.init();
			actions[a].arg.init();
		}
	}

	/**
	 * @brief clear all MPBuffer members of the descriptor
	 *
	 * Clears name, parent, traceDesc, ports, actions and commands.
	 * Numerical fields are untouched.
	 */
	void clear()
	{
		name.clear();
		parent.clear();
		traceDesc.clear();

		for (int p = 0; p < nbPorts; p++)
			ports[p].clear();
		for (int a = 0; a < NBACTIONS; a++)
		{
			actions[a].xml.clear();
			actions[a].arg.clear();
		}
		Cmd* cmdTable = (Cmd*) ((char*) this + cmdOffset);
		for (int c = 0; c < cmdChan.size(); c++)
		{
			cmdTable[c].arg.clear();
		}
	}

};


/**
 * @brief log buffer memory layout
 *
 * A log buffer is one slot in the array of log buffers available.
 * It resembles a header, pointing to the next buffer using the nextPos
 * field.
 */
struct MPLogBuffer
{
	flowvr::ipc::MPAtomicIntBw nextPos; /**< distance to next buffer */

	/**
	 * @brief clear the buffer, leaving the distance field as is
	 */
	void init(size_t size)
	{
		/// during init, this buffer is considered to contain only the 'nextPos' field
		nextPos = sizeof(MPLogBuffer);
		memset(((unsigned char*) this) + nextPos, 0, size - nextPos);
	}
};

/**
 * @brief log info memory layout
 */
struct MPLogInfo
{
	flowvr::ipc::MPSignal bufferFull;
	flowvr::ipc::MPLock   lock;
	flowvr::ipc::MPAtomicInt firstBuffer;
	flowvr::ipc::MPAtomicInt nbBuffer;

	enum
	{
		MAX_BUFFER = 256
	};

	mem::MPBuffer buffers[MAX_BUFFER];


	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// METHODS
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/**
	 * @brief init members, init all buffers used
	 */
	void init()
	{
		bufferFull.init("MPLogInfo.bufferFull");
		lock.init("MPLogInfo");
		firstBuffer = 0;
		nbBuffer = 0;
		for (int i = 0; i < MAX_BUFFER; i++)
			buffers[i].init();
	}

	/**
	 * @brief obtain a BufferWrite to write the next log to with a given size
	 *
	 * Management routine to obtain a new BufferWrite object that can be used to
	 * write the up-coming data to.
	 * This method used linear search to find the next free buffer.
	 * In case it encounters a full buffer during search, it sets the nextPos to 1 and
	 * calls notify on this BufferInfo bufferFull signal.
	 *
	 * @param datasize the number of bytes to hold in the buffer
	 * @return
	 *       - a valid buffer if a segment of size datasize was found to be free
	 *       - an invalid buffer if no segment could be found given the size
	 */
	BufferWrite writeData(size_t datasize)
	{
		int buffer = firstBuffer;
		for (int i = 0; i < nbBuffer; i++)
		{
			BufferWrite b = buffers[(buffer + i) & (MAX_BUFFER - 1)];
			if (b.valid())
			{
				MPLogBuffer* lb = b.getWrite<MPLogBuffer> (0);
				if (lb->nextPos < b.getSize())
				{
					size_t pos = lb->nextPos.exchange_and_add(datasize);
					if (pos + datasize <= b.getSize())
					{
						return BufferWrite(b, pos, datasize);
					}
					else if (pos < b.getSize())
					{
						*b.getWrite<unsigned char> (pos) = 1;
						std::cerr << "LOG: Buffer " << firstBuffer + i
								<< " full (" << pos << " bytes written)."
								<< std::endl;
						bufferFull.notify();
					}
				}
			}
		}
		return BufferWrite();
	}

};


/**
 * @brief Daemon Shared Memory Area Header
 *
 * Used to receive feedback from the daemon after asking him to allocate a new
 * shared memory area.
 * This struct is just meant to be a memory layout on a blob of memory in the
 * shared memory area */      // allocated for the daemon to operate on.
 /** It represents the header-area or the very first bytes in the shared memory segment.
 *
 */
struct MPshmManagerInterface
{
    ipc::MPChannel infoChan; ///< channel to receive info from the daemon

	/**
	 * @brief command ID for the comamnd channel
	 */
    struct Info
    {
        enum ID
        {
            SHMLIST,      /**< daemon sends the list of shared memory areas */
            SHMNEW,       /**< daemon created a new shared memory area */
            SHMNOPE       /**< daemon didn't create a new shared memory area */
        };
        int id;             /**< the ID of the command, @see ID */
        mem::MPBuffer arg;  /**< MPBuffer to the shared list */
    };

	/**
	 *   @brief command
	 */
    Info infoTable[1];

	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
	// METHODS
	// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

	/**
	 * @initialize members of the daemon header with default values
	 *
	 *
	 * @param nbcmd the number of commands to allocate in the cmdTable.
	 *        Note that this number must match the allocation size of the
	 *        daemon header when firing up the daemon. This is a side effect,
	 *        so be aware of that.
     * @param moduleName the module name to copy to the descriptor field
	 */
	void init( const std::string& pid, int nbcmd = 1 )
	{
        // MP table init
        std::string s = pid+".SharedMemoryManager.infoChan";
        infoChan.init( nbcmd, s.c_str() );
		for (int i = 0; i < nbcmd; ++i)
		{
			infoTable[i].id = 0;
            infoTable[i].arg.init();
		}
	}
	
	static size_t size( int nInfo )
    {
        return sizeof(MPshmManagerInterface) + (nInfo-1) * sizeof(MPshmManagerInterface::Info);
    }
};


	/////////////////////////////////////////////////////////////////////
	// DeamonInterface -- utility function around daemon manipluation
	/////////////////////////////////////////////////////////////////////
	class DaemonInterface
	{
	public:
		static bool        setHostName( const std::string &strHostname, int ID );
		static std::string getHostName();
		static int         getDaemonID();
		static std::string getDeamonIDString();
		
		static void setMainArguments( int* argc, char*** argv );
		static int*	getArgc();
		static char*** getArgv();
		
		static void setDefaultDaemonId( int newId, bool bypassEnv );
	private:
		static int*    m_argc;
		static char*** m_argv;
		static int  m_defaultDaemonId;
		static bool m_bypassEnv;
	};





} // namespace flowvr

#endif
