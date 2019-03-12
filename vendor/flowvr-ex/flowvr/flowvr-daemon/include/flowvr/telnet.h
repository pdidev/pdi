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
 * File: include/telnet.h                                          *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/
#include "flowvr/module.h"
#include "flowvr/thread.h"
#include "flowvr/ipc/mtlock.h"
#include "flowvr/ipc/mtsignal.h"

#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <stdlib.h>
#include <unistd.h>
#include <fcntl.h>
#include <signal.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>

class BasicController: public flowvr::Thread
{

public: 
        /**
         * \class Commands
         * \brief Utility class to handle commands
         */ 
        class Commands
        {
            public:
                void init(BasicController *parent);

                class Command
                {
                public:
                	virtual int exec( flowvr::xml::DOMElement *cmd, const flowvr::Message &m = flowvr::Message() ) = 0;

                	virtual bool needsArgs() const { return false; }
                	virtual size_t getArgList( std::list<std::string> & ) const { return 0; }
                };

                class CommandStruct
                {
                public:
                	CommandStruct()
                	: command(NULL) {}

                	CommandStruct( Command *cmd, const std::string &strName, const std::string &shortc, const std::string &hlp, bool _intern = false )
                	: command(cmd)
                	, name(strName)
                	, shortcut(shortc)
                	, help(hlp)
                	, intern(_intern)
                	{}

                	void print() const;

					std::string name;///< command name
					std::string shortcut;///< command short name
					std::string help;///<  help  info
					Command    *command; ///< the command to execute
					bool intern;
                };

                Commands() {}
                ~Commands();
                
                /**
                 * \brief add a new command to the list
                 */
                void add(const std::string &name,
                         const std::string &shortcut,
                         const std::string &help,
                         Command *cmd = NULL,
                         bool _intern = false);

                /**
                 * \brief check if a given string matches a given  command full name or shortcut name (if shortcut=1, skip shortname test otherwise)
                 */
                bool is(const std::string &input, const bool shortcut=false) const;

//                /**
//                 * \brief Return the commande code corresponding to the name given as input.
//                 *        Also probe shortcut names if shortcut set to true.
//                 *        Return NOTFOUND if no corresponding command found.
//                 */
//                Commandenum get(const std::string &input,  const bool shortcut=false) const;

                CommandStruct getCmd( const std::string &input, const bool shortcut = false ) const;

                /**
                 * \brief print the full  list of commands on telnet consol when user tape in help.
                 */
                void  printHelp() const;

            public:
                typedef std::map<std::string,CommandStruct> CMDS;
                CMDS commandsmap; ///< map of commands
        };


	void addHostMapping(const std::string& host, const std::string& newhost)
	{
		std::cout << "Adding Host mapping " << host << "->" << newhost
				<< std::endl;
		if (!host.empty()) // && !newhost.empty())
			destMap[host] = newhost;
	}

	void replaceHost(flowvr::xml::DOMElement * elem);
	void replaceHosts(flowvr::xml::DOMElement* root);



	BasicController()
	: flowvr::Thread()
	, dest("127.0.0.1")
	, flog(-1)
	, lock("BasicController.lock")
	, signal("BasicController.signal")
	, stamps(new flowvr::StampListControl)
	, bVerboseResponse(false)
	, _AbortThread( *this )
	{
		commands.init(this);
	}

	~BasicController();


	bool init(const std::string &prefixFile_="");

	std::string getParent() const
	{
		return reply;
	}

	bool ok() const
	{
		return ctrlout->getStatus() != 0;
	}

	int flush();
	int pause();
	int startCommand();
	int stopCommand();
	int trace();
	int notrace();
	int toggleVerboseResponse();

	int printHistory();

	int sendCommand(const std::string &text, const std::string &destination = "", bool bShowReply = false);
	int processCommand(const std::string &text, bool bShowReply = false );
	int processCommand(flowvr::xml::DOMElement* root,  bool bShowReply = false);
	void groupCommandAll(const std::string &command);



	void printLast();
	void addCommand( const Commands::CommandStruct &cs );

	std::string getDest() const;
	void setDest( const std::string &dest );

	class History
	{
	public:

		class CmdHistory
		{
		public:
			CmdHistory()
			: ok(false)
			, showReply(false)
			{}

			CmdHistory( const std::string &_dest, const std::string &_cmd, const std::string &_reply, bool bok, bool _showReply = false )
			: dest(_dest)
			, cmd( _cmd )
			, reply( _reply )
			, ok(bok)
			, showReply(_showReply ){}

			std::string dest;
			std::string cmd;
			std::string reply;
			bool ok, showReply;
		};

		void append( const CmdHistory & );
		size_t getHistorySize() const;

		const CmdHistory &operator[](int index) const;
		CmdHistory &operator[](int index);

		void clear();

		void print() const;

	private:
		std::vector<CmdHistory> history;
		mutable flowvr::ipc::MTLock lock;
	};

        /*! list of modules that are expected to connect. */
        std::set<std::string> allExpectedModules;


protected:
	/**
	 * @brief implementation from thread interface
	 */
	virtual int run();

private:
	int processFile( const std::string &file );


	int receiveResult( flowvr::xml::DOMElement *, const flowvr::Message &m );
	int receiveNews( flowvr::xml::DOMElement *, const flowvr::Message &m );

	Commands::CommandStruct getCommand(const std::string &) const;


	bool processReply( const std::string &text, int loop, const std::vector<int> &id, std::map<std::string,int> &idmap, bool bFillMode );



    Commands           commands;
	flowvr::ModuleAPI* ctrlin;
	flowvr::ModuleAPI* ctrlout;

	flowvr::InputPort*  pIn;
	flowvr::OutputPort* pOut;

	flowvr::StampListControl *stamps;

	std::string reply;
	std::string dest;

	std::string prefixFile;

	int flog;

	typedef std::map<std::string, std::string> HostMap;
	std::map<std::string, std::string>         destMap;
	std::vector<std::string> vechost;
	std::set<std::string>    modules;


	Commands::CMDS m_mpCommands;
    bool bLeave, bVerboseResponse;

	// ongoing commands
	flowvr::ipc::MTLock lock;
	flowvr::ipc::MTSignal signal;
	int lastSentCmd;
	int lastCompletedCmd;

	History history;
    
    void print_modules(BasicController *bc);
    
    // abort from a module request
    struct AbortThread : public flowvr::Thread {
        AbortThread( BasicController & ctrl ) : _ctrl( ctrl ) {}
    private:
        virtual int run() { std::cerr << "Abort!\n"; _ctrl.stopCommand(); return 0; }
        BasicController & _ctrl;
    };
    AbortThread _AbortThread;
};


