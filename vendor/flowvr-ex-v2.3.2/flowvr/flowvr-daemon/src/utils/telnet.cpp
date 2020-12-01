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
 * File: src/utils/telnet.cpp                                      *
 *                                                                 *
 * Contacts:                                                       *
 *  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
 *                                                                 *
 ******************************************************************/

#include <limits.h>
#include "flowvr/telnet.h"
#undef private
#include <flowvr/xml.h>
#include <flowvr/utils/timing.h>
#include <flowvr/utils/cmdline.h>

// Terminal VT100 special code
// TODO: ifdef for other platforms or use external libs (like ncurse)
const char* termClearEndOfLine = "\e[K";


flowvr::utils::Option<std::string> OptParentName("parent", 'p', "Use the provided string as middle name when building FLOWVR_PARENT  instead of process pid",false);
flowvr::utils::FlagOption OptSync("sync", 's', "Sync mode (flush between each command) (Currently forced to true)", true );
flowvr::utils::FlagOption OptNoOut("noout", 'f', "Suppress sub-processes output");
flowvr::utils::Option<std::string> OptLogOut("log", 'l', "Log sub-processes output and optionally set file prefix", (std::string)"log-");
flowvr::utils::FlagOption OptVerbose("verbose", 'v', "More detailed output (repeat for even more messages)");
flowvr::utils::Option<std::string>OptModuleReplyIp("bindingAddress", 'b', "Use the provided address for building FLOWVR_PARENT  if name  returned by gethosname is not visible to remote deamons (deamons connect on a network diffrent from the one gethostname is related to).",false);


namespace
{

	class Executor
	{
	public:
		static std::map<std::string, pid_t> mod_pid;
		static std::map<pid_t, std::string> pid_mod;

		static void create(const std::string& command, const char* id = NULL,
						   const char* shell = "/bin/sh", const char* execdir = NULL);
		static void killall();
		static int  waitchild();
	};

	std::map<std::string, pid_t> Executor::mod_pid;
	std::map<pid_t, std::string> Executor::pid_mod;


	inline const char* notNULL(const char* s)
	{
		return (s == NULL) ? "" : s;
	}

	int kill_flowvr(pid_t __pid, int __sig)
	{
		return kill(__pid, __sig);
	}


	void _printString( const std::string &s )
	{
		std::cout << "\t\t" << s << std::endl;
	}



	class _printEl : public std::unary_function< BasicController::Commands::CMDS::value_type, void >
	{
	public:
		_printEl( bool bShowInterns )
		: m_showInterns( bShowInterns ) {}

		void operator() ( const BasicController::Commands::CMDS::value_type el ) const
		{
			if( el.second.intern and !m_showInterns )
				return;

			el.second.print();
			if( el.second.command and el.second.command->needsArgs() )
			{
				std::cout << "\tARGUMENTS:" << std::endl;
				std::list<std::string> store;
				el.second.command->getArgList(store);
				std::for_each( store.begin(), store.end(), _printString );
			}
		}

		bool m_showInterns;
	};

	class _printHistEl : public std::unary_function<const BasicController::History::CmdHistory &, void>
	{
	public:
		void operator()( const BasicController::History::CmdHistory &el ) const
		{
		  std::cout << std::endl << "------------COMMAND SUMMARY----------------------------------" << std::endl;
			std::cout << "cmd  :  " << el.cmd << std::endl
					  << "dest :  " << el.dest << std::endl
					  << "reply:  " << el.reply << std::endl
					  << "OK   :  " << (el.ok ? "YES" : "NO") << std::endl
					  << "SHOW :  " << (el.showReply ? "YES" : "NO") << std::endl
					  << "------------------------------------------------------------" << std::endl;
		}
	};

	class _deleteOp : public std::unary_function< BasicController::Commands::CMDS::value_type, void >
	{
	public:
		void operator() ( const BasicController::Commands::CMDS::value_type el ) const
		{
			delete el.second.command;
		}
	};


	class _namePred : public std::unary_function<BasicController::Commands::CommandStruct,void>
	{
	public:
		_namePred( const std::string &strName, bool bTestShortcut )
		: m_name(strName)
		, m_bTestShortcut(bTestShortcut)
		{

		}

		virtual bool operator()( const std::pair<std::string,BasicController::Commands::CommandStruct> &el ) const
		{
			return (el.second.name == m_name or (m_bTestShortcut and el.second.shortcut == m_name) );
		}

		virtual bool operator()( const BasicController::Commands::CommandStruct &el ) const
		{
			return (el.name == m_name or (m_bTestShortcut and el.shortcut == m_name) );
		}

		std::string m_name;
		bool m_bTestShortcut;
	};

	class _nameMatch : public std::unary_function<const std::string &, void>
	{
	public:
		_nameMatch( const std::string &name )
		: m_name(name) {}

		bool operator()( const std::string &sname ) const
		{
			return (m_name == sname);
		}

		std::string m_name;
	};

	class _sendFunctor : public std::unary_function<const std::string &, void>
	{
	public:
		_sendFunctor( BasicController *parent, const std::string &command, std::vector<int> &results )
		: m_parent(parent)
		, m_command(command)
		, m_results(results) {}

		void operator()( const std::string &destination ) const
		{
			m_results.push_back(m_parent->sendCommand( m_command, destination ));
		}

		BasicController *m_parent;
		std::string      m_command;
		std::vector<int>  &m_results;
	};

	////////////////////////////////////////////////////////////////////////////////////////////////////////
	// COMMANDS
	////////////////////////////////////////////////////////////////////////////////////////////////////////

	class HelpCmd : public BasicController::Commands::Command
	{
	public:
		HelpCmd( BasicController::Commands *cmds )
		: commands(cmds)
		{}

		virtual int exec( flowvr::xml::DOMElement *cmd, const flowvr::Message & )
		{
			commands->printHelp();
			return 0;
		}

		BasicController::Commands *commands;
	};

	class ControllerCommand : public BasicController::Commands::Command
	{
	public:
		typedef int (BasicController::*func)();

		ControllerCommand( func foo, BasicController *base )
		: m_func(foo)
		, m_controller(base) {}


		virtual int exec( flowvr::xml::DOMElement *, const flowvr::Message & )
		{
			return (m_controller->*m_func)();
		}

		func             m_func;
		BasicController *m_controller;
	};

	class AdvControllerCommand : public BasicController::Commands::Command
	{
	public:
		typedef int (BasicController::*func)(flowvr::xml::DOMElement *, const flowvr::Message & );

		AdvControllerCommand( func foo, BasicController *base )
		: m_func(foo)
		, m_controller(base) {}


		virtual int exec( flowvr::xml::DOMElement *el, const flowvr::Message &m )
		{
			return (m_controller->*m_func)(el,m);
		}

		func             m_func;
		BasicController *m_controller;
	};



	class RunCommand : public BasicController::Commands::Command
	{
	public:
		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
			const char* id = root->Attribute("metamoduleid");
			if( id == NULL )
				return 0;

			const char* shell = root->Attribute("shell");
			const char* execdir = root->Attribute("execdir");
			if (shell==NULL || shell[0]=='\0')
				shell="/bin/sh";
			Executor::create(root->getTextContent(), id, shell, execdir);

			return -1;
		}

    	virtual bool needsArgs() const { return true; }
    	virtual size_t getArgList( std::list<std::string> &store ) const
    	{
    		store.push_back("metamoduleid");
    		store.push_back("shell");
    		store.push_back("execdir");

    		return store.size();
    	}
	};

	class AddHostMapping : public BasicController::Commands::Command
	{
	public:
		AddHostMapping( BasicController *parent )
		: m_parent(parent) {}

		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
			m_parent->addHostMapping(notNULL(root->Attribute("from")), notNULL(root->Attribute("to")));
			return -1;
		}
    	virtual bool needsArgs() const { return true; }
    	virtual size_t getArgList( std::list<std::string> &store ) const
    	{
    		store.push_back("from");
    		store.push_back("to");

    		return store.size();
    	}

		BasicController *m_parent;
	};

	class SetDestination : public BasicController::Commands::Command
	{
	public:
		SetDestination( BasicController *parent )
		: m_parent(parent) {}

		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
			m_parent->setDest(root->getTextContent());
			return -1;
		}

		BasicController *m_parent;
	};

	class KillAll : public BasicController::Commands::Command
	{
	public:
		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
			Executor::killall();
			return -1;
		}
	};


	class WaitChildCommand : public BasicController::Commands::Command
	{
	public:
		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
	        int pid = Executor::waitchild();
	        if (pid>0)
	            std::cout << "Child "  << pid << "(" << Executor::pid_mod[pid]
	                      << ") exited" << std::endl;
	        return -1;
		}
	};

	class WaitCommand : public BasicController::Commands::Command
	{
	public:
		virtual int exec( flowvr::xml::DOMElement *root, const flowvr::Message & )
		{
	        const char* duration = root->Attribute("duration");
	        if (duration!=NULL)
	        {
	            std::cout << "Waiting "<<duration<<" seconds"<<std::endl;
	            flowvr::utils::microsleep(int(1000*atof(duration)));
	        }
	        return -1;
		}

    	virtual bool   needsArgs() const { return true; }
    	virtual size_t getArgList( std::list<std::string> &store ) const
    	{
    		store.push_back("duration");
    		return store.size();
    	}
	};
}


///////////////////////////////////////////////////////////////////////////////
// EXECUTOR
///////////////////////////////////////////////////////////////////////////////


void Executor::create(const std::string& command, const char* id, const char* shell, const char* execdir)
{
    pid_t pid;

    // command being executed
    std::string new_command = command;
    bool needChDir = false;
    // Change execution directory

    pid = fork ();

    if (pid < 0)
	{
		std::cout << "fork failed." << std::endl;
		return;
	}
	else if (pid == 0)
	{
		// Processus fils
		// Remove standard input
		dup2(open("/dev/null", O_RDONLY), 0);
		if (OptLogOut.count > 0)
		{
			std::string fname = (std::string) OptLogOut;
			fname += id;
			for (unsigned int i = ((std::string) OptLogOut).length(); i
					< fname.length(); i++)
			{
				if (fname[i] == '/' || fname[i] == '\\')
					fname[i] = '_';
			}
			int fd = creat(fname.c_str(), 0666);
			if (OptNoOut)
				dup2(open("/dev/null", O_WRONLY), 1);
			else
				dup2(fd, 1);
			fname += ".err";
			fd = creat(fname.c_str(), 0666);
			dup2(fd, 2);
		}
		else if (OptNoOut)
		{
			dup2(open("/dev/null", O_WRONLY), 1);
			dup2(open("/dev/null", O_WRONLY), 2);
		}

		if (execdir != NULL)
		{
			int retdir = chdir(execdir);
			if (retdir != 0)
				std::cerr << "ERROR: chdir(" << execdir << ") returned "
						<< retdir << std::endl;
		}

		// Set environment variables
		if (id != NULL)
			setenv("FLOWVR_MODNAME", id, 1);
		// Execute the command
		int ret = execl(shell, shell, "-c", new_command.c_str(), NULL);
		std::cout << "ERROR: execl returned " << ret << std::endl;
		exit(ret);
	}
	else
	{
		// Processus pere
		if (id == NULL)
			id = "";
		std::cout << "Forked " << id << " with pid " << pid << ", script: \""
				<< new_command << "\"" << std::endl;
		pid_mod[pid] = id;
		mod_pid[id] = pid;
	}

}

void Executor::killall()
{
	for (std::map<pid_t, std::string>::iterator i = pid_mod.begin(); i != pid_mod.end(); ++i)
		kill_flowvr(i->first, SIGINT);
}

int Executor::waitchild()
{
	int status = 0;
	return ::wait(&status);
}


//////////////////////////////////////////////////////////////////////////////////////
// HISTORY
//////////////////////////////////////////////////////////////////////////////////////

void BasicController::History::append( const CmdHistory &element )
{
	flowvr::ipc::ScopedMTLock l(lock, "history-append");
	history.push_back(element);
}

size_t BasicController::History::getHistorySize() const
{
	flowvr::ipc::ScopedMTLock l(lock, "history-append");
	return history.size();
}

const BasicController::History::CmdHistory &BasicController::History::operator[](int index) const
{
	flowvr::ipc::ScopedMTLock l(lock, "history-append");
	return history[index];
}

BasicController::History::CmdHistory &BasicController::History::operator[](int index)
{
	flowvr::ipc::ScopedMTLock l(lock, "history-append");
	return history[index];
}

void BasicController::History::clear()
{
	flowvr::ipc::ScopedMTLock l(lock, "history-append");
	history.clear();
}

void BasicController::History::print() const
{
	std::for_each( history.begin(), history.end(), _printHistEl() );
}

//////////////////////////////////////////////////////////////////////////////////////
// BASICCONTROLLER
//////////////////////////////////////////////////////////////////////////////////////

BasicController::~BasicController()
{
       _AbortThread.wait();
	Executor::killall(); // module list from   run.xml are launched by separate bash processes. Kill them for a clean stop. 
        std::for_each( m_mpCommands.begin(), m_mpCommands.end(), _deleteOp() );
	delete stamps;
	delete ctrlin;
	delete ctrlout;
}

void BasicController::replaceHost(flowvr::xml::DOMElement* elem)
{
	if (destMap.empty())
		return;

	std::string host = elem->getTextContent();
	HostMap::iterator entry = destMap.find(host);
	if (entry != destMap.end())
	{
		std::string newhost = entry->second;
		std::cout << "Mapping host " << host << " to " << newhost << std::endl;
		elem->Clear();
		elem->LinkEndChild(new flowvr::xml::DOMText(newhost));
	}
	else
		std::cout<<"NO MAPPING FOR HOST "<<host<<std::endl;
}

void BasicController::replaceHosts(flowvr::xml::DOMElement* root)
{
    if (!strcmp(root->getNodeName(), "dest"))
        replaceHost(root);
    else
    {
        flowvr::xml::DOMNodeList* dests = root->getElementsByTagName("dest");
        for (int i=0; i<dests->getLength(); i++)
        {
            flowvr::xml::DOMElement* elem=(flowvr::xml::DOMElement*)dests->item(i);
            replaceHost(elem);
        }
        delete dests;
    }
}


std::string BasicController::getDest() const
{
	return dest;
}

void BasicController::setDest( const std::string &sdest )
{
	dest = sdest;
}

int BasicController::toggleVerboseResponse()
{
	bVerboseResponse = !bVerboseResponse;
	return -1;
}

bool BasicController::init(const std::string &prefixFile_)
{
    lastCompletedCmd = -1;
    lastSentCmd      = -1;
    prefixFile       = prefixFile_;
    stopped.set(0);//atmoic set
    
    //Build the parentName  
    //  Used to set FLOWVR_PARENT and as a reply value used in each message/command sent from this basicController that manage the app  to deamons.
    // parentName has typically the form of "/localhostname/localprocessPID/read:P" (read:P -> name of ctrlin + output port name)
    parentName = "";
    char buf[1024];

    // Prefix is the hostname where the app is started
    if ( !OptModuleReplyIp.value().empty())
            parentName +=  OptModuleReplyIp.value(); // In some case the use needs to provide this name as gethosname returns a value that is not visible from distant nodes.
    else if (!gethostname(buf,sizeof(buf)-1))
      {
	    buf[sizeof(buf)-1]='\0';
            parentName += buf;
      }
    
    //Middlefix is user provided or pid of process that starts the app
    parentName += "/";
    if ( !OptParentName.value().empty() )
       {
	   parentName += OptParentName.value();
      }
    else
      {
	  snprintf(buf,sizeof(buf)-1,"%d",getpid());
	  buf[sizeof(buf)-1]='\0';
	  parentName += buf;
      }
    //Prefix value is added after creating the ctrin controller, done a few lines bellow in this very same method. 
    
    if (parentName.empty())
    {
        std::cerr<<"No parent name"<<std::endl;
        return false;
    }
    
    ctrlin  = flowvr::ModuleAPIFactory::registerController(parentName,"read");
    ctrlout = flowvr::ModuleAPIFactory::registerController(parentName,"send");

    if (ctrlin==NULL)
    {
        std::cerr << "read controller creation failed."<<std::endl;
        return false;
    }

    if (ctrlout==NULL)
    {
        std::cerr << "send controller creation failed."<<std::endl;
        return false;
    }

    
    pIn = new flowvr::InputPort("P", new flowvr::StampListControl, true);
    std::vector<flowvr::Port*> inports;
    inports.push_back(pIn);
    if (!ctrlin->init(inports))
    {
        std::cerr << "read controller init failed."<<std::endl;
        return false;
    }


    std::cout << "parentname="<<parentName<<std::endl;

    // Add Prefix to parentName: adding the controller name and output port ("/read:P").
    parentName = ctrlin->getPortID(pIn);
    //reply=parentName;
    std::cout << "parentname="<<parentName<<std::endl;

    pOut = new flowvr::OutputPort("P", new flowvr::StampListControl, true);
    std::vector<flowvr::Port*> outports;
    outports.push_back(pOut);
    if (!ctrlout->init(outports))
    {
        std::cerr << "send controller init failed."<<std::endl;
        return false;
    }


    addCommand( Commands::CommandStruct( new AdvControllerCommand(&BasicController::receiveResult, this), "result", "", "", true ) );
    addCommand( Commands::CommandStruct( new AdvControllerCommand(&BasicController::receiveNews, this), "news"  , "", "", true ) );

    return true;
}

    void BasicController::printLast()
    {
        if (lastCompletedCmd<lastSentCmd)
        {
	  std::cout << "Last pending command : ";
            std::cout << "\r number=["
            		  << lastCompletedCmd+1
		      <<"]  destination: "
                      << history[lastCompletedCmd+1].dest
                      << termClearEndOfLine;

            const std::string& cmd = history[lastCompletedCmd+1].cmd;

            std::string::size_type id = cmd.find("id=\"", 0);
            if (id != std::string::npos)
                std::cout << ' '<< std::string(cmd, id+4, cmd.find('\"', id+4)-(id+4));
	    _printHistEl()(history[lastCompletedCmd+1]);
            std::cout << std::flush;
        }
    }

    int BasicController::run()
    {
        if (OptLogOut.count>0)
        {
            std::string fname = (std::string)OptLogOut;
            fname+="results";
            flog = creat(fname.c_str(), 0666);
        }

        while (ctrlin->wait())
        {

            flowvr::Message m;
            ctrlin->get(pIn, m);

            if (!m.data.valid())
            {
	      std::cerr<<"Empty Message."<<std::endl;
	      continue;
            }

            std::string text((std::string)m.data);
            flowvr::xml::DOMParser parser;

			if (parser.parseString(text.c_str()))
			{
			  std::cout<<"XML Error in message \"" << text << "\":"
				   << std::endl
				   << parser.getDocument()->ErrorDesc()
				   << std::endl;
			}
			else
			{
				flowvr::xml::DOMElement* root = parser.getDocument()->RootElement();
				Commands::CommandStruct cs = getCommand( root->getNodeName() );
				bool bFail = false;
				if( cs.command )
					cs.command->exec( root, m );
				else
					bFail = true;

				if( bFail || bVerboseResponse )
				{
					std::string source;
					m.stamps.read((*stamps).source, source);
					std::cout << "Received from " << source << ":" << text << std::endl;
				}
			}
        }
        ctrlin->close();
	std::cout<<std::flush;
	if(flog > 0)
        	close(flog);

        return 0;
    }

void BasicController::addCommand( const Commands::CommandStruct &cs )
{
	m_mpCommands[cs.name] = cs;
}

BasicController::Commands::CommandStruct BasicController::getCommand(const std::string &sName) const
{
	Commands::CMDS::const_iterator cit =  m_mpCommands.find(sName);
	if( cit == m_mpCommands.end() )
		return Commands::CommandStruct();

	return (*cit).second;
}

void BasicController::print_modules(BasicController *bc) {
  std::cout << "all connected modules: [";
  for(std::set<std::string>::const_iterator it=bc->modules.begin(); it!=bc->modules.end(); it++) {
    std::cout << *it << " " ;
  }
  std::cout << "]\n";
}


int BasicController::receiveNews( flowvr::xml::DOMElement *root, const flowvr::Message &m )
{
	std::string id( notNULL( root->Attribute("id") ) );
	flowvr::xml::DOMNodeList* mods = root->getElementsByTagName("moduledescription");
	if( id == "newmodule")
	{
		for (int i=0; i<mods->getLength(); i++)
		{
			flowvr::xml::DOMElement *mod=(flowvr::xml::DOMElement*)mods->item(i);
			std::cout << "News: new module "
                                  << notNULL(mod->Attribute("mid"))
                                  << " at "
                                  << notNULL(mod->Attribute("host"))
                                  << (allExpectedModules.count(notNULL(mod->Attribute("mid"))) == 0 ? " UNEXPECTED!" : "" )
                                  << std::endl;

			std::string sModule = notNULL(mod->Attribute("host"))
								+ std::string("/")
								+ notNULL(mod->Attribute("mid"));
			modules.insert(sModule);
		}
	}
	else if( id == "purgemodule" )
	{

		for (int i=0; i<mods->getLength(); i++)
		{
			flowvr::xml::DOMElement* mod=(flowvr::xml::DOMElement*)mods->item(i);
			std::string sModule = notNULL(mod->Attribute("host"))
								+ std::string("/")
								+ notNULL(mod->Attribute("mid"));
			std::cout << "News: Module: " << sModule << " closed" << std::endl;
			modules.erase( sModule );

		}


		if (OptVerbose.count>=1)
		  {
		    if(modules.empty())
		      {
			std::cout << "News: No module left\n";
		      }
		    else
		      {
			std::cout  <<"News: "<< modules.size() <<" modules still running: " <<"\n";
			for(std::set<std::string>::const_iterator cit = modules.begin(); cit != modules.end(); ++cit )
			  {
			    std::cout <<"-- "<< *cit << std::endl;
			  }
		      }
		  }
	}
	else if ( id == "abort" )
	{

	  for (int i=0; i<mods->getLength(); i++)
	    {
	      flowvr::xml::DOMElement* mod=(flowvr::xml::DOMElement*)mods->item(i);
	      std::string sModule = notNULL(mod->Attribute("host"))
		+ std::string("/")
		+ notNULL(mod->Attribute("mid"));
	      std::cout << "News: Abort from module: "<<sModule<<"-> stop the full application"<<std::endl;
	    }

	  // cannot stop right here using 'this->stopCommand()'
	  // since BasicController::run() loop is stalled by the current call
	  // USe a dedicated thread instead:
	  _AbortThread.start();
	  this->yield();//Give up current time slice to favor a quick execution of the abort.
	}
     return 0;
}





void BasicController::stopOnSignal() 
	{
	  {
	    flowvr::ipc::ScopedMTLock l(lock, "stop");
	    stopSignal.wait(lock);
	  }
	}


void BasicController::startInteractiveThread() 
	{
	  _InteractiveThread.start();
	  _InteractiveThread.detach(); // Detach thread to avoid being suspended on command readline when stopping the application
	}

		
	

int BasicController::receiveResult( flowvr::xml::DOMElement *root, const flowvr::Message &m )
{
	std::string text( (std::string)m.data );

	int it = atoi(notNULL(root->Attribute("id")));

	if (it<=lastCompletedCmd or it>lastSentCmd)
	{
		std::string source;
		m.stamps.read((*stamps).source, source);
		std::cout
			<<"Received result with invalid id from "
			<<source<<":"<<text<<std::endl;
	}
	else
	{
		bool ok = (std::string( notNULL(root->Attribute("status")) ) == std::string("OK"));

		if (!ok)
		{
			std::cerr << "ERROR at " << it
					  << " ("  << history[it].cmd
					  << "): " << text
                     << std::endl;
		}
		else
		{
			flowvr::xml::DOMElement* child = root->FirstChildElement();

			if (child!=NULL && !strcmp(child->getNodeName(), "ping"))
			{
				flowvr::xml::DOMElement time("receive");
				flowvr::Trace::cycle_t cycle;
				flowvr::Trace::readCycle(&cycle);
				time.SetAttribute("sec", cycle.tv_sec);
				time.SetAttribute("usec", cycle.tv_usec);
				child->InsertEndChild(time);
				text = flowvr::xml::DOMWriter::toString(root);
			}
			if (OptVerbose.count>=2)
				std::cout << "Command " << it << " OK" << std::endl;
		}

		if (OptLogOut.count>0&& flog!=-1)
		{
			write(flog, text.c_str(), text.length());
			write(flog, "\n", 1);
		}

		history[it].reply = text;
		history[it].ok    = ok;

		bool print=false;
		while( history.getHistorySize() > lastCompletedCmd+1 and history[lastCompletedCmd+1].reply.size() > 0 )
		{
			{
				flowvr::ipc::ScopedMTLock locker(lock, "ctrlin");
				++lastCompletedCmd;
			}

			signal.notify();
			print = true;
		}


		if (print && OptVerbose.count>=1)
			printLast();

		if( history[it].showReply )
			_printHistEl()( history[it] );

	}
	return 0;
}

int BasicController::flush()
{
    flowvr::ipc::ScopedMTLock locker(lock, "ctrlout");
    
    if (lastCompletedCmd<lastSentCmd)
    {
        if (OptVerbose.count>=1)
        {
            std::cout << "Flush waits for command: ";
            printLast();
            std::cout << std::endl;
        }
 
        while (lastCompletedCmd<lastSentCmd)
        {
            struct timeval maxtime; 
            gettimeofday(&maxtime, NULL);
            struct timespec specmaxtime;
            TIMEVAL_TO_TIMESPEC(&maxtime, &specmaxtime);
            specmaxtime.tv_sec += 2; // Warning if more 2 seconds passed and no answer...
	    // Put thread to wait for signal from ctrlin  or for 2 s, release lock. If timeoutreached mean that no new command was aknoledged.
	    if(signal.timedwait(lock, &specmaxtime))
            {
                std::cout << "WARNING: Pending flush - Some  commands are not yet acknowledged by daemons" << std::endl;
                printLast();
                std::cout << std::endl;
            }
        }

        if (OptVerbose.count>=1)
        {
            std::cout << "Flush done: command ack received.";
            std::cout << std::endl;
        }

    }

    return -1;
}

std::vector<int> BasicController::groupCommandAll(const std::string &command)
{

    //building command to send
    std::string text = "<group>"+command+"</group>";

    std::vector<int> liRes;
    std::for_each( vechost.begin(), vechost.end(), _sendFunctor( this, text, liRes ) );

    //need a flush to ensure that all the commands have been completed
    flush();
    return liRes;
}

int BasicController::stopCommand()
{
        //Do not re-execute - stopped is atomic as stopCommand  can be called concurrently from 2 different threads (ommand line thread and abort thread)
       if (stopped > 0 ) return 1;
       stopped.inc(); //Atomic inc

       	//need a pause to ensure that the dataflow is stopped
	if (!pause())
		return 0;
	else
	{
	  //if the pause is ok then send the commands to delete each route, then each object
	  //	  std::vector<int> listid;
	  //	  std::map<std::string,int> listcomp;

	  groupCommandAll("<delroute  id=\"\"/>");
	  //	  processReply("", 0, listid, listcomp, false );


	  groupCommandAll("<delobject id=\"\"/>");
	  //	  processReply("", 0, listid, listcomp, false );


	  // Stop the ctrlin and ctrlout module (wait will turn false)
	  history.clear();
	  ctrlin->close();
	  ctrlout->close();
	  this->wait();//wait for thread related to ctrlin to stop 
	  
	  //tell flowvr that we are done
	  stopSignal.notify();
	  return 1;
	}
}

int BasicController::startCommand()
{
    //just need a start command as action
    groupCommandAll("<action id=" "><start/></action>");
    return ctrlout->getStatus();
}

int BasicController::processFile(const std::string &fileName)
{
    flowvr::xml::DOMDocument file;
    if (! file.LoadFile(fileName) )
    {
    	std::cout << "Error:  failed to open file: " << fileName << std::endl;
    	return 0;
    }
    else
    {
        flowvr::xml::DOMElement* e   = file.RootElement();
        flowvr::xml::DOMElement* cmd = e->FirstChildElement();

        while (cmd!=NULL)
        {
                processCommand(cmd);
                cmd = cmd->NextSiblingElement();
        }
    }
    return ctrlout->getStatus();
}

int BasicController::trace()
{
    std::string fileName = prefixFile + ".prolog.xml";
    return processFile( fileName );
}

int BasicController::notrace()
{
    std::string fileName = prefixFile + ".epilog.xml";
    return processFile( fileName );
}

int BasicController::pause()
{
    std::string text;

    // Need a list id to find the good commands/results in the History vector
    std::vector<int> listid;

    // Empty id: all the object created via this controller are concerned by this command
    listid= groupCommandAll("<action  id=\"\"><pause/></action>");

    std::map<std::string,int> listcomp;
    processReply( text, 0, listid, listcomp, true );

    // need to call the getroutecount on each routingtable,
    // get the answers and delete the matching pairs source count in
    // the list, if all the route are ok (i.e their count is equal to
    // the count related to the same source in the comparison list)
    // then the pause is done, else recall getroutecount on each routing table

    bool pause = false;

    int loop = 0;
    //while the comparisons list is not empty
    while (!pause)
    {
        //need a list id to find the good commands/results in the History vector
        std::vector<int> listcom;
	listcom= groupCommandAll("<getroutecount  id=\"\"/>");
        bool complete = processReply( text, loop, listcom, listcomp, false );

        //if every route is ready for the pause
        if (complete)
            pause = true;

        ++loop;
    } //the comparisons list is updated

    // pause is done!
    return 1;
}

// Used for pause command
// Two modes: insertion (bFillMode=true) and test (bFIllMode=false)
bool BasicController::processReply(const std::string &text, int loop, const std::vector<int> &id, std::map<std::string,int> &idmap, bool bFillMode )
{
	bool complete = true;
	//	std::vector<int>::const_iterator listitid = id.begin();

	for( std::vector<int>::const_iterator listitid = id.begin(); listitid != id.end(); ++listitid )
	  {
	    std::string rep = history[*listitid].reply;
	    
	    flowvr::xml::DOMParser parser;
	    if (parser.parseString(rep.c_str()))
	      {
		std::cerr<< "XML Error in command \""
            		 << text
            		 << "\":"
			 << parser.getDocument()->ErrorDesc()
			 << std::endl;
		continue;
	      }
	    else
	      {
		int count;
		std::string source;
		
		flowvr::xml::DOMElement* rootrep = parser.getDocument()->RootElement();
	    
		// each message sent element is put in this list
		flowvr::xml::DOMNodeList* listmessagesent = rootrep->getElementsByTagName("messagesent");
		
		for (int i=0; i<listmessagesent->getLength(); i++)
		  {
		    //get the element
		    flowvr::xml::DOMNode* dom= listmessagesent->item(i);
		    flowvr::xml::DOMElement *elem = ((flowvr::xml::DOMElement*)dom);
		    
		    //get the source and count attribute
		    source = elem->Attribute("source");
		    count  = atoi(elem->Attribute("count"));
		    
		    if( bFillMode )
		      {
                	idmap.insert(std::map<std::string,int>::value_type(source, count));
			if (OptVerbose.count>=2)
			  std::cout << "processReply - source : "
				    << source
				    << " count : "
				    << count
				    << std::endl;
		      }
		    else
		      {
                	//search the pair<source,count> with the same source
			std::map<std::string,int>::iterator comp = idmap.find(source);
			
			if (comp==idmap.end())
			  {
			    if (OptVerbose.count>=2 || loop>=2)
			      std::cerr
				<< "Warning processRely -  no comparison possible with : "
				<< source << std::endl;
			  }
			else if (comp->second!=count)
			  {
			    if (OptVerbose.count>=2 || loop>=2)
			      std::cout << "processReplay -  Source ID : " << source
					<< " count : " << count
					<< " differs from :  " << comp->second
					<< std::endl;
			    complete = false;
			  }
		      }
		  }
		
	      } //end of the construction of the comparisons list for one result
	  }
	return complete;	
}

int BasicController::sendCommand(const std::string &text, const std::string &destination, bool bShowReply )
{
	std::string towhom;
	if( !destination.empty() )
		towhom = destination;
	else
		towhom = dest;

	if (OptSync)
		flush();

	{
		flowvr::ipc::ScopedMTLock locker(lock, "ctrlout");
		++lastSentCmd;
	}

	history.append( History::CmdHistory( towhom, text, "", false, bShowReply ) );

	if (ctrlout->wait()==0)
		return 0;

	std::vector<std::string>::const_iterator cit = std::find_if( vechost.begin(), vechost.end(),  _nameMatch(towhom) );
	if( cit == vechost.end() )
		vechost.push_back(towhom);

	flowvr::MessageWrite m;
	// Build data
	m.data = ctrlout->alloc(text.length());
	memcpy(m.data.writeAccess(), text.c_str(), text.length());


	// Build stamps
	m.stamps.write((*stamps).dest, towhom);
	m.stamps.write((*stamps).reply, parentName);


	// Send message
	if (ctrlout->put(pOut, m)==0)
		return 0;

	int it;
	m.stamps.read((*stamps).it, it);


	if (OptVerbose.count>=2)
		std::cout << "Sent command nb="<< it
		          << ": "
		          << text
		          << " to dest="<< towhom
                          << std::endl;

	return it;
}


int BasicController::processCommand(const std::string &input, bool bShowReply ) {


    if (input.empty())
        return 0;

    std::string text;

    // If user typed a command, check whether it exists. If so turn it into an xml command
    if(input[0] != '<')
    {
    	// test  if it is an existing user command ( probe shortcuts too )
    	Commands::CommandStruct s = commands.getCmd(input, true);
    	if( s.command != NULL )
    	{
			// Get the long name version
			text = "<"+s.name+"/>";
		}
        else
		{
			std::cerr<<input<<": Command not found"<<std::endl;
			return 0;
		}
    }

    if( text.empty() )
    	text = input;

    // we skip direct xml phrases

    if( text.substr(0,5) == "<?xml" )
		return 0;
	if( text.substr(0,10) == "<commands>" )
		return 0;
	if( text.substr(0, 11) == "</commands>" )
		return 0;
	if( text.substr(0, 9 ) == "<!DOCTYPE" )
		return 0;

    flowvr::xml::DOMParser parser;
    if (parser.parseString(text.c_str()))
    {
        std::cerr<< "XML Error in command \"" << text << "\":"
                 << parser.getDocument()->ErrorDesc() << std::endl;
        return 0;
    }
    else
    {
        flowvr::xml::DOMElement* root = parser.getDocument()->RootElement();
        return processCommand(root, bShowReply);
    }
}

int BasicController::processCommand(flowvr::xml::DOMElement* root, bool bShowReply )
{
    replaceHosts(root);
    std::string text = flowvr::xml::DOMWriter::toString(root);

	Commands::CommandStruct cmd = commands.getCmd( root->getNodeName(), true );
	if( cmd.command )
	{
		int ret = cmd.command->exec( root );
		if( ret == -1 )
			return ctrlout->getStatus();
		else
			return ret;
	}
	else
		sendCommand(text, "", bShowReply);

    return ctrlout->getStatus();
}

int BasicController::printHistory()
{
	history.print();
	return -1;
}


//////////////////////////////////////////////////////////////////////////////
//////////////////////////////////////////////////////////////////////////////


BasicController::Commands::~Commands()
{
	std::for_each( commandsmap.begin(), commandsmap.end(), _deleteOp() );
}

void BasicController::Commands::init(BasicController *parent)
{
	add("help","h","Print this help", new HelpCmd( this ) );

	add("start","go","Start or resume (after pause) application", new ControllerCommand( &BasicController::startCommand, parent ) );
	add("pause","p","Suspend application", new ControllerCommand( &BasicController::pause, parent ));
	add("stop","s","Stop  application", new ControllerCommand( &BasicController::stopCommand, parent ) );
	add("trace","t","Start event capture for application tracing", new ControllerCommand( &BasicController::trace, parent ));
	add("notrace","nt","Stop event catpure", new ControllerCommand( &BasicController::notrace, parent ) );
	add("history","hi","Show command history", new ControllerCommand( &BasicController::printHistory, parent ));
	add("verbose", "vr", "Toggle response verbosity", new ControllerCommand( &BasicController::toggleVerboseResponse, parent ) );


	add("run", "r", "Run command in shell", new RunCommand, true );
	add("mapping", "mp", "Add Host mapping", new AddHostMapping(parent), true);
	add("dest", "d", "Set destination string", new SetDestination(parent), true);
	add("flush", "f", "Flush pending messages", new ControllerCommand( &BasicController::flush, parent ), true );
	add("killall", "ka", "Kill all executed processes", new KillAll );
	add("waitchild", "wc", "Wait for a child process", new WaitChildCommand, true );
	add("wait", "w", "Wait for a number of seconds", new WaitCommand);

}

void  BasicController::Commands::add(const std::string &name,
                                     const std::string &shortcut,
                                     const std::string &help,
                                     Command *cmd, bool _intern )
{
	commandsmap[name] = CommandStruct( cmd, name, shortcut, help, _intern );
}

bool  BasicController::Commands::is(const std::string &input, const bool shortcut) const
{
	return ( std::find_if( commandsmap.begin(), commandsmap.end(), _namePred( input, shortcut ) ) != commandsmap.end() );
}


BasicController::Commands::CommandStruct BasicController::Commands::getCmd( const std::string &input, const bool shortcut ) const
{
	CMDS::const_iterator cit = std::find_if( commandsmap.begin(), commandsmap.end(), _namePred( input, shortcut ) );
	if( cit == commandsmap.end() )
		return CommandStruct();

	return (*cit).second;
}

void   BasicController::Commands::printHelp() const
{
	std::for_each( commandsmap.begin(), commandsmap.end(), _printEl(false) );
}

void BasicController::Commands::CommandStruct::print() const
{
    std::cout << "\t" << name <<"\t" << shortcut << "\t: " << help << std::endl;
}
