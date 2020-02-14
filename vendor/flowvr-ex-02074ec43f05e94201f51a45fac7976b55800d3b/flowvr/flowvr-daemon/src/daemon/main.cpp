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
* File: src/daemon/main.cpp                                       *
*                                                                 *
* Contacts:                                                       *
*  12/10/2003 Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "flowvr/mem/daemonsharedmemorymanager.h"
#include "flowvr/mem/sharedmemoryarea.h"
#include "flowvr/daemon.h"
#include "flowvr/plugins/dispatcherobject.h"
#include "flowvr/plugd/dispatcher.h"
#include "flowvr/config.h"

#include "flowvr/utils/backtrace.h"
#include "flowvr/utils/debug.h"
#include "flowvr/utils/cmdline.h"
#include "flowvr/utils/size.h"

#include <iostream>
#include <sstream>
#include <signal.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/select.h>


using namespace flowvr;
using namespace flowvr::mem;
using namespace flowvr::plugd;
using namespace flowvr::plugins;
using namespace flowvr::xml;

/*
#define FLOWVR_STR(f) #f
#define FLOWVR_STR2(f) FLOWVR_STR(f)
// Was previously used to generate a string from a #define
*/

flowvr::utils::Option<std::string> OptName      ("name"      , 'n' , "Set the daemon internal name", std::string("FlowVR-Daemon ") + std::string(FLOWVR_VERSION_REVISION),false);
flowvr::utils::Option<int>         OptMemID     ("memid"     , 'm' , "Shared memory ID", DefaultMemId, false);
flowvr::utils::Option<utils::Size> OptMemSize   ("memsize"   , 's' , "Size of each shared memory segment in bytes, in Kb (K), in Mb (M) or, finally in Gb (G)", "4M",false);
flowvr::utils::Option<unsigned>    OptMemNbr    ("memnbr"    , 'S' , "Maximum number of Shared memory segments",1U,false);
flowvr::utils::Option<std::string> OptDispatcher("dispatcher", '\0', "Dispatcher class", std::string("flowvr.plugins.DefaultDispatcher"),false);
flowvr::utils::Option<std::string> OptCommander ("commander" , '\0', "Commander class", std::string("flowvr.plugins.AutoCommander"),false);
flowvr::utils::Option<std::string> OptNet       ("network"       , '\0', "NET class (flowvr.plugins.NetMPI or flowvr.plugins.NetMPIm for MPI)", std::string("flowvr.plugins.NetTCP"),false);
flowvr::utils::Option<std::string> OptNetOld    ("net"       , '\0', "NET class (old option)", std::string("<deprecated-option>"));
flowvr::utils::FlagOption          OptKeepSHM   ("keep"      , 'k' , "Keep shared memory in memory (for debugging)");
flowvr::utils::FlagOption          OptVerbose   ("verbose"   , 'v' , "Increase verbose level (display more messages)");
flowvr::utils::Option<int>         OptTop       ("top"       , 't' , "Display 'top'-like informations every Int seconds",5);
flowvr::utils::FlagOption          OptFile      ("file"      , 'f' , "Avoid special symbols in outputs");

std::string getcmd()
{
  static char userCommand[16384];
  do
  {
    if (!std::cin.good())
    {
      std::cerr << "Error on c++ stdin"<<std::endl;
      return std::string();
    }
    std::cout <<'$';
    std::cin.getline(userCommand,sizeof(userCommand));
    userCommand[sizeof(userCommand)-1]='\0';
  }
  while (userCommand[0]=='\0');
  std::string res;
  if (userCommand[0]!='<')
  {
    res+="<";
    res+=userCommand;
    res+="/>";
  }
  else res = userCommand;
  return res;
}

static Object* createObject(const std::string& id, std::string desc, Dispatcher* dispatcher, const std::string& parentclass="", xml::DOMNode* param = NULL)
{
  DOMDocument doc;
  std::string::size_type param_pos = desc.find('(');
  if (param_pos != std::string::npos)
  {
    std::string param(desc,param_pos+1,desc.size()-1-(param_pos+1));
    param = "<addobject>"+param+"</addobject>";
    desc = desc.substr(0,param_pos);
    doc.Parse(param.c_str());
  }
  else
  {
    doc.Parse("<addobject/>");
  }
  if (param!=NULL)
    doc.RootElement()->InsertEndChild(*param);

  doc.RootElement()->SetAttribute("id",id);
  doc.RootElement()->SetAttribute("class",desc);

  std::cout << xml::DOMWriter::toString(doc.RootElement())<<std::endl;

  Class* myClass = Class::find(desc);
  if (myClass==NULL)
    return NULL;
  if (parentclass.size()>0)
  {
    if (!myClass->derive(parentclass))
    {
      std::cerr<<"Class "<<myClass->name()<<" does not derive from "<<parentclass<<std::endl;
      return NULL;
    }
  }
  Object* myObject = myClass->construct(id);
  if (myObject==NULL)
    return NULL;

  Result res = myObject->init(doc.RootElement(),dispatcher);
  std::cout << id << ":" << res.toString() << std::endl;
  if (res.error())
  {
    myObject->close(NULL,dispatcher);
    return NULL;
  }
  return myObject;
}


// Terminal VT100 special code
// TODO: ifdef for other platforms or use external libs (like ncurse)

const char* termCursorSave = "\e7";
const char* termCursorRestore = "\e8";
const char* termCursorHome = "\e[H";
const char* termClearScreen = "\e[2J";
const char* termClearEndOfScreen = "\e[0J";
const char* termClearBeginningOfScreen = "\e[1J";
const char* termClearEndOfLine = "\e[K";

static std::vector<std::string> statusLines;
static void statusAdd(const std::string& s)
{
  statusLines.push_back(s);
  std::cout << s << termClearEndOfLine << '\n';
}

static void statusClear()
{
  for (unsigned int i=0;i<statusLines.size();i++)
  {
    for (unsigned int c=0;c<statusLines[i].length();c++)
      std::cout << ' ';
    std::cout << termClearEndOfLine << '\n';
  }
  std::cout << termCursorHome;
  statusLines.clear();
}

static void initStatus()
{
  std::cout << termClearScreen;
}

static void printStatus(const std::string &filter)
{
  static std::vector<std::string> stats;
  std::cout << termCursorSave << termCursorHome;

  statusClear();
  
  statusAdd( OptName.value() );
  
  DaemonSharedMemoryManager * dshmman = DaemonSharedMemoryManager::instance();
  if ( dshmman )
  {
      std::vector< std::string > shmStatus = dshmman->status();
      // put in status
      for ( std::vector< std::string >::const_iterator
            it = shmStatus.begin()  ;  it != shmStatus.end()  ;  ++it )
      {
          statusAdd( *it );
      }
  }

  std::vector<std::string> objects = flowvr::plugd::Object::getListObject("/");
  for (unsigned int i=0;i<objects.size();i++)
  {
    flowvr::plugd::Object* o = flowvr::plugd::Object::find(objects[i]);
    if (o!=NULL) // Object could have been deleted between getListObject and find calls
    {
    	if(!filter.empty() )
    	{
    		std::string state = o->status();
    		if ( state.find( filter ) != std::string::npos )
    			statusAdd(state);
    	}
    	else
    		statusAdd(o->status());

    }
  }
  std::cout << termClearEndOfLine << '\n';

  std::cout << termCursorRestore;
  std::cout << std::flush;
}

static void clean(void)
{
  static bool done=false;
  if (done) return;
  done=true;

  DaemonSharedMemoryManager * dshmman = DaemonSharedMemoryManager::instance();
  if ( dshmman )
  {
      SharedMemoryArea *mainArea = dshmman->getMainMemoryArea();
      
      int headpos = mainArea->readHeader();
      if ((unsigned)headpos<(unsigned)mainArea->getSize())
          *mainArea->getWrite<int>(headpos) = 0xDEADADDE;

      if ( !OptKeepSHM )
          dshmman->releaseMemory();
      Allocator::the()->detach();
  }
}

static volatile sig_atomic_t gs_done = 0;

/**
 * @brief handler called when catching some signals
 *
 * This handler only ask the main loop to terminate before rearming the signal
 *  with the default handler.
 *
 * @note a signal handle must perform async-safe stuff only
 *
 * @param sig the signal cough
 */
static void handler(int sig)
{
  gs_done = 1;  // ask the main loop to stop
  // let the main loop quit normally unless the same signal is cough once again
  signal(sig,SIG_DFL);
}
/**
 * @brief handler called when catching some signals
 *
 * This handler only ask the main loop to terminate before rearming the signal
 *  with itself.
 *
 * @note a signal handle must perform async-safe stuff only
 *
 * @param sig the signal cough
 */
static void handleGentleQuit(int sig)
{
    gs_done = 1;
    signal(sig,handleGentleQuit);
}


int main(int argc, char** argv)
{    
    DaemonInterface::setMainArguments( &argc, &argv );

  std::cout << "FlowVR Daemon version "<< OptName.get();
#ifdef FLOWVR_USE_MPTHREAD
  std::cout << " (MPThread)";
#endif
  std::cout << " (" << flowvr::ipc::MPAtomicInt::getImplName()<<" atomic)"
		    << " -- (" << flowvr::ipc::MPAtomicIntBw::getImplName() << " bw-atomic) "
		    << std::endl;

  if( OptMemID.count == 0 )
	  OptMemID = DaemonInterface::getDaemonID();
  else
	  DaemonInterface::setDefaultDaemonId(OptMemID.value(), true);

  flowvr::utils::CmdLine cmd;
  bool error=false;
  if (!cmd.parse(argc,argv,&error))
  {
	std::cout << "Error during command line parsing." << std::endl
			  << cmd.help() << std::endl;
    return error?1:0;
  }

  if (!cmd.args.empty())
  {
    std::cerr << "Unrecognized arguments:";
    for (unsigned int i=1;i<cmd.args.size();i++)
      std::cerr << " " << cmd.args[i];
    std::cerr << std::endl;
  }

  flowvr::daemon::verboseLevel = OptVerbose.count;

  //Check if we have to disable all terminal specific characters
  if (OptFile.count>0){
    termCursorSave = "";
    termCursorRestore = "";
    termCursorHome = "";
    termClearScreen = "";
    termClearEndOfScreen = "";
    termClearBeginningOfScreen = "";
    termClearEndOfLine = "";
  }
  if (OptTop.count>0)
    initStatus();
  if (OptMemNbr.value() == 0)
    OptMemNbr = 1;

  //////////////////////////////////////////////////////////////////////
  //        Set up shared memory allocator
  //////////////////////////////////////////////////////////////////////

  int cmdtablesize = 512; // arbitrary number ;)

  DaemonSharedMemoryManager *shmman = new DaemonSharedMemoryManager(
        OptMemSize.value(), OptMemNbr.value(), OptMemID.value(), cmdtablesize );
  Allocator::setAllocator( shmman );
  Allocator::the()->attach();
  
  SharedMemoryArea * mainArea = NULL;
  if ( NULL != DaemonSharedMemoryManager::instance() )
       mainArea = DaemonSharedMemoryManager::instance()->getMainMemoryArea();
      
  if ( NULL == mainArea )
  {
	  std::cerr << " ** ERROR -- could not create shared memory area." << std::endl;
	  return 2;
  }
  else
  {
      MPDaemonHeader* header = mainArea->getWrite<MPDaemonHeader>(mainArea->readHeader());
      header->init(cmdtablesize);
      header->versionMaj = 0;
      header->versionMin = 0;
      header->daemonName = OptName;
  }

  //////////////////////////////////////////////////////////////////////
  std::cout << std::endl << "Install signal handler" << std::endl;
  //////////////////////////////////////////////////////////////////////

  signal(SIGINT,handler);
  signal(SIGTERM,handler);
  signal(SIGSEGV, handler);
  signal(SIGQUIT, handleGentleQuit);
  atexit(clean);

  //////////////////////////////////////////////////////////////////////
  std::cout << std::endl << "Starting Dispatcher" << std::endl;
  //////////////////////////////////////////////////////////////////////

  Object* dispatcherObj = createObject("/Dispatcher",OptDispatcher,NULL,"flowvr.plugins.Dispatcher");

  if (dispatcherObj==NULL)
  {
	  std::cerr << " ** ERROR -- could not start dispatcher." << std::endl;
	  return 3;
  }

  Dispatcher* dispatcher = ((DispatcherObject*)dispatcherObj)->getDispatcher();

  //////////////////////////////////////////////////////////////////////
  std::cout << std::endl << "Starting Commander" << std::endl;
  //////////////////////////////////////////////////////////////////////

  {
    DOMElement root("memid");
    char buf[16];
    snprintf(buf,sizeof(buf),"%zd", (size_t) (OptMemID.value()));
    root.LinkEndChild(new DOMText(buf));
    Object* commanderObj = createObject("/Commander",OptCommander,dispatcher,"",&root);
    if (commanderObj == NULL)
    	return 4;

    DOMElement croot("root");
    croot.LinkEndChild(new xml::DOMElement("control"));
    dispatcher->setRemoteControlMessageHandler(commanderObj->createAction(&croot));

	//////////////////////////////////////////////////////////////////////
	std::cout << std::endl << "Starting NET ["<< OptNet.value() << "]" << std::endl;
	//////////////////////////////////////////////////////////////////////

	Object* netObj = createObject("/NET",OptNet,dispatcher);
	if (netObj==NULL)
		return 5;

	dispatcher->setControlMessageHandler(netObj->createAction(&croot));

	if(OptNetOld.count)
	{
		std::cout << "WARNING: using deprecated -net option, "
				  << "this has no effect, the option can safely be removed."
				  << std::endl;
	}
  }

  //////////////////////////////////////////////////////////////////////
  //        Main loop
  //////////////////////////////////////////////////////////////////////

  // When printing the top-like display, we use 'select()' to handle both the
  // user input in STDIN and a timer periodically triggering the display itself.
  if (OptTop.count>0)
  {
	fd_set set;
	struct timeval t;
	t.tv_sec = (int)OptTop;
	t.tv_usec = 0;

	std::string strFilter;

	while ( !gs_done )
	{
	  printStatus( strFilter );

	  FD_ZERO(&set);
	  FD_SET(1, &set);

      if(OptFile.count>0)
          sleep(5);
      else
      {
          if( select( 2, &set, NULL, NULL, &t ) == 1 )
          {
              // select will return either after timeout OR
              // CTRL-D (r==0) OR endline / return
              char buffer[4096]; // we assume that the user does not type too much :)

              char *r = fgets(buffer, 4096, stdin );
              if( r )
              {
                  switch( buffer[0] )
                  {
                  case '/':
                      strFilter = std::string( buffer+1, buffer+strlen(buffer)-1 );
                      break;
                  default:
                      break;
                  }
              }
              else
              {
                  strFilter = "";
              }
          }
          else
          {
              t.tv_sec = (int)OptTop;
          }
      }
	}
  }
  // The case of no top-like display is handled separately. 'select()' seems to
  // do nasty things when the application is in the backgroud of a terminal.
  else
  {
      while( !gs_done ) {
          // no more input. wait...
          sleep(3600);
      }
  }
  
  return 0;
}

