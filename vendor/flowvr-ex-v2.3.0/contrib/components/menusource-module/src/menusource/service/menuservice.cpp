/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       flowvr-petaflow
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
* File: menuservice.cpp                                    
* Contacts: assenmac                                     
*                                                                 *
******************************************************************/

#include "menuservice.h"
#include <cegrenderer/cegrenderer.h>
#include <menusourceconfig/menusourceconfig.h> // @todo not nice: inclusion from upper element with absolute path
#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUILogger.h>
#include <CEGUI/CEGUIExceptions.h>
#include <CEGUI/CEGUIWindow.h>
#include <CEGUI/CEGUIWindowManager.h>
#include <CEGUI/ScriptingModules/LuaScriptModule/CEGUILua.h>

#include <flowvr/portutils/portmodule.h>
#include <flowvr/moduleapi.h>
#include <flowvr/render/chunkrenderwriter.h>


#include <GL/glut.h> // for keycodes

using namespace CEGUI;
using namespace flowvr::portutils;
using namespace flowvr;
using namespace flowvr::utils;
using namespace flowvr::render;

DEFIMP_SERVICEPLUG(menuservice)


extern "C" void getParameters( flowvr::portutils::ARGS &args )
{
	args["LOGFILE"]     = Parameter("cegui.log", "Logfilename for cegui logger", Parameter::P_STRING, Parameter::PS_NONE, Parameter::ST_DEFAULT, Parameter::eMode( Parameter::MD_OPTIONAL | Parameter::MD_NONNULL ) );
	args["RESOURCECONF"] = Parameter("cegui.res", "XML resource configuration for menus", Parameter::P_STRING, Parameter::PS_NONE, Parameter::ST_DEFAULT, Parameter::eMode( Parameter::MD_OPTIONAL | Parameter::MD_NONNULL ) );
	args["LOGLEVEL"]    = Parameter("0", "0: disable, 4: insane", Parameter::P_NUMBER);
	args["PORTNAME"]    = Parameter("scene", "The port name to use for output", Parameter::P_STRING );

	args["SCRIPT"]      = Parameter(Parameter::P_STRING, Parameter::PS_NONE, "Initial script to load", Parameter::MD_OPTIONAL );
	args["WAITSCRIPT"]  = Parameter("false", "infinite wait before script execution", Parameter::P_BOOLEAN);
}

menuservice::menuservice( flowvr::portutils::Module &m )
: IServiceLayer(m)
, m_pSystem( NULL )
, m_pRenderer( NULL )
, m_config( NULL )
, m_scripts( &LuaScriptModule::create() )
, m_dtLast(0)
, m_nLogLevel(0)
, m_bWaitScript(false)
, m_bUpdate(false)
{
	setupKeyCodes();
}

menuservice::~menuservice()
{
	delete m_config;

	if( m_pSystem )
		CEGUI::System::destroy();

	LuaScriptModule::destroy( *m_scripts );

	delete m_pRenderer;
}

bool menuservice::setParameters( const flowvr::portutils::ARGS &args )
{
	try
	{
		m_sLogFile  = args("LOGFILE").getValue<std::string>();
		m_sMsXml    = args("RESOURCECONF").getValue<std::string>();

		int logLevel = args("LOGLEVEL").getValue<int>();
		m_nLogLevel = std::min<int>( Insane,
									  std::max<int>( 0, logLevel ) );

		m_strPortName = args("PORTNAME").getValue<std::string>();

		if( args.hasParameter("SCRIPT") )
			m_sIniScript = args("SCRIPT").getValueString();

		m_bWaitScript = args("WAITSCRIPT").getValue<bool>();
	}
	catch( ParameterException &e )
	{
		std::cerr << e.what() << std::endl;
		return false;
	}
	return true;
}

void menuservice::setupKeyCodes()
{
	smpKeyNormalCodes[0x27] = Key::Escape;
	smpKeyNormalCodes[0x08] = Key::Backspace;
	smpKeyNormalCodes[0x7f] = Key::Delete;
	smpKeyNormalCodes[0x0D] = Key::Return;
	smpKeyNormalCodes[0x09] = Key::Tab;


	smpSpecialCodes[GLUT_KEY_LEFT]  = Key::ArrowLeft;
	smpSpecialCodes[GLUT_KEY_RIGHT] = Key::ArrowRight;
	smpSpecialCodes[GLUT_KEY_UP]    = Key::ArrowUp;
	smpSpecialCodes[GLUT_KEY_DOWN]  = Key::ArrowDown;
	smpSpecialCodes[GLUT_KEY_HOME]  = Key::Home;
	smpSpecialCodes[GLUT_KEY_END]   = Key::End;
	smpSpecialCodes[GLUT_KEY_PAGE_UP]   = Key::PageUp;
	smpSpecialCodes[GLUT_KEY_PAGE_DOWN]   = Key::PageDown;
	smpSpecialCodes[GLUT_KEY_INSERT]   = Key::Insert;

	smpSpecialCodes[GLUT_KEY_F1]    = Key::F1;
	smpSpecialCodes[GLUT_KEY_F2]    = Key::F2;
	smpSpecialCodes[GLUT_KEY_F3]    = Key::F3;
	smpSpecialCodes[GLUT_KEY_F4]    = Key::F4;
	smpSpecialCodes[GLUT_KEY_F5]    = Key::F5;
	smpSpecialCodes[GLUT_KEY_F6]    = Key::F6;
	smpSpecialCodes[GLUT_KEY_F7]    = Key::F7;
	smpSpecialCodes[GLUT_KEY_F8]    = Key::F8;
	smpSpecialCodes[GLUT_KEY_F9]    = Key::F9;
	smpSpecialCodes[GLUT_KEY_F10]    = Key::F10;
	smpSpecialCodes[GLUT_KEY_F11]    = Key::F11;
	smpSpecialCodes[GLUT_KEY_F12]    = Key::F12;
}

bool menuservice::PostLoop()
{
	if(!m_pSystem or !m_pRenderer) // can happen...
		return false;

	// create time injection (for animation / blending and stuff like that)
	//dtNow = TimeSpecUtil::TimeUtils::getMicroStamp();
	microtime dtNow = flowvr::utils::getMicroStamp();
	if( m_dtLast > 0 and float(dtNow - m_dtLast) > 0.005 )
	{
		m_pSystem->injectTimePulse( float(dtNow - m_dtLast) );

		// save now time-stamp for future iteration
		if(m_bUpdate or m_pSystem->isRedrawRequested())
		{
//			std::cout << "redraw requested." << std::endl;
			m_pSystem->renderGUI();
			m_pRenderer->flush();
			m_bUpdate = false;
		}
	}

	m_dtLast = dtNow;

	return true;
}

void menuservice::signalUpdate()
{
	m_bUpdate = true;
}

int menuservice::getMappedKey( int c, int mod, bool special ) const
{
	if( special )
	{
		KEYCODES::const_iterator it = smpSpecialCodes.find(c);
		if(it == smpSpecialCodes.end())
			return -1;
		return (*it).second;
	}
	else
	{
		KEYCODES::const_iterator it = smpKeyNormalCodes.find(c);
		if(it == smpKeyNormalCodes.end())
			return -1;
		return (*it).second;
	}
}

bool menuservice::getIsValid() const
{
	return (m_pSystem and m_pRenderer);
}

bool menuservice::Once()
{
	ModuleAPI *m = getParent().getModuleAPI();

	Port *p = m->getPortByName( m_strPortName );
	if(!p or (p->isOutput() == false) )
		return false;

	OutputPort *sop = static_cast<OutputPort*>(p);
	try
	{
		m_pRenderer = new FlowVRCegRenderer(sop, (m ? m->generateID() : 0x7F000000 ) );
		m_pSystem = &System::create(*m_pRenderer, NULL, NULL, NULL, m_scripts, m_sMsXml, m_sLogFile );
		Logger::getSingleton().setLoggingLevel(LoggingLevel(m_nLogLevel));

		if(!m_sIniScript.empty())
		{
//			static bool b = m_bWaitScript;
//			while(b)
//				utils::microsleep(1000);


			std::cout << "-- ## executing script file [" << m_sIniScript << "]" << std::endl;
			m_pSystem->executeScriptFile( m_sIniScript.c_str() );
		}
		else
			std::cout << "-- ## not executing any script file." << std::endl;
	}
	catch( CEGUI::Exception &e )
	{
		std::cerr << "Exception during system setup: " << e.getMessage().c_str() << std::endl;
		System::destroy();
		m_pSystem = NULL;
		delete m_pRenderer;
		m_pRenderer = NULL;
		return false;
	}
	return true;
}
