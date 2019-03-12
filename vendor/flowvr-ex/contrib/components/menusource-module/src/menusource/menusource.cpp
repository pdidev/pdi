/******************************************************************
*                                                                 *
*       File : menusource.cpp                           *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#include "cegrenderer/cegrenderer.h"

#include <CEGUI/CEGUI.h>
#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUIInputEvent.h>
#include <CEGUI/ScriptingModules/LuaScriptModule/CEGUILua.h>

#include "menusourceconfig/menusourceconfig.h"

#include <flowvr/module.h>
#include <flowvr/utils/timing.h>
#include <flowvr/render/chunkrenderwriter.h>
#include <ftl/chunkevents.h>
#include <flowvr/utils/cmdline.h>
#include <GL/glut.h>

#include "msgtypes.h"


namespace
{
	typedef std::map<int, int> KEYCODES;

	KEYCODES smpKeyNormalCodes,
				smpSpecialCodes;

	flowvr::utils::Option<std::string> logFile("logfile",
										'l' ,
										"CEGUI log file name",
										"cegui.out.log",
										false);
	flowvr::utils::Option<int> logLevel("loglevel",
										'd' ,
										"CEGUI log level (0-4)",
										0,
										false);
	flowvr::utils::Option<std::string> configFile("menuconfig",
										'c' ,
										"CEGUI main resource config file name",
										"config/menusource-config.xml",
										false);
	flowvr::utils::Option<std::string> scriptFile("menuscript",
										'm' ,
										"CEGUI menu script file name",
										"",
										false);
}

void setupKeyCodes()
{
	smpKeyNormalCodes[0x27] = CEGUI::Key::Escape;
	smpKeyNormalCodes[0x08] = CEGUI::Key::Backspace;
	smpKeyNormalCodes[0x7f] = CEGUI::Key::Delete;
	smpKeyNormalCodes[0x0D] = CEGUI::Key::Return;
	smpKeyNormalCodes[0x09] = CEGUI::Key::Tab;


	smpSpecialCodes[GLUT_KEY_LEFT]  = CEGUI::Key::ArrowLeft;
	smpSpecialCodes[GLUT_KEY_RIGHT] = CEGUI::Key::ArrowRight;
	smpSpecialCodes[GLUT_KEY_UP]    = CEGUI::Key::ArrowUp;
	smpSpecialCodes[GLUT_KEY_DOWN]  = CEGUI::Key::ArrowDown;
	smpSpecialCodes[GLUT_KEY_HOME]  = CEGUI::Key::Home;
	smpSpecialCodes[GLUT_KEY_END]   = CEGUI::Key::End;
	smpSpecialCodes[GLUT_KEY_PAGE_UP]   = CEGUI::Key::PageUp;
	smpSpecialCodes[GLUT_KEY_PAGE_DOWN]   = CEGUI::Key::PageDown;
	smpSpecialCodes[GLUT_KEY_INSERT]   = CEGUI::Key::Insert;

	smpSpecialCodes[GLUT_KEY_F1]    = CEGUI::Key::F1;
	smpSpecialCodes[GLUT_KEY_F2]    = CEGUI::Key::F2;
	smpSpecialCodes[GLUT_KEY_F3]    = CEGUI::Key::F3;
	smpSpecialCodes[GLUT_KEY_F4]    = CEGUI::Key::F4;
	smpSpecialCodes[GLUT_KEY_F5]    = CEGUI::Key::F5;
	smpSpecialCodes[GLUT_KEY_F6]    = CEGUI::Key::F6;
	smpSpecialCodes[GLUT_KEY_F7]    = CEGUI::Key::F7;
	smpSpecialCodes[GLUT_KEY_F8]    = CEGUI::Key::F8;
	smpSpecialCodes[GLUT_KEY_F9]    = CEGUI::Key::F9;
	smpSpecialCodes[GLUT_KEY_F10]    = CEGUI::Key::F10;
	smpSpecialCodes[GLUT_KEY_F11]    = CEGUI::Key::F11;
	smpSpecialCodes[GLUT_KEY_F12]    = CEGUI::Key::F12;
}

int getKeyCode( int c, int mod, bool special )
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


void handle_viewport_change( CEGUI::System *system,
							 CEGUI::FlowVRCegRenderer *renderer,
		                     const flowvr::Message &msg )
{
	// ok, check on vp changes
	flowvr::render::balzac::ViewportChgMsg vp;
	vp.fromBuffer( msg.data );

	// ignore the x/y offset, as we assume to have 1 vieport
	// starting window-relative at 0, giving the pixel dimension
	// in w/h of the vp. We first pass this to the renderer.
	renderer->setDisplaySize( CEGUI::Size( vp.m_w, vp.m_h ) );

	// have to look that up: the client area for the current GUI
	// sheet has to be pushed manually. Using a 0 offset, covering
	// the whole visible area for now.
	if(system->getGUISheet())
	{
		system->getGUISheet()->setArea(
				CEGUI::URect( CEGUI::UVector2( CEGUI::UDim(0,0),
											   CEGUI::UDim(0,0) ),
							  CEGUI::UVector2( CEGUI::UDim(1,0),
											   CEGUI::UDim(1,0) ) ) );
	}
	else
	{
		std::cerr << "no root shell upon vp resize!" << std::endl;
	}
}

/**
 * @return true if chunk contained a key that was swallowed
 */
bool handle_keyboard_chunk( CEGUI::System *system, const ftl::Chunk *c )
{
	int  modifiers = ((ftl::ChunkEventKeyboard *) c)->modifier;
	bool special   = ((ftl::ChunkEventKeyboard *) c)->special;
	int key        = ((ftl::ChunkEventKeyboard *) c)->key;

	// true: down, false: up key event
	bool bDown     = ((ftl::ChunkEventKeyboard *) c)->val;

	if( modifiers & 0x01 ) // shift
	{
		bDown ? system->injectKeyDown( CEGUI::Key::LeftShift ) : system->injectKeyUp( CEGUI::Key::LeftShift );
	}
	if( modifiers & 0x02 ) // ctrl
	{
		bDown ? system->injectKeyDown( CEGUI::Key::LeftControl ) : system->injectKeyUp( CEGUI::Key::LeftControl );
	}
	if( modifiers & 0x04 ) // alt
	{
		bDown ? system->injectKeyDown( CEGUI::Key::LeftAlt ) : system->injectKeyUp( CEGUI::Key::LeftAlt );
	}


	int keyCode = getKeyCode( key, modifiers, special );
	bool bSwallowed = false;
	if( keyCode != -1 )
	{
		bSwallowed = (bDown ? system->injectKeyDown(keyCode) : system->injectKeyUp(keyCode));
	}
	else
	{
		if(bDown)
		{
			bSwallowed = system->injectChar(static_cast<CEGUI::utf32>(key)) || bSwallowed;
		}
		else
			bSwallowed = true;
	}

	return bSwallowed;
}

bool handle_mouse_chunk( CEGUI::System *system, const ftl::Chunk *c )
{
	ftl::ChunkEventMouse *pM = ((ftl::ChunkEventMouse *) c);

	// inject current position
	system->injectMousePosition(pM->mouseTranslation[0], pM->mouseTranslation[1]);

	bool bBt = false;
	int btMask = 0;
	if(int(pM->mouseKeys) & 0x01)
	{
		btMask |= CEGUI::LeftButton;
		bBt = true;
	}

	if(int(pM->mouseKeys) & 0x04)
	{
		bBt = true;
		btMask |= CEGUI::RightButton;
	}

	if(int(pM->mouseKeys) & 0x02)
	{
		bBt = true;
		btMask |= CEGUI::MiddleButton;
	}

	bool bBtDispatch = false;

	// mouse chunks do not only contain buttons, but coordinates as
	// well, so it is not easy to say whether a mouse movement should
	// be passed on or not. The policy chosen here is:
	// - in case the UI sucked a button -> the button is eliminated
	//   from the chunk (setting pM->released to 0), but the
	//   mouse chunk itself is passed on
	// - in case mouse wheel was not dispatched, the chunk is passed on
	bool bPassOn     = false;

	if(pM->released == 2) // that's a button up
	{
		bBtDispatch = system->injectMouseButtonUp( CEGUI::MouseButton(btMask) );
	}
	else if(pM->released == 1) // that's a button down
	{
		bBtDispatch = system->injectMouseButtonDown( CEGUI::MouseButton(btMask) );
	}

	if( bBtDispatch ) // button was dispatched?
	{
		pM->released = 0; // kill (when forwarding, this chunk does not contain
		                  // a mouse button anymore)
	}

	if( pM->mouseWheelDir ) // this is a mouse wheel chunk
	{
		if(system->injectMouseWheelChange( float(pM->mouseWheelDir) )==false)
		{
			// unused mouse wheel is passed on as well
			bPassOn = true;
		}
	}

	return bPassOn;
}

bool handle_input( CEGUI::System *system,
				   ftl::ChunkEventWriter &writer,
		           const flowvr::Message &mTokenChunks )
{
	// work on keys
	int nChkCnt = 0;
	bool bRender = false;
	for (ftl::ChunkIterator it = ftl::chunkBegin(mTokenChunks); it
				!= ftl::chunkEnd(mTokenChunks); ++it, ++nChkCnt)
	{
		const ftl::Chunk* c = (const ftl::Chunk*) it; // Convert the iterator in a chunk base structure
		switch (c->type & 0x0F)
		{ // Look for each type of IO input

			case ftl::ChunkEvent::KEYBOARD: // In the case of a keyboard input
			{
				if(handle_keyboard_chunk(system, c) == false)
					writer.addChunk(c); // not swallowed -> pass on
				else
					bRender = true;
				break;
			}
			case ftl::ChunkEvent::MOUSE: // In the case of a mouse input
			{
				if( handle_mouse_chunk(system, c ) == true)
					writer.addChunk(c); // not swallowed -> pass on
				else
					bRender = true;
				break;
			}
			default:
			{
				std::cerr << "chunk error??\n";
				break;
			}
		}
	}

	return bRender;
}

bool handle_new_script( const flowvr::Message &scpMsg )
{
	return false;
}

int main(int argc, char **argv)
{
	flowvr::utils::CmdLine cmd;
	bool error=false;
	if (cmd.parse(argc,argv,&error) == false)
	{
		std::cerr << "wrong or missing parameters. quit."
		          << std::endl;
		return -1;
	}

	flowvr::InputPort       pTkIn("token_in", NULL, false, true); // mouse and keyboard
	flowvr::InputPort       pVpIn("viewport_in", NULL, false, true); // viewport changes (event port)
	flowvr::InputPort       pScIn("script_in", NULL, false, true); // script commands to follow
	flowvr::InputPort       pReIn("response_in", NULL, false, true);

	flowvr::render::SceneOutputPort pOut("scene");  // menus
	flowvr::OutputPort      pTkOut("token_out"); // passing mouse and keyboard
	flowvr::OutputPort      pCmdOut("request_out"); // send requests


	// Initialize FlowVR
	std::vector<flowvr::Port*> ports;
	ports.push_back(&pOut);
	ports.push_back(&pTkIn);
	ports.push_back(&pVpIn);
	ports.push_back(&pScIn);
	ports.push_back(&pTkOut);
	ports.push_back(&pCmdOut);
	ports.push_back(&pReIn);

	flowvr::ModuleAPI* module = flowvr::initModule(ports);
	if (module == NULL)
		return 1;

	setupKeyCodes();

	CEGUI::FlowVRCegRenderer *renderer;
	CEGUI::System            *system;
	try
	{
		// @todo fix the constant prefix here.
		renderer = new CEGUI::FlowVRCegRenderer(&pOut, 0x7E000000);
		system   = &CEGUI::System::create( *renderer, NULL, NULL, NULL, NULL, "", logFile.value().c_str() );


		if( logFile.count )
		{
			if(logLevel.count)
			{
				int nLogLevel = std::min<int>( CEGUI::Insane,
											  std::max<int>(0, logLevel.value() ) );

				CEGUI::Logger::getSingleton().setLoggingLevel(CEGUI::LoggingLevel(nLogLevel));
			}

			CEGUI::Logger::getSingleton().setLogFilename(logFile.value(), false);
		}

		// important: setup the CE GUI resource system and stuff
		MenuSourceConfig cfg( system, configFile.value() );

//		// create the lua scripting module
     	CEGUI::LuaScriptModule& scriptmod = CEGUI::LuaScriptModule::create();
		system->setScriptingModule(&scriptmod);

		// evaluate initial script as requested by user
		if( !scriptFile.value().empty() )
			system->executeScriptFile(scriptFile.value());
	}
	catch(CEGUI::Exception &x)
	{
		std::cerr << "MENUSOURCE: caught an exception; \n error in CEGUI initialization. \n giving up."
		          << std::endl;
		std::cerr << x.getMessage().c_str() << std::endl;
		std::cerr << "\n\n#########################################\n\n";

		return -2;
	}
	catch(...)
	{
		std::cerr << "\n\n#########################################\n\n";
		std::cerr << "MENUSOURCE: caught an exception; \n error in CEGUI initialization. \n giving up."
		          << std::endl;
		std::cerr << "\n\n#########################################\n\n";
		return -3;
	}


	bool bEnabled = true;
	flowvr::utils::microtime dtNow  = 0;
	flowvr::utils::microtime dtLast = 0;

	ftl::ChunkEventWriter   m_writer;

	// initial write out. (in case some scripts were loaded to create some windows already)
	system->renderGUI();
	renderer->flush();

	while(module->wait())
	{
		// get messages from all ports
		// start with the input chunks
	 	flowvr::Message mTokenChunks;
		module->get(&pTkIn, mTokenChunks);

		// get now possible viewport changes
		flowvr::Message mVpChangeMsg;
		module->get(&pVpIn, mVpChangeMsg);

		// first and always: check for viewport changes
		if( mVpChangeMsg.valid() )
		{
			handle_viewport_change( system, renderer, mVpChangeMsg );
		}

		// get a possible script to execute
		flowvr::Message mScMsg;
		module->get( &pScIn, mScMsg );

		if( mScMsg.valid() )
		{
			// execute script
			handle_new_script(mScMsg);
		}


		// see whether menu is enabled or disabled...
		if( bEnabled == false )
		{
			// disabled, so pass all input un-filtered to possible receiver(s)
			flowvr::MessageWrite mTkOut;
			mTkOut.data = module->alloc(mTokenChunks.data.getSize());
			memcpy( mTkOut.data.getWrite<void*>(),
					mTokenChunks.data.getRead<void*>(),
					mTokenChunks.data.getSize());
			module->put(&pTkOut, mTkOut);
		}
		else
		{
			bool bRender = false;
			if( mTokenChunks.valid() )
			{
				try
				{
					bRender = handle_input( system, m_writer, mTokenChunks );
				}
				catch( CEGUI::Exception &x )
				{
					std::cerr << "CEGUI -- EXCEPTION: "
							  << std::endl
							  << x.getMessage().c_str()
							  << std::endl;
				}
			}

			// create time injection (for animation / blending and stuff like that)
			//dtNow = TimeSpecUtil::TimeUtils::getMicroStamp();
			dtNow = flowvr::utils::getMicroStamp();

			if( float(dtNow - dtLast) > 0.005 )
			{
				system->injectTimePulse( float(dtNow - dtLast) );
				// save now time-stamp for future iteration
				if(system->isRedrawRequested())
				{
					bRender = true;
				}
				dtLast = dtNow;
			}

			if(bRender)
			{
				system->renderGUI();
				renderer->flush();
			}
		}
	}

	// main loop was left, so clean up a bit
	CEGUI::LuaScriptModule::destroy( * dynamic_cast<CEGUI::LuaScriptModule*>(system->getScriptingModule()) );

	// release reference to our scripting module
	system->setScriptingModule(NULL);

	// delete what we have created
	CEGUI::System::destroy();
	delete renderer;

	// signal module close to FlowVR
	module->close();

	// de-allocate resources
	delete module;

	// wait a bit (for debugging the module purge)
	flowvr::utils::microsleep(500);
	return 0;
}
