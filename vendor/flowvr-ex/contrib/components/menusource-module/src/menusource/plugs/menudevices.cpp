/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
                          flowvr-petaflow
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
* File: menudevices.cpp
*                                                                 *
* Contacts: assenmac
*                                                                 *
******************************************************************/


#include <flowvr/portutils/portplugin.h>
#include <flowvr/portutils/porthandler.h>

#include <menuservice.h>

#include <CEGUI/CEGUISystem.h>
#include <flowvr/render/chunkrenderwriter.h>
#include <ftl/chunkevents.h>

namespace
{
	/**
	 * @return true if chunk contained a key that was swallowed
	 */
	bool handle_keyboard_chunk( menuservice *service, const ftl::Chunk *c )
	{
		CEGUI::System *system = service->m_pSystem;

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


		int keyCode = service->getMappedKey( key, modifiers, special );
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

	bool handle_mouse_chunk( menuservice *service, const ftl::Chunk *c )
	{
		CEGUI::System *system = service->m_pSystem;
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

	bool handle_input( menuservice *service,
					   ftl::ChunkEventWriter &writer,
					   const flowvr::Message &mTokenChunks )
	{
		// work on keys
		CEGUI::System *system = service->m_pSystem;

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
					if(handle_keyboard_chunk(service, c) == false)
						writer.addChunk(c); // not swallowed -> pass on
					else
						bRender = true;
					break;
				}
				case ftl::ChunkEvent::MOUSE: // In the case of a mouse input
				{
					if( handle_mouse_chunk(service, c ) == true)
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
}

namespace
{

	class menudevicesHandler : public flowvr::portutils::PipeHandler
	{
	public:
		menudevicesHandler( menuservice *pService, const flowvr::portutils::ARGS &args )
		: flowvr::portutils::PipeHandler()
		, m_service(pService)
		{
		}

		virtual eState handleMessagePipe( const flowvr::Message &m_in,
				                          const flowvr::StampList *sl_in,
				                          flowvr::MessageWrite &m_out,
				                          flowvr::StampList *sl_out,
				                          flowvr::Allocator &allocator )
		{
			if( !m_service->getIsValid() )
				return E_ERROR;

			ftl::ChunkEventWriter w;
			if(handle_input( m_service, w, m_in ))
				m_service->signalUpdate();

			if(w.isDirty())
				m_out = w.dump<flowvr::Allocator>(&allocator);

			return E_OK;
		}


		menuservice *m_service;
	};
}


DEFIMP_PORTPLUG_NOPRM( menudevicesHandler, menuservice, menudevices )

