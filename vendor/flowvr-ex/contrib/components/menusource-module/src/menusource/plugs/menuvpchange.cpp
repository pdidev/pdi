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
* File: vpchange.cpp
*                                                                 *
* Contacts: assenmac
*                                                                 *
******************************************************************/


#include <flowvr/portutils/portplugin.h>
#include <flowvr/portutils/porthandler.h>

#include <menuservice.h>

#include <flowvr/render/balzac/data/msgtypes.h>
#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUIWindow.h>
#include <cegrenderer/cegrenderer.h>

using namespace flowvr::render::balzac;


namespace
{

	class vpchangeHandler : public flowvr::portutils::SinkPortHandler
	{
	public:
		vpchangeHandler( menuservice *pService, const flowvr::portutils::ARGS &args )
		: flowvr::portutils::SinkPortHandler()
		, m_service(pService)
		{
		}

		virtual eState handleMessage( const flowvr::Message &in, 
                                      const flowvr::StampList *stampsIn )
		{
			if( !m_service->getIsValid() )
				return E_ERROR;

			ViewportChgMsg vp;
			vp.fromBuffer( in.data );

			// ignore the x/y offset, as we assume to have 1 vieport
			// starting window-relative at 0, giving the pixel dimension
			// in w/h of the vp. We first pass this to the renderer.
			m_service->m_pRenderer->setDisplaySize( CEGUI::Size( vp.m_w, vp.m_h ) );

			std::cout << "new vp size = [" << vp.m_w << " ; " << vp.m_h << "]" << std::endl;

//			// have to look that up: the client area for the current GUI
//			// sheet has to be pushed manually. Using a 0 offset, covering
//			// the whole visible area for now.
			if(m_service->m_pSystem->getGUISheet())
			{
				m_service->m_pSystem->getGUISheet()->setArea(
						CEGUI::URect( CEGUI::UVector2( CEGUI::UDim(0,0),
													   CEGUI::UDim(0,0) ),
									  CEGUI::UVector2( CEGUI::UDim(1,0),
													   CEGUI::UDim(1,0) ) ) );
//				CEGUI::URect r = m_service->m_pSystem->getGUISheet()->getArea();
//				std::cout << "area: "
//						  << r.d_max.d_x.d_scale << " ; "
//						  << r.d_max.d_x.d_offset << " ; "
//						  << r.d_max.d_y.d_scale << " ; "
//						  << r.d_max.d_y.d_offset << ". "
//						  << r.d_min.d_x.d_scale << " ; "
//						  << r.d_min.d_x.d_offset << " ; "
//						  << r.d_min.d_y.d_scale << " ; "
//						  << r.d_min.d_y.d_offset << ". "
//						  << std::endl;

			}
			else
			{
				std::cerr << "no root shell upon vp resize!" << std::endl;
			}
			return E_OK;
		}


		menuservice *m_service;
	};
}


DEFIMP_PORTPLUG_NOPRM( vpchangeHandler, menuservice, vpchange )

