/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
*                                                                 *
*-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
* INRIA                                                           *
* ALL RIGHTS RESERVED.	                                          *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *	
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
*                                                                 *
*  Contact : Jean-Denis Lesage                                    *
*                                                                 *
******************************************************************/

// flowvr-app core includes
#include <flowvr/app/core/genclass.h>
#include <flowvr/app/components/flowvr-app.comp.h>


// mplayer components includes
#include "mplayer/components/metamoduleviewermplayer.comp.h"

// flowvr-render components includes
#include <flowvr/render/components/renderer.comp.h>


#include "mplayer/components/mplayer.comp.h"

#include <vector>

using namespace flowvr::app;
using namespace flowvr::render;

namespace mplayer
{


  // Required to enable dynamic component loading
  // Argument given in parameter is the class name (respect case) (also requires a MPlayer(id) constructor)
  GENCLASS(MPlayer)
     

	void MPlayer::execute()
        {
            // The metamodule mplayer
            MetaModuleViewerMPlayer* mplayer = addObject<MetaModuleViewerMPlayer>("mplayer");

            // We set the max frequency of the viewer
            SyncMaxFrequency* sMax = addObject<SyncMaxFrequency>("sMax");
            ConnectionStamps* cBeginIt = addObject<ConnectionStamps>("cBeginIt");
            ConnectionStamps* cEndIt = addObject<ConnectionStamps>("cEndIt");
            link(mplayer->getPort("endIt"), cBeginIt->getPort("in"));
            link(cBeginIt->getPort("out"), sMax->getPort("endIt"));
            link(sMax->getPort("out"), cEndIt->getPort("in"));
            link(cEndIt->getPort("out"), mplayer->getPort("beginIt"));
            sMax->setParameter("freq", "20");

            // Add component that renderers the scene. This component receives all primitives on a scene input port
            Renderer* renderer = addObject<Renderer>("Renderer"); 

            //link all viewers to renderer.
            //Remark: All communication schemas dedicated to flowvr-render are included in renderer. It is the component renderer that merges all primitives from different viewers. Renderer component also deals with synchronization issues. You just have to link scenes port to renderer to get your primitives rendered 
            link(mplayer->getPort("scene"), renderer->getPort("scenes"));

        }
};
