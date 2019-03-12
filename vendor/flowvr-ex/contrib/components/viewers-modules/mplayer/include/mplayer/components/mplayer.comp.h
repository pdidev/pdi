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

// core library flowvr-app include
#include <flowvr/app/core/component.h>


#ifndef _MPLAYER_H_
#define _MPLAYER_H_

using namespace flowvr::app;

namespace mplayer
{

    // MPlayer is a composite component
    class MPlayer : public Composite
    {

        public :
            MPlayer(const std::string id_) : Composite(id_)
        {
            setInfo("MPlayer displays a burning mplayer");
        };

            virtual ~MPlayer(){};

            // Composite components need an execute method. 
            virtual void execute();

            // Mandatory create method
            virtual Component* create() const	{ return new MPlayer(this->getId());	};

    };


};


#endif //_MPLAYER_H_
