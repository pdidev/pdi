/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
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
* File: menusource.comp.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include "menusource.comp.h"

#include <flowvr/app/components/syncmaxfrequency.comp.h>
#include <flowvr/portutils/components/portmodulerun.comp.h>
#include <flowvr/app/core/run.h>

using namespace flowvr::app;
using namespace flowvr::portutils;


namespace menusource
{

	MenuSource::MenuSource( const std::string &id_ )
	: Composite( id_ )
	{
		addParameter( "portfile", "menu.xml" );
		addParameter( "config-prefix", "" );


		addPort("scene", OUTPUT, FULL );
		addPort("devices", INPUT, FULL );
		addPort("viewport", INPUT, FULL );

	}

	MenuSource::~MenuSource()
	{

	}

	void MenuSource::execute()
	{
		std::string portfile = getParameter<std::string>("portfile");
		std::string prefix   = getParameter<std::string>("config-prefix");

		FlowvrRunSSH ssh(this);

		PortModuleRun *menu = new PortModuleRun("menu", prefix+std::string("/")+portfile, ssh, this );
		SyncMaxFrequency *mf     = addObject<SyncMaxFrequency>("sync");

		link( getPort("devices"), menu->getPort("devices_in") );
		link( menu->getPort("scene"), getPort("scene") );
		link( getPort("viewport"), menu->getPort("viewport_in") );


		link( menu->getPort("endIt"), mf->getPort("endIt") );
		link( mf->getPort("out"), menu->getPort("beginIt") );
	}

	Component *MenuSource::create() const
	{
		return new MenuSource( getId() );
	}
}
