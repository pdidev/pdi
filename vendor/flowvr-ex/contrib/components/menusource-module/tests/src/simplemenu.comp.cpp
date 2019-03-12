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
* File: simplemenutest.comp.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <flowvr/app/core/genclass.h>
#include <flowvr/app/core/run.h>
#include <flowvr/render/balzac/components/balzacbasic.comp.h>

//#include <flowvr/render/balzac/components/metamoduleViewerMesh.comp.h>
#include <flowvr/portutils/components/portmodulerun.comp.h>
#include "components/menusource.comp.h"

using namespace flowvr::app;
using namespace flowvr::render::balzac;
using namespace std;
using namespace flowvr::portutils;


namespace menusource
{
	namespace tests
	{
		class SimpleMenu : public Composite
		{
		public:
			SimpleMenu(const std::string &strId)
			: Composite(strId)
			{
				addParameter( "config-prefix", "");
			}

			Composite *create()
			{
				return new SimpleMenu( getId() );
			}

			void execute()
			{

				FlowvrRunSSH ssh(this);

				string strPrefix = getParameter<string>("config-prefix");

				BalzacBasic *balzac = new BalzacBasic( "balzacbasic", strPrefix+"balzac-scene.xml"/*, &ssh, this*/ );
				MenuSource *menu = addObject<MenuSource>("menusource");

				link( balzac->getPort("devices"), menu->getPort("devices") );
				link( menu->getPort("scene"), balzac->getPort("scene") );
				link( balzac->getPort("viewport"), menu->getPort("viewport") );
			}
		};

		GENCLASS(SimpleMenu)
	}
}
