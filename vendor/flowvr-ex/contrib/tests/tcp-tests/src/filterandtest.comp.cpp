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
* File: tcp-test.comp.cpp                                         *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/


#include "tcp-test/components/filterandtest.comp.h"

#include <flowvr/app/core/genclass.h>
#include <flowvr/app/core/run.h>
#include <flowvr/app/components/filtersignaland.comp.h>
#include <flowvr/app/components/filterroutingnode.comp.h>
#include <flowvr/app/components/syncmaxfrequency.comp.h>

#include <flowvr/portutils/components/portmodule.comp.h>
#include <flowvr/portutils/components/portmodulerun.comp.h>

using namespace flowvr::app;
using namespace flowvr::portutils;


namespace tcptest
{

	class PreFilter : public Composite
	{
	public:
		PreFilter( const std::string &_id )
		: Composite(_id)
		{
			addPort("beginIt", INPUT, STAMPS);
			addPort("out", OUTPUT, STAMPS);
			addPort("endIt", OUTPUT, STAMPS );
		}

		Component *create() const
		{
			return new PreFilter(getId());
		}

		void execute()
		{
			FlowvrRunSSH ssh(this);
			PortModuleRun *p = new PortModuleRun( "PreFilterModule", "config/prefilter.xml", ssh, this );
			link(getPort("beginIt"), p->getPort("beginIt") );
			link(getPort("endIt"), p->getPort("endIt") );

			FilterRoutingNode *rn = addObject<FilterRoutingNode>("routing");
			rn->setParameter("nb", 0);

			link( p->getPort("endIt"), rn->getPort("in") );
			link( rn->getPort("out"), getPort("out") );
		}

	};


	FilterAndTest::FilterAndTest( const std::string &_id )
	: Composite(_id)
	{
		addParameter<int>("nb", 1);
		addParameter("configprefix", "config/");
	}

	void FilterAndTest::execute()
	{
		FilterSignalAnd *fand = addObject<FilterSignalAnd>("and");
		SyncMaxFrequency *mf = addObject<SyncMaxFrequency>("sync");

		link( fand->getPort("out"), mf->getPort("endIt") );

		int nb = getParameter<int>("nb");
		for( size_t n = 0 ; n < nb; ++n )
		{
			PreFilter *pf = addObject<PreFilter>("pf" + toString<int>(n));
			link( pf->getPort("out"), fand->getPort("in") );
			link( pf->getPort("beginIt"), mf->getPort("out") );

		}

		FlowvrRunSSH ssh(this);
		PortModuleRun *p = new PortModuleRun( "PostAndModule", "config/postandmodule.xml", ssh, this );

		link( fand->getPort("out"), p->getPort("beginIt") );
	}

	Component *FilterAndTest::create() const
	{
		return new FilterAndTest( getId() );
	}


	GENCLASS(FilterAndTest)
}
