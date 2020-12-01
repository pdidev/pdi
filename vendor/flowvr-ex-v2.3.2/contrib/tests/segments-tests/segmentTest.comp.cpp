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
* File: segmentTest.comp.cpp                                      *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include "segmentTest.comp.h"
// needed for the GENCLASS macro
#include <flowvr/app/core/genclass.h>
#include <flowvr/app/core/run.h>
#include <flowvr/app/components/connection.comp.h>
#include <flowvr/app/components/filterpresignal.comp.h>
#include <flowvr/portutils/components/portmodulerun.comp.h>

namespace segmentTest
{
	using namespace flowvr::portutils;
	using namespace flowvr::app;

	GENCLASS(SegmentTest)

	SegmentTest::SegmentTest(const std::string &id_)
	: Composite(id_)
	{
		setInfo("Create a segment send/receive test");
	}


	void SegmentTest::execute()
	{
		flowvr::app::FlowvrRunSSH ssh(this);

		PortModuleRun *push = new PortModuleRun("push", "config/segment-push.xml", ssh, this );
		PortModuleRun *pull = new PortModuleRun("pull", "config/segment-pull.xml", ssh, this );

		link( push->getPort("push"), pull->getPort("pull") );

		FilterPreSignal *ps = addObject<FilterPreSignal>("ps");

		link( pull->getPort("endIt"), ps->getPort("in") );
		link( ps->getPort("out"), push->getPort("beginIt") );
	}

	flowvr::app::Component* SegmentTest::create() const
	{
		return new SegmentTest(this->getId());
	}

}

