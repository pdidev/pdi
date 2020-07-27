/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
                          flowvr-dev-05-11-2010
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
* File: simpleoutPlug.cpp
*                                                                 *
* Contacts: assenmac
*                                                                 *
******************************************************************/


#include <flowvr/portutils/portplugin.h>
#include <flowvr/portutils/porthandler.h>

extern "C" void getParameters( flowvr::portutils::ARGS &args )
{
	// insert parameter here
}

namespace
{

	class simpleoutPlugHandler : public flowvr::portutils::NoPortHandler
	{
	public:
		simpleoutPlugHandler( const flowvr::portutils::ARGS &args )
		: flowvr::portutils::NoPortHandler()
		, m_nb(0)
		{
			try
			{
			}
			catch( std::exception &e )
			{
			}
		}

		virtual eState handleNoMessage()
		{
			std::cout << "iterated! [" << m_nb++ << "]" << std::endl;
			return E_OK;
		}

		size_t m_nb;
	};
}


DEFIMP_PORTPLUG_NOSERVICE( simpleoutPlugHandler, simpleoutPlug )

