/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
                          flowvr-dev
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
* File: pushSegmentPlug.cpp
*                                                                 *
* Contacts: assenmac
*                                                                 *
******************************************************************/


#include <flowvr/portutils/portplugin.h>
#include <flowvr/portutils/porthandler.h>

#include <flowvr/utils/timing.h>
#include <flowvr/stamp.h>

#include <stdlib.h>
#include <math.h>

#include <algorithm>
#include <iterator>
#include <iostream>

using namespace flowvr;
using namespace flowvr::portutils;


extern "C" void getParameters( ARGS &args )
{
	args["TIMEOUT"] = Parameter("500", "Wait-time after send (ms)", Parameter::P_NUMBER);
	args["MIN"] = Parameter("0", "Minimum number of segments (added to the default of 1)", Parameter::P_NUMBER);
	args["MAX"] = Parameter("10", "Maximum number of segments (subtracted from the default of 1)", Parameter::P_NUMBER);
	args["RANDOMSEND"] = Parameter("true", "send messages with segments at random", Parameter::P_BOOLEAN);
	args["SIZE"] = Parameter(Parameter::P_NUMBER, Parameter::PS_NONE, "sets the size of all segments, random value else", Parameter::MD_MANDATORY);
	args["KEEPSEGMENTS"] = Parameter("1", "Keep segments in message as segments (0=no, 1=yes,-1=random)", Parameter::P_NUMBER );
}


namespace
{

	void init()
	{
		double y, seed = flowvr::utils::getNtpTimeStamp();
		modf(seed,&y);

		srand( seed );
	}

	int genRand( int nMin, int nMax )
	{
		if( nMin == nMax )
			return nMin;

		return nMin + (int)( (float)( nMax-nMin + 1 ) * ( rand() / (RAND_MAX + 1.0) ) );
	}

	class pushSegmentPlugHandler : public flowvr::portutils::SourcePortHandler
	{
	public:
		pushSegmentPlugHandler( const flowvr::portutils::ARGS &args )
		: flowvr::portutils::SourcePortHandler()
		, m_timeout(0)
		, m_min(0)
		, m_max(0)
		, m_randomsend(true)
		, m_packetsize(-1)
		, m_iteration(0)
		, m_keepsegments(1)
		{
			init();
			try
			{
				m_timeout = args("TIMEOUT").getValue<int>();
				m_min = args("MIN").getValue<int>();
				m_max = args("MAX").getValue<int>();
				m_randomsend = args("RANDOMSEND").getValue<bool>();
				m_packetsize = args("SIZE").getValue<int>();
				m_keepsegments = args("KEEPSEGMENTS").getValue<int>();
			}
			catch( std::exception &e )
			{

			}
		}

		void createSignature( const Buffer &buffer, std::vector<int> &signature )
		{
			signature.push_back( buffer.getSize(Buffer::ALLSEGMENTS) );
			signature.push_back( buffer.getNumberOfSegments() );

			for( size_t n = 0; n < buffer.getNumberOfSegments(); ++n )
				signature.push_back(  buffer.getSize(n) );
		}

		virtual eState handleMessage( flowvr::MessageWrite &out,
                                      flowvr::StampList *stampsOut,
                                      flowvr::Allocator &allocate )
		{
			StampInfo *sizes = (*stampsOut)["sizes"];
			bool b;
			if( !sizes )
				goto leave;

			out.data = allocate.alloc(1);
			if( !out.data.valid() )
				return E_ERROR;

			out.stamps.write( (*sizes)[0], 1 );

			b = (m_randomsend ? (genRand(0,1) == 1 ? true : false) : true);

			if(b)
			{
				out.stamps.write( (*stampsOut).userflags, m_iteration++ );

				int nNum  = genRand(m_min,m_max);

				StampInfo *num   = (*stampsOut)["num_segments"];
				StampInfo *signature = (*stampsOut)["signature"];
				StampInfo *orig_keep_flag = (*stampsOut)["orig_keep_flag"];

				if( !num or !signature or !orig_keep_flag)
				{
					std::cerr << "stamps are not defined correctly!" << std::endl;
					goto leave;
				}


				StampInfo *flags = &(*stampsOut).sysflags;

				int nflags;
				out.stamps.read( *flags, nflags );

				bool keepsegments;
				switch( m_keepsegments )
				{
				case 0:
					keepsegments = false;
					break;
				case -1:
					keepsegments = ( genRand(0,1) == 1 ? true:false );
					break;
				case 1:
				default:
					keepsegments = true;
					break;
				}

				if( keepsegments )
					out.stamps.write( *flags, nflags bitor StampList::SYS_FLG_KEEPSEGMENTS );

				out.stamps.write( *orig_keep_flag, keepsegments ? 1:0 );

				for( int n = 0; n < nNum ; ++n )
				{
					int nSize = (m_packetsize == -1 ? genRand( 1, 1000000 ) : m_packetsize * (n+1));
					out.stamps.write( (*sizes)[n+1], nSize );
					BufferWrite wr = allocate.alloc(nSize);
					if(!wr.valid())
						return E_ERROR;

					out.data += wr;
				}

				nNum = out.data.getNumberOfSegments();
				out.stamps.write( *num, nNum );

				std::vector<int> sig;
				createSignature( out.data, sig );

				for( int n=0; n < sig.size(); ++n )
					out.stamps.write( (*signature)[n], sig[n] ); // copy signature element-wise
			}
			else
			{
				out.stamps.write( (*stampsOut).userflags, -m_iteration++ );
			}
leave:
			return E_OK;
		}

		virtual void postPutActivation()
		{
			flowvr::utils::microsleep(m_timeout);
		}

		int m_timeout,
		    m_min,
		    m_max,
		    m_packetsize,
		    m_iteration,
		    m_maxsegments,
		    m_minsegments,
		    m_keepsegments;
		bool m_randomsend;

	};
}


DEFIMP_PORTPLUG_NOSERVICE( pushSegmentPlugHandler, pushSegmentPlug )

