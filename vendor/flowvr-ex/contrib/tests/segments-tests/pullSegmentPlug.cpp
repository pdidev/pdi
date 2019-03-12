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
* File: pullSegmentPlug.cpp
*                                                                 *
* Contacts: assenmac
*                                                                 *
******************************************************************/


#include <flowvr/portutils/portplugin.h>
#include <flowvr/portutils/porthandler.h>

#include <algorithm>
#include <iterator>
#include <iostream>


using namespace flowvr::portutils;
using namespace flowvr;


extern "C" void getParameters( ARGS &args )
{
	args["SEGONLY"] = Parameter("true", "Show only messages with segments in", Parameter::P_BOOLEAN );
	args["SHOWALL"] = Parameter("false", "Show all info that is available (clutters display)", Parameter::P_BOOLEAN );
	args["VERBOSE"] = Parameter("true", "Give output (filters positive results, just showing failure)", Parameter::P_BOOLEAN);
}

namespace
{

	class pullSegmentPlugHandler : public flowvr::portutils::SinkPortHandler
	{
	public:
		pullSegmentPlugHandler( const flowvr::portutils::ARGS &args )
		: flowvr::portutils::SinkPortHandler()
		, bShowSegMsgOnly(false)
		, bShowAll( false )
		, bVerbose(true)
		{
			try
			{
				bShowSegMsgOnly = args("SEGONLY").getValue<bool>();
				bShowAll = args("SHOWALL").getValue<bool>();
				bVerbose = args("VERBOSE").getValue<bool>();
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
				signature.push_back( buffer.getSize(n) );
		}

		virtual eState handleMessage( const flowvr::Message &in,
                                      const flowvr::StampList *stampsIn )
		{
			bool bShow = true;

			if( bShowSegMsgOnly and in.data.getSegments().empty() )
				bShow = false;

			if(bShow)
			{
				int it, tag;
				in.stamps.read(stampsIn->it, it);
				in.stamps.read((*stampsIn).userflags, tag);

				if(bVerbose)
				{
					std::cout << "received buffer @ " << it << " ; " << tag << std::endl;
					std::cout << "buffer-length = "   << in.data.getSize(Buffer::ALLSEGMENTS) << std::endl;
					std::cout << "# PULL -- segments: " << in.data.getSegments().size() << std::endl;
				}

				StampInfo *num   = (*stampsIn)["num_segments"];
				StampInfo *sizes = (*stampsIn)["sizes"];
				StampInfo *signature = (*stampsIn)["signature"];
				StampInfo *orig_keep_flag = (*stampsIn)["orig_keep_flag"];

//				std::cout << "array size (units of int): " << (*sizes).getSize()/sizeof(int) << std::endl;

				if( !num or !sizes or !signature or !orig_keep_flag)
				{
					std::cerr << "stamps not setup correctly!" << std::endl;
					goto leave;
				}

				int nNum, orig_keep;
				if(in.stamps.read( *num, nNum ) == false)
					std::cerr << "error reading num-stamps" << std::endl;
				if(in.stamps.read( *orig_keep_flag, orig_keep ) == false )
					std::cerr << "error reading orig_keep_flag" << std::endl;

				if( bVerbose )
				{
					if( orig_keep )
						std::cout << "original message said to KEEP SEGMENTS." << std::endl;
					else
						std::cout << "original message said nothing about the segments." << std::endl;
				}


				bool nNumSegmentMatches = (nNum == in.data.getSegments().size() or (!orig_keep and in.data.getSegments().size() == 1) );


				if( bVerbose or !nNumSegmentMatches )
				{
					std::cout << "#segs in stamps: " << (  nNumSegmentMatches ? "MATCH" : "MISMATCH");
					if(!nNumSegmentMatches)
						std::cout << " " << nNum << " vs " << in.data.getSegments().size();
					else
						std::cout << " expected  " << in.data.getSegments().size() << " and got them.";

					std::cout << std::endl;
				}

				if( orig_keep )
				{
					for( size_t n = 0; n < in.data.getSegments().size(); ++n )
					{
						const flowvr::Buffer &b = in.data[n];
						int size;
						if(in.stamps.read( (*sizes)[n], size )==false)
							std::cerr << "error reading sizes.. array size on receive = " << (*sizes).getSize() << std::endl;

						bool bSizeMatch = (size == b.getSize());

						if(bVerbose and bShowAll)
						{
							std::cout << "[" << n << "]: ("
									  << std::hex
									  << b.getRead<void>()
									  << std::dec
									  << " ; "
									  << b.getSize()
									  << " ; "
									  << b.getImp()
									  << " ; "
									  << ( bSizeMatch ? "SIZE-MATCH" : "SIZE-MISMATCH")
									  << " e: " << size << " , g: " << b.getSize();
						}

						if(!bSizeMatch)
						{
							int it;
							in.stamps.read( (*stampsIn).it, it );
							std::cerr << "SIZE-MISMATCH in it [" << it << "]: expected " << size << " vs " << b.getSize();
						}

						if( bVerbose )
						{
							if(bShowAll)
								std::cout << ")" << std::endl;
							else if(!bSizeMatch)
								std::cout << std::endl;
						}
					}
				}

				std::vector<int> sig;
				createSignature( in.data, sig );

				if( orig_keep )
				{
					for( int n=0; n < sig.size(); ++n )
					{
						int v;
						if( in.stamps.read( (*signature)[n], v ) )
						{
							if( v != sig[n] )
							{
								std::cout << "SIGNATURE MISMATCH!" << std::endl;
								goto leave;
							}
						}
					}
					if( bVerbose )
						std::cout << "SIGNATURE MATCH: ";
				}
				else
				{
					if( bVerbose )
					{
						if( !sig.empty() and sig[0] == in.data.getSize(Buffer::ALLSEGMENTS) )
							std::cout << "SIZE MATCH" << std::endl;
						else
							std::cerr << "SIZE-MISMATCH" << std::endl;
					}
				}


				if( bVerbose )
				{
					if(bShowAll)
						std::copy( sig.begin(), sig.end(), std::ostream_iterator<int>( std::cout, " ") );
					std::cout << std::endl;
				}
			}
leave:
			return E_OK;
		}


		bool bShowSegMsgOnly, bShowAll, bVerbose;
	};
}


DEFIMP_PORTPLUG_NOSERVICE( pullSegmentPlugHandler, pullSegmentPlug )

