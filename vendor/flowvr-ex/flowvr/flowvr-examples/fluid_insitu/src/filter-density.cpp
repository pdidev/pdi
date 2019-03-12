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
* File: ./gldens.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/

#include "fluid/Array2D.hpp"

#include "flowvr/moduleapi.h"
#include "flowvr/bufferpool.h"

#include <cmath>

#include <vector>




static void dofilter(  const_Array2D<unsigned char[2]> IN,  Array2D<unsigned char[2]> OUT ) {
    const int inx = IN.height();
    const int iny = IN.width();
    const int outx = OUT.height();
    const int outy = OUT.width();
    const int dx = inx / outx;
    const int dy = iny / outy;
    // filter data
    for ( int x = 0  ;  x < outx  ;  x++ ) {
        for ( int y = 0  ;  y < outy  ;  y++ ) {
            const int X = x*dx;
            const int Y = y*dy;
            float sum0 = 0.f;
            float sum1 = 0.f;
            for ( int i = 0  ;  i < dx  ;  i++ )
            for ( int j = 0  ;  j < dy  ;  j++ ) {
                sum0 += IN[ X+i ][ Y+j ][0];
                sum1 += IN[ X+i ][ Y+j ][1];
            }
            OUT[x][y][0] = sum0 / (dx*dy);
            OUT[x][y][1] = sum1 / (dx*dy);
        }
    }
}



int main( int argc, char **argv ) {
    
    flowvr::InputPort inDensity("in");
    flowvr::InputPort inSync("sync");
    flowvr::OutputPort outDensity("out");//, &PortOutStamps );
    flowvr::Port* PortsArray[] = { &inDensity, &inSync, &outDensity };
    
    int dx = 4;
    int dy = 4;
    if ( argc > 1 ) {
        if ( int factor = atoi(argv[1]) ) {
            // round to the lower power of two
            factor = 1 << (int) log2( factor );
            dx = dy = factor;
        }
    }
    
    const int nbPorts = sizeof(PortsArray) / sizeof(*PortsArray);
    const std::vector< flowvr::Port* > Ports( PortsArray, PortsArray+nbPorts );
    if ( flowvr::ModuleAPI* const module = flowvr::initModule( Ports ) ) {
        flowvr::BufferPool pool; // pool allocation as buffers are of the same size
        bool first = true;
        int nit = 0;
        while ( module->wait() ) {
            if ( first ) {
                first = false;
                module->put( &outDensity, inDensity.stamps );
            }
            static flowvr::StampInfo& inP = * (*inDensity.stamps)["P"];
            static flowvr::StampInfo& inN = * (*inDensity.stamps)["N"];
            static flowvr::StampInfo& outP = * (*outDensity.stamps)["P"];
            static flowvr::StampInfo& outN = * (*outDensity.stamps)["N"];
            
            // adapt frequency
            if ( inSync.isConnected() ) {
                flowvr::Message syncmsg;
                module->get( &inSync, syncmsg );
                const unsigned sync = atoll( syncmsg.data.getRead<char>() );
                if ( sync == 0 ) {
                    continue;
                } else if ( ++nit < sync ) {
                    continue;
                }
                nit = 0;
            }
            
            // get
            flowvr::Message in;
            flowvr::MessageWrite out;
            module->get( &inDensity, in );
            out.stamps.clone( in.stamps, outDensity.stamps );
            int oldP[2]; // start
            int oldN[2]; // dimension of the input grid
            in.stamps.read( inP[0], oldP[0] );
            in.stamps.read( inP[1], oldP[1] );
            in.stamps.read( inN[0], oldN[0] );
            in.stamps.read( inN[1], oldN[1] );
            const int newP[2] = { oldP[0] / dx, oldP[1] / dy };
            const int newN[2] = { oldN[0] / dx, oldN[1] / dy };
            const_Array2D< unsigned char[2] > IN( oldN[1], oldN[0], in.data.getRead<unsigned char[2]>() );
            
            // filter data
            const size_t size = newN[0] * newN[1] * sizeof(unsigned char[2]);
            out.data = pool.alloc( module->getAllocator(), size );
            Array2D< unsigned char[2] > OUT( newN[1], newN[0], out.data.getWrite<unsigned char[2]>() );
            dofilter( IN, OUT );
            
            // output data
            out.stamps.write( outP[0], newP[0] );
            out.stamps.write( outP[1], newP[1] );
            out.stamps.write( outN[0], newN[0] );
            out.stamps.write( outN[1], newN[1] );
            module->put( &outDensity , out );
        }
        module->close();
    }
    return 0;
}
