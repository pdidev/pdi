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


#include "flowvr/moduleapi.h"

#include <cassert>
#include <cmath>
#include <cstddef>
#include <cstdio>

#include <vector>

#include "fluid/Array2D.hpp"



namespace {

enum MASK {
    NONE    = 0U,
    A       = 1U,
    B       = 2U,
    C       = 4U,
    D       = 8U,
    AB      = A|B,
    AC      = A|C,
    AD      = A|D,
    BC      = B|C,
    BD      = B|D,
    CD      = C|D,
    ABC     = A|B|C,
    ABD     = A|B|D,
    ACD     = A|C|D,
    BCD     = B|C|D,
    ABCD    = A|B|C|D
};



static
float lerp( float x1, float x2, float l1, float l2, float isovalue ) {
    const float t = (isovalue - l2) / (l1 - l2);
    return t*(x1-x2) + x2;
}


template< int CORNER, typename T >
static
void corner( std::vector<float>& v, int i, int j, const_Array2D<T> grid, float isovalue ) {
    switch ( CORNER ) {
    case A:  case BCD: {
        const float sj = lerp(j, j+1, grid(i,  j  ), grid(i  ,j+1), isovalue);
        const float si = lerp(i, i+1, grid(i,  j  ), grid(i+1,j  ), isovalue);
        const float m[4] = { si, j,  i, sj };
        v.insert( v.end(), m, m+4 );
        break;
    }
    case B:  case ACD: {
        const float sj = lerp(j, j+1, grid(i+1,j  ), grid(i+1,j+1), isovalue);
        const float si = lerp(i, i+1, grid(i,  j  ), grid(i+1,j  ), isovalue);
        const float m[4] = { si, j,  i+1, sj };
        v.insert( v.end(), m, m+4 );
        break;
    }
    case C:  case ABD: {
        const float sj = lerp(j, j+1, grid(i,  j  ), grid(i,  j+1), isovalue);
        const float si = lerp(i, i+1, grid(i,  j+1), grid(i+1,j+1), isovalue);
        const float m[4] = { si, j+1,  i, sj };
        v.insert( v.end(), m, m+4 );
        break;
    }
    case D:  case ABC: {
        const float sj = lerp(j, j+1, grid(i+1,j  ), grid(i+1,j+1), isovalue);
        const float si = lerp(i, i+1, grid(i,  j+1), grid(i+1,j+1), isovalue);
        const float m[4] = { si, j+1,  i+1, sj };
        v.insert( v.end(), m, m+4 );
        break;
    }
    default:break;
    }
}






template< typename T >
static
void processs_cell( std::vector<float>& v, int i, int j, const_Array2D<T> grid, float isovalue ) {
    const unsigned below[4] = {
            (grid(i  ,j  ) <= isovalue),
            (grid(i+1,j  ) <= isovalue),
            (grid(i  ,j+1) <= isovalue),
            (grid(i+1,j+1) <= isovalue)
    };
    const unsigned mask = below[0]*A | below[1]*B | below[2]*C | below[3]*D;
    
    switch( mask ) {
    // no isoline in this cell
    default: case NONE: case ABCD:
        break;
    // one single corner
    case A:  case BCD:
        corner<A>(v, i, j, grid, isovalue );
        break;
    case B:  case ACD:
        corner<B>(v, i, j, grid, isovalue );
        break;
    case C:  case ABD:
        corner<C>(v, i, j, grid, isovalue );
        break;
    case D:  case ABC:
        corner<D>(v, i, j, grid, isovalue );
        break;
    // Two opposite corners
    case AD:
    case BC:{
        const float barycenter = (grid(i,j)+grid(i+1,j)+grid(i,j+1)+grid(i+1,j+1))/4.0;
        if( (mask == AD  &&  barycenter <= isovalue)
            || (mask == BC  &&  barycenter >  isovalue) ) {
            corner<B>( v, i,j, grid, isovalue );
            corner<C>( v, i,j, grid, isovalue );
        } else {
            corner<A>( v, i,j, grid, isovalue );
            corner<D>( v, i,j, grid, isovalue );
        }
        break;
    }
    // straight line splitting the cell in two
    case AC:
    case BD:{
        const float si1 = lerp(i, i+1, grid(i,j), grid(i+1,j), isovalue);
        const float si2 = lerp(i, i+1, grid(i,j+1), grid(i+1,j+1), isovalue);
        const float m[4] = { si1, j,  si2, j+1 };
        v.insert( v.end(), m, m+4 );
        break;}
    case AB:
    case CD:{
        const float sj1 = lerp(j, j+1, grid(i,j), grid(i,j+1), isovalue);
        const float sj2 = lerp(j, j+1, grid(i+1,j), grid(i+1,j+1), isovalue);
        const float m[4] = { i, sj1,  i+1, sj2 };
        v.insert( v.end(), m, m+4 );
        break;}
    }
}

template< typename T, int N >
static
void processs_grid( std::vector<float> & v, const_Array2D<T>  grid, const float (&isovalue)[N] ) {
    const int ni = grid.height();
    const int nj = grid.width();
    for ( int i = 0  ;  i < ni-1  ;  i++ ) {
        for ( int j = 0  ;  j < nj-1  ;  j++ ) {
            for( int k = 0  ;  k < N  ;  k++ ) {
                processs_cell( v, i,j, grid, isovalue[k] );
            }
        }
    }
}


}// anonymous namespace





int main( int argc, char **argv ) {
    
    float nx = atoi(argv[1]);
    float ny = atoi(argv[2]);

    int dx = 4;
    int dy = 4;
    if ( argc > 3 ) {
        if ( int factor = atoi(argv[3]) ) {
            // round to the lower power of two
            factor = 1 << (int) log2( factor );
            dx = dy = factor;
        }
    }
    
    flowvr::InputPort inDensity("density");
    flowvr::OutputPort outVertices("isolines");
    flowvr::Port* PortsArray[] = { &inDensity, &outVertices };
    
    const int nbPorts = sizeof(PortsArray) / sizeof(*PortsArray);
    const std::vector< flowvr::Port* > Ports( PortsArray, PortsArray+nbPorts );
    if ( flowvr::ModuleAPI* const module = flowvr::initModule( Ports ) ) {
        while ( module->wait() ) {
            static flowvr::StampInfo& inN = * (*inDensity.stamps)["N"];
            
            // get
            flowvr::Message in;
            module->get( &inDensity, in );
            int newN[2]; // dimension of the input grid
            in.stamps.read( inN[0], newN[0] );
            in.stamps.read( inN[1], newN[1] );
            
            // keep the bleue fluid only
            static std::vector< unsigned char > bleue;
            bleue.resize( newN[0] * newN[1] );
            for ( int i = 0  ;  i < newN[0] * newN[1]  ;  i++ ) {
                bleue[i] = in.data.getRead<char>()[ 2*i + 1 ];
            }
            const_Array2D< unsigned char > IN( newN[0], newN[1], &bleue[0] );
            
            // compute isolines
            flowvr::MessageWrite out;
            static std::vector<float> vertices;
            const float isovalues[] = { 32, 64, 96, 128, 160, 192, 224 };
            processs_grid( vertices, IN, isovalues );
            out.data =  module->alloc( vertices.size() * sizeof(float) );
            float * const p = out.data.getWrite<float>();
            for ( int i=1, end=vertices.size()  ;  i < end  ;  i+=2 ) {
                p[i-1] = (0.5f + vertices[i-0]) * (dx / nx);
                p[i-0] = (0.5f + vertices[i-1]) * (dy / ny);
            }
            vertices.clear();
            
            // put
            module->put( &outVertices , out );
        }
        module->close();
    }
    return 0;
}
