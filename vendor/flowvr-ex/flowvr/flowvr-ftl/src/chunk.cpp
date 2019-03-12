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
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: ./src/chunk.cpp                                           *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#include <ftl/chunk.h>

#include <string>
#include <iostream>

namespace ftl
{

/// Print chunk info to stdout
void Chunk::print() const
{

  std::cout << "Chunk @ "  << this << ":\t"
	    << "  size = " << size << "\t"
	    << "  type = " << type << "\n";


}



} // namespace ftl
