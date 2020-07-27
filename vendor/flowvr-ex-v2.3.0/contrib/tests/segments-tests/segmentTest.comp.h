/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                                                                 *
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
*  Contact :                                                      *
*                                                                 *
******************************************************************/

#ifndef SEGMENTTEST_COMP_H_
#define SEGMENTTEST_COMP_H_

#include <flowvr/app/core/component.h>

namespace segmentTest
{
	class SegmentTest : public flowvr::app::Composite
	{
	public :
	  SegmentTest(const std::string &id_);
	  virtual void execute();
	  virtual flowvr::app::Component* create() const;

	};
} // end of namespace SegmentTest



#endif // SEGMENTTEST_COMP_H_
