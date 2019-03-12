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
* File: cegrendertarget.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include "cegrendertarget.h"

#include <CEGUI/CEGUIRenderQueue.h>
#include <CEGUI/CEGUIGeometryBuffer.h>

namespace CEGUI
{
	FlowVRCegRenderTarget::FlowVRCegRenderTarget()
	: m_area(0,0,0,0)
	{

	}

	FlowVRCegRenderTarget::~FlowVRCegRenderTarget()
	{

	}

	void FlowVRCegRenderTarget::draw(const GeometryBuffer& buffer)
	{
		buffer.draw();
	}

	void FlowVRCegRenderTarget::draw(const RenderQueue& queue)
	{
		queue.draw();
	}

	void FlowVRCegRenderTarget::setArea(const Rect& area)
	{
		m_area = area;
	}

	const Rect& FlowVRCegRenderTarget::getArea() const
	{
	 return m_area;
	}

	bool FlowVRCegRenderTarget::isImageryCache() const
	{
		return false;
	}

	void FlowVRCegRenderTarget::activate()
	{

	}

	void FlowVRCegRenderTarget::deactivate()
	{

	}

	void FlowVRCegRenderTarget::unprojectPoint(const GeometryBuffer& buff,
							const Vector2& p_in, Vector2& p_out) const
	{
		p_out = p_in;
	}
}
