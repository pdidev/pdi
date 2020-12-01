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

#ifndef CEGRENDERTARGET_H_
#define CEGRENDERTARGET_H_

#include <CEGUI/CEGUIRenderTarget.h>
#include <CEGUI/CEGUIRect.h>

namespace CEGUI
{
	class FlowVRCegRenderTarget : public RenderTarget
	{
	public:
		FlowVRCegRenderTarget();
		virtual ~FlowVRCegRenderTarget();

	    virtual void draw(const GeometryBuffer& buffer);
	    virtual void draw(const RenderQueue& queue);
	    virtual void setArea(const Rect& area);
	    virtual const Rect& getArea() const;
	    virtual bool isImageryCache() const;
	    virtual void activate();
	    virtual void deactivate();
	    virtual void unprojectPoint(const GeometryBuffer& buff,
	                                const Vector2& p_in, Vector2& p_out) const;

	private:
	    Rect m_area;
	};
}


#endif // CEGRENDERTARGET_H_
