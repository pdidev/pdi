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

#ifndef CEGTEXTURETARGET_H_
#define CEGTEXTURETARGET_H_

#include <CEGUI/CEGUITextureTarget.h>
#include <CEGUI/CEGUISize.h>
#include <CEGUI/CEGUIRect.h>

#include <map>
#include <list>

#include <flowvr/id.h>

namespace flowvr
{

	namespace render
	{
		class ChunkRenderWriter;
	}
}

namespace CEGUI
{
	class Texture;
	class FlowVRCegRenderer;
	class FlowVRCegTexture;
	class FlowVRCegGeometryBuffer;

	class FlowVRCegTextureTarget : public TextureTarget
	{
	public:
			FlowVRCegTextureTarget( unsigned int TGId, FlowVRCegRenderer &parent, const std::string &strName );
			~FlowVRCegTextureTarget();

			//////////////////////////////////////////////////////
			// FROM RENDERTARGET
			//////////////////////////////////////////////////////

		   /*!
			\brief
				Draw geometry from the given GeometryBuffer onto the surface that
				this RenderTarget represents.

			\param buffer
				GeometryBuffer object holding the geometry that should be drawn to the
				RenderTarget.
			*/
			virtual void draw(const GeometryBuffer& buffer);

			/*!
			\brief
				Draw geometry from the given RenderQueue onto the surface that
				this RenderTarget represents.

			\param queue
				RenderQueue object holding the geometry that should be drawn to the
				RenderTarget.
			*/
			virtual void draw(const RenderQueue& queue);

			/*!
			\brief
				Set the area for this RenderTarget.  The exact action this function
				will take depends upon what the concrete class is representing.  For
				example, with a 'view port' style RenderTarget, this should set the area
				that the view port occupies on the display (or rendering window).

			\param area
				Rect object describing the new area to be assigned to the RenderTarget.

			\exception InvalidRequestException
				May be thrown if the RenderTarget does not support setting or changing
				its area, or if the area change can not be satisfied for some reason.
			*/
			virtual void setArea(const Rect& area);

			/*!
			\brief
				Return the area defined for this RenderTarget.

			\return
				Rect object describing the currently defined area for this RenderTarget.
			*/
			virtual const Rect& getArea() const;

			/*!
			\brief
				Return whether the RenderTarget is an implementation that caches
				actual rendered imagery.

				Typically it is expected that texture based RenderTargets would return
				true in response to this call.  Other types of RenderTarget, like
				view port based targets, will more likely return false.

			\return
				- true if the RenderTarget does cache rendered imagery.
				- false if the RenderTarget does not cache rendered imagery.
			*/
			virtual bool isImageryCache() const;

			/*!
			\brief
				Activate the render target and put it in a state ready to be drawn to.

			\note
				You MUST call this before doing any rendering - if you do not call this,
				in the unlikely event that your application actually works, it will
				likely stop working in some future version.
			*/
			virtual void activate();

			/*!
			\brief
				Deactivate the render target after having completed rendering.

			\note
				You MUST call this after you finish rendering to the target - if you do
				not call this, in the unlikely event that your application actually
				works, it will likely stop working in some future version.
			*/
			virtual void deactivate();

			/*!
			\brief
				Take point \a p_in unproject it and put the result in \a p_out.
				Resulting point is local to GeometryBuffer \a buff.
			*/
			virtual void unprojectPoint(const GeometryBuffer& buff,
										const Vector2& p_in, Vector2& p_out) const;

			//////////////////////////////////////////////////////////////
			// FROM TextureTarget
			//////////////////////////////////////////////////////////////
		   /*!
		    \brief
		        Clear the surface of the underlying texture.
		    */
		    virtual void clear();

		    /*!
		    \brief
		        Return a pointer to the CEGUI::Texture that the TextureTarget is using.

		    \return
		        Texture object that the TextureTarget uses when rendering imagery.
		    */
		    virtual Texture& getTexture() const;

		    /*!
		    \brief
		        Used to declare to the TextureTarget the largest size, in pixels, of the
		        next set of incoming rendering operations.

		    \note
		        The main purpose of this is to allow for the implementation to resize
		        the underlying texture so that it can hold the imagery that will be
		        drawn.

		    \param sz
		        Size object describing the largest area that will be rendererd in the
		        next batch of rendering operations.

		    \exception InvalidRequestException
		        May be thrown if the TextureTarget would not be able to handle the
		        operations rendering content of the given size.
		    */
		    virtual void declareRenderSize(const Size& sz);

		    /*!
		    \brief
		        Return whether rendering done on the target texture is inverted in
		        relation to regular textures.

		        This is intended to be used when generating geometry for rendering the
		        TextureTarget onto another surface.

		    \return
		        - true if the texture content should be considered as inverted
		        vertically in comparison with other regular textures.
		        - false if the texture content has the same orientation as regular
		        textures.
		    */
		    virtual bool isRenderingInverted() const ;


		    /////////////////////////////////////////////////////////////////////
		    // FLOWVR STUFF
		    /////////////////////////////////////////////////////////////////////
		    unsigned int getFBOID() const;
	private:

		    void updateTexture();

		    Size m_size;
		    Rect m_area;

		    // flowvr-related stuff
		    unsigned int  m_nFBOId;
		    flowvr::ID m_nCamID,
		               m_txId;


			flowvr::ID   m_MPrim,
			             m_MVbId,
			             m_mIbId;

		    FlowVRCegTexture  *m_tx;
		    FlowVRCegRenderer &m_parent;

		    class prim
		    {
		    public:
		    	prim()
		    	: m_id(0)
		    	, m_vbId(0)
		    	, m_ibId(0)
		    	, m_buffers(NULL)
		    	, m_numVerts(0)
		    	{}

		    	prim( flowvr::ID id, flowvr::ID vbId, flowvr::ID ibId )
		    	: m_id(id)
		    	, m_vbId(vbId)
		    	, m_ibId(ibId)
		    	, m_numVerts(0)
		    	{

		    	}

		    	flowvr::ID m_id,
		    	   m_vbId,
		    	   m_ibId;

		    	size_t m_numVerts;

		    	std::list<FlowVRCegGeometryBuffer*> *m_buffers;
		    };

		    typedef std::map<Texture*, prim > TX2GB;
		    TX2GB m_txMap;

		    std::string m_strName;
	};
}


#endif // CEGTEXTURETARGET_H_
