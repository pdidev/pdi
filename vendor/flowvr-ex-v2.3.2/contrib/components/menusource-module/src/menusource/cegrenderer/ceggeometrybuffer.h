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

#ifndef CEGGEOMETRYBUFFER_H_
#define CEGGEOMETRYBUFFER_H_

#include <CEGUI/CEGUIGeometryBuffer.h>
#include <CEGUI/CEGUIVector.h>
#include <CEGUI/CEGUIRect.h>
#include <ftl/mat.h>
#include <flowvr/id.h>
#include <vector>
#include <map>


namespace CEGUI
{
	class RenderEffect;
	class Texture;
	class Rect;

	class FlowVRCegRenderer;
	class FlowVRCegRenderTarget;

	class FlowVRCegGeometryBuffer : public GeometryBuffer
	{
	public:
		FlowVRCegGeometryBuffer( FlowVRCegRenderer &parent );
		virtual ~FlowVRCegGeometryBuffer();


	    /*!
	    \brief
	        Draw the geometry buffered within this GeometryBuffer object.
	    */
	    virtual void draw() const;

	    /*!
	    \brief
	        Set the translation to be applied to the geometry in the buffer when it
	        is subsequently rendered.

	    \param v
	        Vector3 describing the three axis translation vector to be used.
	    */
	    virtual void setTranslation(const Vector3& v);

	    /*!
	    \brief
	        Set the rotations to be applied to the geometry in the buffer when it is
	        subsequently rendered.

	    \param r
	        Vector3 describing the rotation factors to be used.
	    */
	    virtual void setRotation(const Vector3& r);

	    /*!
	    \brief
	        Set the pivot point to be used when applying the rotations.

	    \param p
	        Vector3 describing the location of the pivot point to be used when
	        applying the rotation to the geometry.
	    */
	    virtual void setPivot(const Vector3& p);

	    /*!
	    \brief
	        Set the clipping region to be used when rendering this buffer.
	    */
	    virtual void setClippingRegion(const Rect& region);

	    /*!
	    \brief
	        Append a single vertex to the buffer.

	    \param vertex
	        Vertex object describing the vertex to be added to the GeometryBuffer.
	    */
	    virtual void appendVertex(const Vertex& vertex);

	    /*!
	    \brief
	        Append a number of vertices from an array to the GeometryBuffer.

	    \param vbuff
	        Pointer to an array of Vertex objects that describe the vertices that
	        are to be added to the GeometryBuffer.

	    \param vertex_count
	        The number of Vertex objects from the array \a vbuff that are to be
	        added to the GeometryBuffer.
	    */
	    virtual void appendGeometry(const Vertex* const vbuff, uint vertex_count);

	    /*!
	    \brief
	        Set the active texture to be used with all subsequently added vertices.

	    \param texture
	        Pointer to a Texture object that shall be used for subsequently added
	        vertices.  This may be 0, in which case texturing will be disabled for
	        subsequently added vertices.
	    */
	    virtual void setActiveTexture(Texture* texture);

	    /*!
	    \brief
	        Clear all buffered data and reset the GeometryBuffer to the default
	        state.
	    */
	    virtual void reset();

	    /*!
	    \brief
	        Return a pointer to the currently active Texture object.  This may
	        return 0 if no texture is set.

	    \return
	        Pointer the Texture object that is currently active, or 0 if texturing
	        is not being used.
	    */
	    virtual Texture* getActiveTexture() const;

	    /*!
	    \brief
	        Return the total number of vertices currently held by this
	        GeometryBuffer object.

	    \return
	        The number of vertices that have been appended to this GeometryBuffer.
	    */
	    virtual uint getVertexCount() const;

	    /*!
	    \brief
	        Return the number of batches of geometry that this GeometryBuffer has
	        split the vertices into.

	    \note
	        How batching is done will be largely implementation specific, although
	        it would be reasonable to expect that you will have <em>at least</em>
	        one batch of geometry per texture switch.

	    \return
	        The number of batches of geometry held by the GeometryBuffer.
	    */
	    virtual uint getBatchCount() const;

	    /*!
	    \brief
	        Set the RenderEffect to be used by this GeometryBuffer.

	    \param effect
	        Pointer to the RenderEffect to be used during rendering of the
	        GeometryBuffer.  May be 0 to remove a previously added RenderEffect.

	    \note
	        When adding a RenderEffect, the GeometryBuffer <em>does not</em> take
	        ownership of, nor make a copy of, the passed RenderEffect - this means
	        you need to be careful not to delete the RenderEffect if it might still
	        be in use!
	    */
	    virtual void setRenderEffect(RenderEffect* effect);

	    /*!
	    \brief
	        Return the RenderEffect object that is assigned to this GeometryBuffer
	        or 0 if none.
	    */
	    virtual RenderEffect* getRenderEffect();


	    ///////////////////////////////////////////////////////////////////////
	    // FLOWVR-RENDER STUFF
	    ///////////////////////////////////////////////////////////////////////

		struct LcVertex
		{
	    	LcVertex()
	    	: pos()
	    	, uv()
	    	, col()
	    	{}

	    	LcVertex( const ftl::Vec3f &_pos, const ftl::Vec2f &_uv, const ftl::Vec4f &_col )
	    	: pos(_pos)
	    	, uv(_uv)
	    	, col(_col)
	    	{}

			ftl::Vec3f pos;
			ftl::Vec2f uv;
			ftl::Vec4f col;
		};

	    struct BatchInfo
	    {
		    mutable std::vector<LcVertex> m_vertices;
	    };

	    typedef std::map<Texture*, BatchInfo> BATCHMAP;
	    mutable BATCHMAP m_batchInfos;
	    BatchInfo *activeBatch;

	    unsigned long long m_VbId,
	                       m_IbId,
	                       m_VsId,
	                       m_PsId;

	    Vector3 m_translation, m_rotation, m_pivot;
	    mutable ftl::Mat4x4f m_composedT;

	private:
	    void updateTransform() const;
	    void updateGeometry() const;
	public:
	    class prim
		{
		public:
			prim()
			: m_id(0)
			, m_vbId(0)
			, m_ibId(0)
			{}

			prim( flowvr::ID id, flowvr::ID vbId, flowvr::ID ibId )
			: m_id(id)
			, m_vbId(vbId)
			, m_ibId(ibId)
			{

			}

			flowvr::ID m_id,
			   m_vbId,
			   m_ibId;
		};

	    prim getPrimitiveForTexture( Texture * tx ) const;
	    prim createPrimitiveForTexture( Texture * ) const;

	    typedef std::map<Texture*, prim> TEXMAP;
	    mutable TEXMAP m_mpbatchMap;


	private:

	    RenderEffect          *m_effect;
	    Texture               *m_activeTexture;
	    FlowVRCegRenderer     &m_parent;
	    Rect m_clipArea;

	    mutable bool m_bTransDirty, m_bGeomDirty;
	};
}


#endif // CEGGEOMETRYBUFFER_H_
