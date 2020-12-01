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
* File: ceggemoetrybuffer.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include "ceggeometrybuffer.h"
#include "cegrenderer.h"
#include "cegrendertarget.h"
#include "cegtexturetarget.h"
#include "cegtexture.h"

#include <CEGUI/CEGUIVertex.h>
#include <CEGUI/CEGUIRenderingRoot.h>

#include <algorithm>
#include <cmath>
#include <limits>

#include <ftl/quat.h>
#include <ftl/convert.h>

#include <flowvr/render/chunkrenderwriter.h>

#include <fstream>

using namespace flowvr;
using namespace flowvr::render;
using namespace ftl;


namespace
{

	void dumpAC3D( const std::string &strFileName, ChunkVertexBuffer *vb, const std::string &txName, const ftl::Mat4x4f &m )
	{
		char buffer[4096];
		std::ofstream out(strFileName.c_str());

		if( !out.good() )
		{
			std::cerr << "could not open file [" << strFileName << "]" << std::endl;
			return;
		}

		out << "AC3Db\n";
		out << "MATERIAL \"ac3dmat1\" rgb 1 1 1  amb 0.2 0.2 0.2  emis 0 0 0  spec 0.2 0.2 0.2  shi 128  trans 0" << std::endl;
		out << "OBJECT world\nkids 1\nOBJECT dump\nname \"dump\"\nloc 0 0 0" << std::endl;

		if(!txName.empty())
			out << "texture " << txName << std::endl;

		size_t numvert = vb->dataSize() / vb->vertexSize;
		size_t numtri  = numvert / 3;

		out << "numvert " << numvert << std::endl;

		CEGUI::FlowVRCegGeometryBuffer::LcVertex *vdata = (CEGUI::FlowVRCegGeometryBuffer::LcVertex*)vb->data();

		for( size_t n=0; n < numvert; ++n )
		{
			Vec3f v = ftl::transform( m, vdata[n].pos );
			out << v.x() << " " <<
					v.y() << " " <<
					v.z() << std::endl;
		}

		out << "numsurf " << numtri << std::endl;


		for( size_t s = 0; s < numtri; ++s )
		{
			out << "SURF 0x20\nmat 0\nrefs 3\n";
			for( size_t v = 0; v < 3; ++v )
			{
				size_t index = (numtri*3) - (s*3+v) - 1;
				out << index << " " << vdata[index].uv[0] << " " << vdata[index].uv[1] << "\n";
			}
		}

		out << "kids 0\n";
	}


	using namespace CEGUI;
	class _countVertices : public std::unary_function<CEGUI::FlowVRCegGeometryBuffer::BATCHMAP::value_type, void>
	{
	public:
		_countVertices()
		: m_sum(0)
		{

		}

		void operator()( const CEGUI::FlowVRCegGeometryBuffer::BATCHMAP::value_type &v )
		{
			m_sum += v.second.m_vertices.size();
		}

		size_t m_sum;
	};

	class _mult : public std::binary_function<Vertex,Mat4x4f,Vertex>
	{
	public:
		Vertex operator()( const Vertex &v, const Mat4x4f &m ) const
		{
			Vec3f vT( v.position.d_x, v.position.d_y, v.position.d_z );
			Vec3f o = transform(m,vT);

			Vertex out = v;
			out.position.d_x = o.x();
			out.position.d_y = o.y();
			out.position.d_z = o.z();

//			std::cout << ftl::toString(m) << std::endl;
//			std::cout << ftl::toString(o) << std::endl;
//			std::cout << "in = [" << v.position.d_x << " ; " << v.position.d_y << " ; " << v.position.d_z << "]" <<std::endl
//					  << "out= [" << out.position.d_x << " ; " << out.position.d_y << " ; " << out.position.d_z << "]" <<std::endl;

			return out;
		}
	};



	class _delPrimitive : public std::unary_function<CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type, void>
	{
		public:
		_delPrimitive( flowvr::render::ChunkRenderWriter &scene )
		: m_scene(scene)
		{}

		void operator()( CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type &info )
		{
			m_scene.delPrimitive( info.second.m_id );
			m_scene.delVertexBuffer( info.second.m_vbId );
			m_scene.delIndexBuffer( info.second.m_ibId );
		}

		flowvr::render::ChunkRenderWriter &m_scene;
	};

	template<class T>
	class _setParam : public std::unary_function<CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type, void>
	{
		public:
		_setParam( flowvr::render::ChunkRenderWriter &scene, const T &prm, int param )
		: m_scene(scene)
		, m_prm(prm)
		, m_paramId(param)
		{}

		void operator()( CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type &info )
		{
//			std::cout << "setting [" << info.second.m_id << "'s transform to [" << m_mat << "]" << std::endl;
			m_scene.addParam( info.second.m_id, m_paramId, "", m_prm );
		}

		flowvr::render::ChunkRenderWriter &m_scene;
		T   m_prm;
		int m_paramId;
	};

	class _setTargetGroup : public std::unary_function<CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type, void>
	{
		public:
		_setTargetGroup( flowvr::render::ChunkRenderWriter &scene, unsigned int tg )
		: m_scene(scene)
		, m_tg(tg)
		{}

		void operator()( CEGUI::FlowVRCegGeometryBuffer::TEXMAP::value_type &info )
		{
			std::cout << "setting [" << info.second.m_id << "'s targetgroup to [" << m_tg << "]" << std::endl;
			m_scene.addParam( info.second.m_id, ChunkPrimParam::TARGETGROUP, "",  1 << m_tg );
		}

		flowvr::render::ChunkRenderWriter &m_scene;
		unsigned int m_tg;
	};


}

namespace CEGUI
{
	FlowVRCegGeometryBuffer::FlowVRCegGeometryBuffer(FlowVRCegRenderer &parent)
	: GeometryBuffer()
	, m_effect(NULL)
	, m_parent(parent)
	, m_VbId(~0)
	, m_IbId(~0)
	, m_VsId(~0)
	, m_PsId(~0)
	, m_activeTexture(NULL)
	, m_translation( Vector3(0,0,0) )
	, m_rotation( Vector3(0,0,0) )
	, m_pivot( Vector3(0,0,0) )
	, m_bTransDirty(false)
	, m_bGeomDirty(false)
	, activeBatch(NULL)
	{
		m_composedT.identity();
	}

	FlowVRCegGeometryBuffer::~FlowVRCegGeometryBuffer()
	{
		std::for_each( m_mpbatchMap.begin(), m_mpbatchMap.end(), _delPrimitive( m_parent.getChunkWriter() ) );
	}


	void FlowVRCegGeometryBuffer::draw() const
	{
		if( m_bGeomDirty )
		{
			FlowVRCegTextureTarget *currentTarget = dynamic_cast<FlowVRCegTextureTarget*>( m_parent.getCurrentRenderTarget() );
			if( currentTarget == NULL )
				currentTarget = m_parent.m_defaultRenderTarget;
			updateGeometry();
//			std::cout << "geom dirty, adjusting target-group to [" <<  TargetGroups::FBO2Tg(currentTarget->getFBOID()) << "]" << std::endl;
//			std::for_each( m_mpbatchMap.begin(), m_mpbatchMap.end(), _setTargetGroup( m_parent.getChunkWriter(), TargetGroups::FBO2Tg(currentTarget->getFBOID()) ) );
		}
		if( m_bTransDirty )
			updateTransform();
	}


	void FlowVRCegGeometryBuffer::setTranslation(const Vector3& v)
	{
		m_translation = v;
		m_bTransDirty = true;
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::setTranslation() -- called [" << v.d_x << " ; " << v.d_y << " ; " << v.d_z << "]"<< std::endl;
	}

	void FlowVRCegGeometryBuffer::setRotation(const Vector3& r)
	{
		m_rotation = r;
		m_bTransDirty = true;
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::setRotation() -- called [" << r.d_x << " ; " << r.d_y << " ; " << r.d_z << "]"<< std::endl;
	}

	void FlowVRCegGeometryBuffer::setPivot(const Vector3& p)
	{
		m_pivot = p;
		m_bTransDirty = true;
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::setPivot() -- called [" << p.d_x << " ; " << p.d_y << " ; " << p.d_z << "]"<< std::endl;
	}

    void FlowVRCegGeometryBuffer::setClippingRegion(const Rect& region)
    {
//    	m_geomDirty = true;
//    	std::cout << "FlowVRCegGeometryBuffer[" << this << "]::setClippingRegion() -- called " << std::endl;
    	m_clipArea = region;
    }

	void FlowVRCegGeometryBuffer::appendVertex(const Vertex& v)
	{
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::appendVertex() called." << std::endl;
		(*activeBatch).m_vertices.push_back(LcVertex( Vec3f(v.position.d_x, v.position.d_y, v.position.d_z),
                Vec2f(v.tex_coords.d_x, v.tex_coords.d_y),
                Vec4f(v.colour_val.getRed(),
						v.colour_val.getGreen(),
						v.colour_val.getBlue(),
						v.colour_val.getAlpha() ) ));
//		m_vertices.push_back( vertex );
//		m_geomDirty = true;

		m_bGeomDirty = true;
	}

	void FlowVRCegGeometryBuffer::appendGeometry(const Vertex* const vbuff, uint vertex_count)
	{
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::appendGeometry() called." << std::endl;

		for( size_t n = 0; n < vertex_count; ++n )
		{
			const Vertex &v = *(vbuff+n);
			(*activeBatch).m_vertices.push_back( LcVertex( Vec3f(v.position.d_x, v.position.d_y, v.position.d_z),
					                                       Vec2f(v.tex_coords.d_x, v.tex_coords.d_y),
					                                       Vec4f(v.colour_val.getRed(),
					               								v.colour_val.getGreen(),
					               								v.colour_val.getBlue(),
					               								v.colour_val.getAlpha() ) ) );
		}

		m_bGeomDirty = true;
	}

	void FlowVRCegGeometryBuffer::setActiveTexture(Texture* texture)
	{
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::setActiveTexture() -- " << this << " changed texture from: " << m_activeTexture
//				  << " [" << (m_activeTexture ? static_cast<FlowVRCegTexture*>(m_activeTexture)->getFileName():"NULL") << "] to texture @ " << texture
//				  << " [" << (texture ? static_cast<FlowVRCegTexture*>(texture)->getFileName():"NULL") << "]" << std::endl;
		m_activeTexture = texture;

		if( m_activeTexture )
		{
			BATCHMAP::iterator cit = m_batchInfos.find( m_activeTexture );
			if( cit == m_batchInfos.end() )
			{
				m_batchInfos[ m_activeTexture ] = BatchInfo();
				activeBatch = &m_batchInfos[ m_activeTexture ];
			}
			else
			{
				activeBatch = &(*cit).second;
			}
		}
		else
			activeBatch = NULL;
	}

	void FlowVRCegGeometryBuffer::reset()
	{
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::reset() called" << std::endl;
		m_batchInfos.clear();
		m_activeTexture = NULL;
	}

	Texture* FlowVRCegGeometryBuffer::getActiveTexture() const
	{
		return m_activeTexture;
	}

	uint FlowVRCegGeometryBuffer::getVertexCount() const
	{
		_countVertices c = std::for_each( m_batchInfos.begin(), m_batchInfos.end(), _countVertices() );
//		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::getVertexCount() has [" << c.m_sum << "] vertices." << std::endl;
		return c.m_sum;
	}

	uint FlowVRCegGeometryBuffer::getBatchCount() const
	{
		return m_batchInfos.size();
	}

	void FlowVRCegGeometryBuffer::setRenderEffect(RenderEffect* effect)
	{
		m_effect = effect;
	}

	RenderEffect* FlowVRCegGeometryBuffer::getRenderEffect()
	{
		return m_effect;
	}

	void FlowVRCegGeometryBuffer::updateTransform() const
	{
//		std::cout << "void FlowVRCegGeometryBuffer[" << this << "]::updateTransform() const" << std::endl;
		m_bTransDirty = false;

		Mat4x4f rx = matrixRotation( m_rotation.d_x, Vec3f(1,0,0) );
		Mat4x4f ry = matrixRotation( m_rotation.d_y, Vec3f(0,1,0) );
		Mat4x4f rz = matrixRotation( m_rotation.d_z, Vec3f(0,0,1) );

		Mat4x4f t  = matrixTranslation( Vec3f( m_translation.d_x, m_translation.d_y, m_translation.d_z )+Vec3f( m_pivot.d_x, m_pivot.d_y, m_pivot.d_z ) );
		Mat4x4f p  = matrixTranslation(-Vec3f( m_pivot.d_x, m_pivot.d_y, m_pivot.d_z ) );

		m_composedT =  p * rx * ry * rz * t;

//		std::cout << "final mat: " << toString(m_composedT) << std::endl;
		// set transform
		std::for_each( m_mpbatchMap.begin(), m_mpbatchMap.end(), _setParam<Mat4x4f>( m_parent.getChunkWriter(), m_composedT, ChunkPrimParam::TRANSFORM ) );

		// apply projection matrix for this primitive for the main display
		Size sz = m_parent.getDisplaySize();
		Mat4x4f op = ftl::matrixOrthoProjection( 0.0f, float(sz.d_width), float(sz.d_height), 0.0f, 0.0f, 1.0f );

		std::for_each( m_mpbatchMap.begin(), m_mpbatchMap.end(), _setParam<Mat4x4f>( m_parent.getChunkWriter(), op, ChunkPrimParam::PROJECTION ) );

	}


	void FlowVRCegGeometryBuffer::updateGeometry() const
	{
		std::cout << "FlowVRCegGeometryBuffer[" << this << "]::updateGeometry() -- called." << std::endl;
		ChunkRenderWriter &scene = m_parent.getChunkWriter();
		int vdataBufferTypes[3] = {Type::Vec3f, Type::Vec2f, Type::Vec4f};

		for( BATCHMAP::iterator cit = m_batchInfos.begin(); cit != m_batchInfos.end(); ++cit )
		{
			prim p = getPrimitiveForTexture( (*cit).first );
			if( !p.m_ibId or !p.m_vbId )
			{
				std::cerr << "FlowVRCegGeometryBuffer::updateGeometry() -- could not get primitive!" << std::endl;
				continue;
			}

			size_t numVertices = (*cit).second.m_vertices.size();

			std::cout << "\t --- adding [" << numVertices << "] vertices to primitive." << std::endl;

			ChunkVertexBuffer* vb = scene.addVertexBuffer(p.m_vbId, numVertices, 3, vdataBufferTypes);
			int indexBufferType = Type::Null;
			ChunkIndexBuffer* ib = scene.addIndexBuffer(p.m_ibId, 0, indexBufferType, ChunkIndexBuffer::Triangle);

			// Fill vdata and index buffers
			LcVertex* vertex = (LcVertex*)vb->data();
			memcpy( vertex, &(*cit).second.m_vertices[0], numVertices * sizeof(LcVertex) );

			if( numVertices )
			{
				std::string txName = static_cast<FlowVRCegTexture*>((*cit).first)->getFileName();
				dumpAC3D("out_"+ftl::toString<int>(numVertices)+".ac", vb, txName, m_composedT );
			}
		}


		m_bGeomDirty = false;
	}

	FlowVRCegGeometryBuffer::prim FlowVRCegGeometryBuffer::getPrimitiveForTexture( Texture * tx ) const
	{
		TEXMAP::const_iterator it = m_mpbatchMap.find(tx);
		if( it == m_mpbatchMap.end() )
		{
			prim p = createPrimitiveForTexture(tx);
			m_mpbatchMap[tx] = p;
			return p;
		}
		return (*it).second;
	}

	FlowVRCegGeometryBuffer::prim FlowVRCegGeometryBuffer::createPrimitiveForTexture( Texture *tx ) const
	{

		FlowVRCegTexture *fvrtx = dynamic_cast<FlowVRCegTexture*>(tx);
		if(!fvrtx)
			return prim(); // should not happen

		ChunkRenderWriter &scene = m_parent.getChunkWriter();

		prim p( m_parent.generateID(), m_parent.generateID(), m_parent.generateID() );

		std::cout << "FlowVRCegGeometryBuffer::createPrimitiveForTexture() -- creating primitive [" << p.m_id << "]" << std::endl;
		scene.addPrimitive( p.m_id, ("menu-tx-gb["+fvrtx->getFileName()+"]") ); // no tg-group here on purpose

		scene.addParamID( p.m_id, ChunkPrimParam::VSHADER,"", m_parent.m_nIdVs );
		scene.addParamID( p.m_id, ChunkPrimParam::PSHADER,"", m_parent.m_nIdPs );

		scene.addParamID( p.m_id, ChunkPrimParam::VBUFFER_ID,"position", p.m_vbId );
		scene.addParamID( p.m_id, ChunkPrimParam::VBUFFER_ID,"texcoord0",p.m_vbId );
		scene.addParamID( p.m_id, ChunkPrimParam::VBUFFER_ID,"color0", p.m_vbId );
		scene.addParamID( p.m_id, ChunkPrimParam::VBUFFER_NUMDATA,"texcoord0",  1 );
		scene.addParamID( p.m_id, ChunkPrimParam::VBUFFER_NUMDATA,"color0",  2 );

		// Add a shader parameter
		scene.addParamEnum(p.m_id, ChunkPrimParam::PARAMVSHADER, "ModelViewProj", ChunkPrimParam::ModelViewProjection);

		scene.addParamID(p.m_id, ChunkPrimParam::TEXTURE, "texture", fvrtx->getCgId() );

		// Link index buffer idIB to primitive id
		// we will get a number of triangles, and right now we do not unify them to have different texture coordinates
		// so pass Type::Null here to indicate that the vertices are drawn 'as-is'
		int indexBufferType = Type::Null;
		scene.addIndexBuffer(p.m_ibId, 0, indexBufferType, ChunkIndexBuffer::Triangle);

		scene.addParamID(p.m_id, ChunkPrimParam::IBUFFER_ID, "", p.m_ibId );

		scene.addParam(p.m_id, ChunkPrimParam::PARAMOPENGL, "Blend", true );
		scene.addParam(p.m_id, ChunkPrimParam::PARAMOPENGL, "DepthTest", false);
		scene.addParam(p.m_id, ChunkPrimParam::PARAMOPENGL, "DepthWrite", false );
		// draw the primitive above all others
//		scene.addParam(p.m_id, ChunkPrimParam::ORDER, "", std::numeric_limits<int>::max() ); // draw last

		scene.addParam( p.m_id, ChunkPrimParam::TRANSFORM_OVERRIDE, "", true );

		return p;
	}
}
