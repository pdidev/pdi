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
* File: cegtexturetarget.cpp                                      *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/

#include "cegtexturetarget.h"
#include "cegtexture.h"
#include "cegrenderer.h"
#include "ceggeometrybuffer.h"

#include <CEGUI/CEGUIRenderQueue.h>
#include <CEGUI/CEGUIGeometryBuffer.h>
#include <CEGUI/CEGUIVertex.h>

#include <flowvr/render/chunkrenderwriter.h>
#include <ftl/convert.h>

#include <limits>
#include <fstream>

using namespace flowvr::render;
using namespace ftl;



namespace
{
	struct LcVertex
	{
		Vec3f pos;
		Vec2f uv;
		Vec4f col;
	};


	void dumpAC3D( const std::string &strFileName, ChunkVertexBuffer *vb, const std::string &txName )
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

		LcVertex *vdata = (LcVertex*)vb->data();

		for( size_t n=0; n < numvert; ++n )
		{
			out << vdata[n].pos.x() << " " <<
					vdata[n].pos.y() << " " <<
					vdata[n].pos.z() << std::endl;
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
}


namespace CEGUI
{
	FlowVRCegTextureTarget::FlowVRCegTextureTarget( unsigned int fboId, FlowVRCegRenderer &parent, const std::string &strName )
	: TextureTarget()
	, m_size(0,0)
	, m_area(0,0,0,0)
	, m_nFBOId(fboId)
	, m_nCamID( parent.generateID() )
	, m_tx(new FlowVRCegTexture( parent, parent.generateID(), "FBO["+strName+"]" ))
	, m_parent(parent)
	, m_txId( parent.generateID() )
	, m_strName(strName)
	{
		Size dispSize = m_parent.getDisplaySize();

		m_area = Rect( 0, 0, dispSize.d_width, dispSize.d_height );

		std::cout << "************ Creating FBO[" << m_nFBOId << " ; tg = " << 1 << TargetGroups::FBO2Tg(m_nFBOId) << "]" << std::endl;
		ChunkRenderWriter &scene = parent.getChunkWriter();
		std::cout << "************ [" << m_strName << "] AREA: w=" << m_area.d_right-m_area.d_left<< " ; h=" << m_area.d_bottom - m_area.d_top << std::endl;


		Mat4x4f m;
		m.clear();

		float left    =   m_area.d_left;
		float right   =   m_area.d_right;
		float top     =   m_area.d_top;
		float bottom  =   m_area.d_bottom;
		float zNear   =   0;
		float zFar    =   1;


		m = ftl::matrixOrthoProjection(left, right, bottom, top, zNear, zFar);


		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		// THE GEOMETRY (QUAD) FOR THE UI TO BE DRAWN ON
		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

		// create quad geometry and target groups
		// Get IDs for the primitive and its resources
		m_MPrim   = m_parent.generateID(); // Id of the primitive
		m_MVbId   = m_parent.generateID(); // Id of the Vertex Buffer
		m_mIbId   = m_parent.generateID(); // Id of the Index Buffer

		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		// ADD AN FBO
		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		scene.addTexture(m_txId,
						 ChunkTexture::RGBA,
						 ftl::Type::vector(ftl::Type::Byte, 4),
						 m_area.d_right-m_area.d_left, m_area.d_bottom-m_area.d_top , 0,
						 TargetGroups::FBO2txTp(m_nFBOId) ); // create a dummy texture for the FBO


		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		// CAM / REF-FRAME FOR THE UI
		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
		Vec4f col(0,0,0,0);

		scene.addPrimitive(m_nCamID, ("FlowVRCegTextureTarget -- CAM["+ftl::toString<unsigned int>(m_nFBOId)+"]"),
				           0,
				           TP_CAMERA,
				           (unsigned int)ChunkPrimParam::TG( TargetGroups::FBO2Tg(m_nFBOId) ) );

		scene.addParam(m_nCamID, ChunkPrimParam::PARAMOPENGL, "clear", &col, Type::Vec4f );
		scene.addParam(m_nCamID, ChunkPrimParam::PROJECTION, "", m ); // attach the same orthogonal projection as above

		// for the FBO cam!
		Mat4x4f mI;
		mI.identity();
		scene.addParam(m_nCamID, ChunkPrimParam::TRANSFORM, "", mI );

//		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//		// ADD THE QUAD FOR THIS RENDERTARGET TO THE NORMAL SCENE
//		// ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//		// Create vdata+index buffers for a quad
//		// We use  two vdata buffers (position and uv texture position). This array provides the number of vdata buffers and their types
//		int vdataBufferTypes[3] = {Type::Vec3f, Type::Vec2f, Type::Vec4f };
//		// Create the vdata buffers (4 points and 2 buffers)
//		ChunkVertexBuffer* vb = scene.addVertexBuffer(m_MVbId, 4, 3, vdataBufferTypes);
//		ChunkIndexBuffer* ib  = scene.addIndexBuffer(m_mIbId, 4, Type::Byte, ChunkIndexBuffer::Quad);
//
//		// A structure defining the type of vertex we use with position and uv texture position
//		struct Vertex
//		{
//			Vec3f pos;
//			Vec2f uv;
//			Vec4f col;
//		};
//
//		// Fill vdata and index buffers
//		Vertex* vertex = (Vertex*)vb->data();
//		// create unit quad, tex-cords invert the image on the quad
//		vertex[0].pos = Vec3f(  0,  0, 0 ); vertex[0].uv = Vec2f( 0, 1 ); vertex[0].col = Vec4f( 1,1,1,0.60 );
//		vertex[1].pos = Vec3f(  0,  1, 0 ); vertex[1].uv = Vec2f( 0, 0 ); vertex[1].col = Vec4f( 1,1,1,0.60 );
//		vertex[2].pos = Vec3f(  1,  1, 0 ); vertex[2].uv = Vec2f( 1, 0 ); vertex[2].col = Vec4f( 1,1,1,0.60 );
//		vertex[3].pos = Vec3f(  1,  0, 0 ); vertex[3].uv = Vec2f( 1, 1 ); vertex[3].col = Vec4f( 1,1,1,0.60 );
//
//		// Index buffer. We set values of the 12 triangles
//		Vec<4,char>* idata = (Vec<4,char>*) ib->data();
//		idata[0] = Vec<4,char>(0, 1, 2, 3);
//
//		// Create new primitive
//		scene.addPrimitive(m_MPrim, "CEGUI SURFACE QUAD -- tx["+ftl::toString<unsigned int>(m_nFBOId)+"]");
//
//		// Link the shaders to the primitive m_MPrim
//		scene.addParamID(m_MPrim, ChunkPrimParam::VSHADER,"", m_parent.m_nIdVs );
//		scene.addParamID(m_MPrim, ChunkPrimParam::PSHADER,"", m_parent.m_nIdPs );
//
//		// Link vdata buffer idVB to primitive m_MPrim
//		// Position, UV Texture position are given by vertex data buffer idVB
//		scene.addParamID(m_MPrim, ChunkPrimParam::VBUFFER_ID,"position", m_MVbId);
//		scene.addParamID(m_MPrim, ChunkPrimParam::VBUFFER_ID,"texcoord0",m_MVbId);
//		scene.addParamID(m_MPrim, ChunkPrimParam::VBUFFER_ID,"color0",m_MVbId);
//		// Position is 1rst value for each point. Its offset is 0 (default case)
//		// UV Texture is the 2nd value for each point. Its offset is 1
//		scene.addParamID(m_MPrim, ChunkPrimParam::VBUFFER_NUMDATA,"texcoord0",1);
//		scene.addParamID(m_MPrim, ChunkPrimParam::VBUFFER_NUMDATA,"color0",2);
//
//		// Link index buffer idIB to primitive m_MPrim
//		scene.addParamID(m_MPrim, ChunkPrimParam::IBUFFER_ID, "", m_mIbId);
//
//		// Add a shader parameter
//		scene.addParamEnum(m_MPrim, ChunkPrimParam::PARAMVSHADER, "ModelViewProj", ChunkPrimParam::ModelViewProjection);
//
//		// Link the texture to the primitive m_MPrim -> USE THE FBO OUTPUT AS TEXTURE TO THIS QUAD
//		scene.addParamID(m_MPrim, ChunkPrimParam::TEXTURE, "texture", m_txId);
//
//
//		// scale up to the display size
//		Mat4x4f p = matrixScale( Vec3f(m_area.d_right-m_area.d_left,m_area.d_bottom-m_area.d_top,1) );
//
//		// add this to the quad (so it fills the whole screen (see projetion set-up down below))
//
//		scene.addParam(m_MPrim, ChunkPrimParam::TRANSFORM, "", p );
//
//		// make sure this transformation is set and not applied
//		scene.addParam(m_MPrim, ChunkPrimParam::TRANSFORM_OVERRIDE,"", true);
//
//		// make all elements appear in pixel space on this quad
//		scene.addParam(m_MPrim, ChunkPrimParam::PROJECTION, "", m );
//
//		// draw the quad above all others
//		scene.addParam(m_MPrim, ChunkPrimParam::ORDER, "", std::numeric_limits<int>::max() ); // draw last
//
//		// set-up GL mode for this quad
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "Blend", true );
//		scene.addParamEnum(m_MPrim, ChunkPrimParam::PARAMOPENGL, "BlendMode", BLEND_STD);
//
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "DepthTest", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "DepthWrite", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "AlphaTest", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "CullFace", 0 );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "Fog", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "Lighting", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "TexGenS", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "TexGenT", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "TexGenR", false );
//		scene.addParam(m_MPrim, ChunkPrimParam::PARAMOPENGL, "Scissor", Vec4f(0,1,2,3) ); // test
//
//		scene.addParam(m_MPrim, ChunkPrimParam::VISIBLE, "", true );
	}

	FlowVRCegTextureTarget::~FlowVRCegTextureTarget()
	{
		ChunkRenderWriter &scene = m_parent.getChunkWriter();
		for( TX2GB::iterator it = m_txMap.begin(); it != m_txMap.end(); ++it )
		{
			scene.delPrimitive( (*it).second.m_id );
			scene.delVertexBuffer( (*it).second.m_vbId );
			scene.delIndexBuffer((*it).second.m_ibId );

			delete (*it).second.m_buffers;
		}

		delete m_tx;

		scene.delPrimitive( m_nCamID );
//		scene.delPrimitive( this->m_MPrim );
		scene.delIndexBuffer( this->m_mIbId );
		scene.delVertexBuffer( this->m_MVbId );
		scene.delTexture( this->m_txId );
	}


	///////////////////////////////////////////////////////////////////////
	// FROM RENDERBUFFER
	///////////////////////////////////////////////////////////////////////

	void FlowVRCegTextureTarget::draw(const GeometryBuffer& buffer)
	{
		buffer.draw();
	}

    void FlowVRCegTextureTarget::draw(const RenderQueue& queue)
    {
    	m_parent.setCurrentRenderTarget( this );
    	queue.draw();
    	m_parent.setCurrentRenderTarget( NULL );
    }

	void FlowVRCegTextureTarget::setArea(const Rect& area)
	{
		m_area = area;
//		updateTexture();
	}

    const Rect& FlowVRCegTextureTarget::getArea() const
    {
    	return m_area;
    }

	bool FlowVRCegTextureTarget::isImageryCache() const
	{
		return true;
	}

	void FlowVRCegTextureTarget::activate()
	{
	}

	void FlowVRCegTextureTarget::deactivate()
	{
	}

	void FlowVRCegTextureTarget::unprojectPoint(const GeometryBuffer& buff,
						const Vector2& p_in, Vector2& p_out) const
	{
		p_out = p_in;
	}



	unsigned int FlowVRCegTextureTarget::getFBOID() const
	{
		return m_nFBOId;
	}


	/////////////////////////////////////////////////////////////
	// FROM TEXTURETARGET
	/////////////////////////////////////////////////////////////


	void FlowVRCegTextureTarget::clear()
	{

	}


	Texture& FlowVRCegTextureTarget::getTexture() const
	{
		return *m_tx;
	}

	void FlowVRCegTextureTarget::declareRenderSize(const Size& sz)
	{
		m_size = sz;
		setArea( Rect( m_area.getPosition(), m_size ) );
		updateTexture();
	}

	bool FlowVRCegTextureTarget::isRenderingInverted() const
	{
		return true;
	}


	void FlowVRCegTextureTarget::updateTexture()
	{
		std::cout << "FlowVRCegTextureTarget["+m_strName+"]::updateTexture() -- updating texture target to ("
				  << m_size.d_width
				  << " ; "
				  << m_size.d_height
				  << ") -- area: "
				  << m_area.d_left
				  << " ; "
				  << m_area.d_right
				  << " ; "
				  << m_area.d_top
				  << " ; "
				  << m_area.d_bottom
				  << std::endl;

		ChunkRenderWriter &w = m_parent.getChunkWriter();

		Size sz = m_size; //m_parent.getDisplaySize();

		// always scale texture to match display size of renderer (should be window size)
		w.addTexture( m_txId, ChunkTexture::RGBA,
		              ftl::Type::vector(ftl::Type::Byte, 4),
		              sz.d_width,
		              sz.d_height,
		              0,
		              TargetGroups::FBO2txTp(m_nFBOId) ); // create FBO for TG(TGId),
													 // associate with texture TxId
		m_tx->setSize( sz ); // forward to texture (for scaling)


		// update the projection on the texture

		Mat4x4f m;
		m.clear();

		float left    =   0;
		float right   =   float(sz.d_width); ///2.0f;
		float top     =   0;
		float bottom  =   float(sz.d_height); ///2.0f;
		float zNear   =   0;
		float zFar    =   1;


		m = ftl::matrixOrthoProjection(left, right, bottom, top, zNear, zFar);
//		w.addParam(m_MPrim, ChunkPrimParam::PROJECTION, "", m );
		w.addParam(m_nCamID, ChunkPrimParam::PROJECTION, "", m ); // attach the same orthogonal projection as above


//		m = matrixScale( Vec3f( sz.d_width, sz.d_height, 1 ) );
//		w.addParam(m_MPrim, ChunkPrimParam::TRANSFORM, "", m );
	}
}
