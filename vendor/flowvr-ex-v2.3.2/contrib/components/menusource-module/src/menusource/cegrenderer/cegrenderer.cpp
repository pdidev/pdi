/******************************************************************
 *                                                                 *
 *       File : cegrenderer.cpp                           *
 *                                                                 *
 *                                                                 *
 *       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
 *                                                                 *
 ******************************************************************/
#include "cegrenderer.h"
#include "cegtexture.h"
#include "cegtexturetarget.h"
#include "ceggeometrybuffer.h"
#include "cegrendertarget.h"

#include <algorithm>
#include <flowvr/render/chunkrenderwriter.h>
#include <flowvr/render/primitive.h>
#include <flowvr/render/chunk.h>
#include <flowvr/moduleapi.h>
#include <ftl/convert.h>

#include <Cg/cgGL.h>


#include <CEGUI/CEGUIRenderingRoot.h>


#include <map>
#include <list>
#include <deque>
#include <queue>

#include <iostream>
#include <limits>

using namespace flowvr;
using namespace flowvr::render;


namespace CEGUI
{

	FlowVRCegRenderer::FlowVRCegRenderer( flowvr::OutputPort *port,
										  unsigned int nIdPrefix )
	: Renderer()
	, m_port(port)
	, m_writer( new flowvr::render::ChunkRenderWriter )
	, m_nId(0)
	, m_nIdPrefix(nIdPrefix)
	, m_nIdVs(0)
	, m_nIdPs(0)
	, m_identifierString( "FlowVRCegRenderer" )
	, m_defaultRenderTarget( NULL )
	, m_renderingRoot( NULL )
	, m_dispSize(640,480)
	, m_dispDPI(96,96)
	, m_nFBOId(0)
	, m_currentTarget(NULL)
	{

		m_defaultRenderTarget = new FlowVRCegTextureTarget(m_nFBOId++, *this, "defaultRenderTarget" );
		m_renderingRoot = new RenderingRoot( *m_defaultRenderTarget );

		m_nIdVs = generateID();
		m_nIdPs = generateID();

		if( !m_writer->loadVertexShader( m_nIdVs, "shaders/texture_menu_v.cg" ) )
			m_nIdVs = 0;

		if( !m_writer->loadPixelShader ( m_nIdPs, "shaders/texture_menu_p.cg" ) )
			m_nIdPs = 0;
	}

	FlowVRCegRenderer::~FlowVRCegRenderer()
	{
		destroyAllTextures();
		destroyAllTextureTargets();
		destroyAllGeometryBuffers();

		if(m_nIdVs)
			m_writer->delVertexShader( m_nIdVs );
		if(m_nIdPs)
			m_writer->delPixelShader( m_nIdPs );

		delete m_renderingRoot;
		delete m_defaultRenderTarget;


		// send to flowvr-render
		flush();

		delete m_writer;
	}

	RenderTarget *FlowVRCegRenderer::getCurrentRenderTarget() const
	{
		return m_currentTarget;
	}

	void FlowVRCegRenderer::setCurrentRenderTarget( RenderTarget *tg )
	{
		m_currentTarget = tg;
	}

	void FlowVRCegRenderer::destroyTexture(Texture& texture)
	{
		FlowVRCegTexture &tex = dynamic_cast<FlowVRCegTexture&>(texture);
		m_textures.remove(&tex);

		std::cout << "deleted texture @ " << &tex << std::endl;
		delete &tex;
	}

	void FlowVRCegRenderer::destroyAllTextures(void)
	{
		while(!m_textures.empty())
			destroyTexture( *m_textures.front() );
	}

	TextureTarget* FlowVRCegRenderer::createTextureTarget()
	{
		static int n=0;

		m_targets.push_back( new FlowVRCegTextureTarget(m_nFBOId++, *this, "target-texture ["+ftl::toString<int>(n++)+"]" ) );
		return m_targets.back();
	}

	void FlowVRCegRenderer::destroyTextureTarget(TextureTarget* target)
	{
		m_targets.remove( static_cast<FlowVRCegTextureTarget*>(target) );
		delete target;
	}

	void FlowVRCegRenderer::destroyAllTextureTargets()
	{
		while( ! m_targets.empty() )
			destroyTextureTarget( (TextureTarget*)m_targets.front() );
	}

	GeometryBuffer& FlowVRCegRenderer::createGeometryBuffer()
	{
		m_geombuffers.push_back( new FlowVRCegGeometryBuffer( *this ) );
		std::cout << "FlowVRCegRenderer::createGeometryBuffer() -- [" << m_geombuffers.size() << "]" << std::endl;
		return *m_geombuffers.back();
	}

	void FlowVRCegRenderer::destroyGeometryBuffer(const GeometryBuffer& buffer)
	{
		m_geombuffers.remove( (FlowVRCegGeometryBuffer*)&buffer );
		delete &buffer;

		std::cout << "FlowVRCegRenderer::destroyGeometryBuffer() -- [" << m_geombuffers.size() << "]" << std::endl;
	}

	void FlowVRCegRenderer::destroyAllGeometryBuffers()
	{
		while( ! m_geombuffers.empty() )
			destroyGeometryBuffer( (const GeometryBuffer&) *m_geombuffers.front() );
	}

	void FlowVRCegRenderer::setDisplaySize(const Size& size)
	{
		m_dispSize = size;
		m_defaultRenderTarget->declareRenderSize(m_dispSize);
	}

	const Size& FlowVRCegRenderer::getDisplaySize() const
	{
		return m_dispSize;
	}

	const Vector2& FlowVRCegRenderer::getDisplayDPI() const
	{
		return m_dispDPI;
	}

	uint FlowVRCegRenderer::getMaxTextureSize() const
	{
		return 4096;
	}


	Texture& FlowVRCegRenderer::createTexture()
	{
		m_textures.push_back( new FlowVRCegTexture( *this, generateID(), "" ) );
		return *m_textures.back();
	}

	Texture& FlowVRCegRenderer::createTexture(const String& filename,
								   const String& resourceGroup)
	{
		FlowVRCegTexture *t = new FlowVRCegTexture( *this, generateID(), std::string(filename.c_str()) );
		if( t )
		{
			t->loadFromFile(filename, resourceGroup);
			m_textures.push_back(t);
			return *t;
		}

		// should not reach this point
		throw std::exception();
	}

	Texture& FlowVRCegRenderer::createTexture(const Size& size)
	{
		FlowVRCegTexture *t = new FlowVRCegTexture( *this, generateID(), "" );
		if( t )
		{
			m_textures.push_back( t );
			return *t;
		}

		// should not reach this point
		throw std::exception();
	}

	void FlowVRCegRenderer::beginRendering()
	{
//		std::cout << " ++++++++++##############++++++++ BEGIN RENDER" << std::endl;
		getDefaultRenderingRoot().getRenderTarget().activate();
	}


	void FlowVRCegRenderer::endRendering()
	{
		std::for_each( m_targets.begin(), m_targets.end(), std::mem_fun( &FlowVRCegTextureTarget::deactivate ) );
		(*m_defaultRenderTarget).deactivate();

		flush();
		m_currentTarget = NULL;

//		std::cout << " ++++++++++##############++++++++ END RENDER" << std::endl;
	}

	RenderingRoot& FlowVRCegRenderer::getDefaultRenderingRoot()
	{
		return *m_renderingRoot;
	}

	const String& FlowVRCegRenderer::getIdentifierString() const
	{
		return m_identifierString;
	}

	void FlowVRCegRenderer::flush()
	{
		if(m_writer->isDirty())
			m_writer->put( m_port );
	}

	flowvr::ID FlowVRCegRenderer::generateID()
	{
		return m_port->getModule()->generateID();
	}

	flowvr::render::ChunkRenderWriter &FlowVRCegRenderer::getChunkWriter() const
	{
		return *m_writer;
	}
}
