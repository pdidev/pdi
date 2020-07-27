/******************************************************************
*                                                                 *
*       File : cegrenderer.h                                       *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#ifndef CEGRENDERER_H_
#define CEGRENDERER_H_

#include "CEGUI/CEGUIBase.h"
#include "CEGUI/CEGUIRenderer.h"
#include "CEGUI/CEGUITexture.h"
#include "CEGUI/CEGUISize.h"
#include "CEGUI/CEGUIRect.h"
#include "CEGUI/CEGUIColourRect.h"

#include <set>
#include <list>

#include <flowvr/id.h>

namespace flowvr
{
	class OutputPort;
	namespace render
	{
		class ChunkRenderWriter;
		class ChunkVertexBuffer;
		class ChunkIndexBuffer;
	}
}

namespace CEGUI
{

	class FlowVRCegTexture;
	class FlowVRCegTextureTarget;
	class FlowVRCegGeometryBuffer;
	class FlowVRCegRenderTarget;

	class FlowVRCegRenderer : public Renderer
	{
	public:
		FlowVRCegRenderer( flowvr::OutputPort *pOut,
				           unsigned int nIdPrefix );
		~FlowVRCegRenderer();

		virtual RenderingRoot& getDefaultRenderingRoot();
		virtual GeometryBuffer& createGeometryBuffer();
		virtual void destroyGeometryBuffer(const GeometryBuffer& buffer);
		virtual void destroyAllGeometryBuffers();

		virtual TextureTarget* createTextureTarget();
		virtual void destroyTextureTarget(TextureTarget* target);
		virtual void destroyAllTextureTargets();

		virtual Texture& createTexture();
		virtual Texture& createTexture(const String& filename,
									   const String& resourceGroup);
		virtual Texture& createTexture(const Size& size);

		virtual void destroyTexture(Texture& texture);
		virtual void destroyAllTextures();

		virtual void beginRendering();
		virtual void endRendering();

		virtual void setDisplaySize(const Size& size);
		virtual const Size& getDisplaySize() const;
		virtual const Vector2& getDisplayDPI() const;
		virtual uint getMaxTextureSize() const;


		virtual const String& getIdentifierString() const;



		// ++++++++++++++++++++++++++++++++++++++++++++++
		// FLOWVR-API
		// ++++++++++++++++++++++++++++++++++++++++++++++
		void flush();

		flowvr::render::ChunkRenderWriter &getChunkWriter() const;
		flowvr::ID generateID();

		RenderTarget *getCurrentRenderTarget() const;
		void setCurrentRenderTarget( RenderTarget * );
	public:

		// #############################################
		// CEGUI ELEMENTS
		// #############################################
		String m_identifierString;
		FlowVRCegTextureTarget *m_defaultRenderTarget;
		RenderingRoot          *m_renderingRoot;

		// #############################################
		// TEXTURES AND FLOWVR-RENDER STATE
		// #############################################
		typedef std::list<FlowVRCegTexture*> TEXTURES;
		TEXTURES m_textures;

		typedef std::list<FlowVRCegTextureTarget*> TTARGETS;
		TTARGETS m_targets;

		typedef std::list<FlowVRCegGeometryBuffer*> GBUFFERS;
		GBUFFERS m_geombuffers;

		flowvr::OutputPort                *m_port;
		flowvr::render::ChunkRenderWriter *m_writer;

		unsigned int m_nId;       /**< id counter */
		unsigned int m_nIdPrefix; /**< daemon prefix */
		unsigned int m_nFBOId;


		unsigned int m_nIdVs,  /**< vertex shader id used for quads */
		             m_nIdPs;  /**< pixel  shader id used for quads */

		Size    m_dispSize;
		Vector2 m_dispDPI;

		RenderTarget *m_currentTarget;
	};
}

#endif /* CEGRENDERER_H_ */
