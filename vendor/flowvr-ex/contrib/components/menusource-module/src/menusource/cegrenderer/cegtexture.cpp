/******************************************************************
*                                                                 *
*       File : cegtexture.cpp                           *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#include "cegtexture.h"

#include <flowvr/render/chunkrenderwriter.h>
#include <flowvr/utils/backtrace.h>
#include <ftl/type.h>
#include <ftl/convert.h>

#include <CEGUI/CEGUIDefaultResourceProvider.h>
#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUIImageCodec.h>

#include "cegrenderer.h"

#include <unistd.h>

namespace CEGUI
{
	FlowVRCegTexture::FlowVRCegTexture(Renderer &owner, int nId, const std::string &name )
	: Texture()
	, m_size(0,0)
	, m_originalSize(0,0)
	, m_texelScaling(1,1)
	, m_owner(owner)
	, m_nTxId(nId)
	, m_strFileName(name)
	{

	}

	FlowVRCegTexture::~FlowVRCegTexture()
	{
		unload();
	}

	/*************************************************************************
		Abstract Interface
	*************************************************************************/


	void	FlowVRCegTexture::loadFromFile(const String& filename, const String& resourceGroup)
	{
		std::cout << "FlowVRCegTexture::loadFromFile("
		          << filename
		          << ") -- ["
		          << resourceGroup
		          << "]"
		          << std::endl;

	    RawDataContainer texFile;
	    m_strFileName = filename.c_str();

	    System::getSingleton().getResourceProvider()->loadRawDataContainer(filename, texFile, resourceGroup);
	    System::getSingleton().getImageCodec().load(texFile, this); // will call 'load from buffer'
	    System::getSingleton().getResourceProvider()->unloadRawDataContainer(texFile);
	}

	void	FlowVRCegTexture::loadFromMemory(const void* buffPtr,
								   const Size &buffer_size,
								   PixelFormat pixelFormat)
	{
		std::cout << "FlowVRCegTexture::loadFromMemory()"
		          << std::endl;

		int imageType;
		int pixelType;

		switch(pixelFormat)
		{
			case  Texture::PF_RGB:
			{
				std::cout << "RGB texture requested.\n";
				imageType = flowvr::render::ChunkTexture::RGB;
				pixelType = ftl::Type::vector(ftl::Type::Byte, 3);
				break;
			}
			case Texture::PF_RGBA:
			{
				std::cout << "RGBA texture requested.\n";
				imageType = flowvr::render::ChunkTexture::RGBA;
				pixelType = ftl::Type::vector(ftl::Type::Byte, 4);
				break;
			}
			default:
				return; // failed
		}

		FlowVRCegRenderer *parent = dynamic_cast<FlowVRCegRenderer*>( &m_owner );
		flowvr::render::ChunkTexture *textureBuffer
			= parent->m_writer->addTexture(m_nTxId, imageType, pixelType, buffer_size.d_width, buffer_size.d_height );

		memcpy(textureBuffer->data(),buffPtr,textureBuffer->dataSize());
		m_originalSize = m_size = Size(  textureBuffer->nx, textureBuffer->ny );


		static int n = 0;
		std::string fname = ftl::toString<int>(n++)+ ".png";
		parent->getChunkWriter().saveTexture( textureBuffer, fname );
		m_strFileName = fname;

		updateCachedScaleValues();
	}


	std::string FlowVRCegTexture::getFileName() const
	{
		return m_strFileName;
	}

	int FlowVRCegTexture::getCgId() const
	{
		return m_nTxId;
	}

	void FlowVRCegTexture::unload()
	{
		if( m_nTxId )
		{
			FlowVRCegRenderer *parent = static_cast<FlowVRCegRenderer*>( &m_owner );
			parent->m_writer->delTexture( m_nTxId );
		}
	}

    const Size& FlowVRCegTexture::getSize() const
    {
    	return m_size;
    }

    void FlowVRCegTexture::setSize( const Size &sz )
    {
    	m_size = sz;
    	updateCachedScaleValues();
    }

    const Size& FlowVRCegTexture::getOriginalDataSize() const
    {
    	return m_originalSize;
    }

    const Vector2& FlowVRCegTexture::getTexelScaling() const
    {
    	return m_texelScaling;
    }

    void FlowVRCegTexture::saveToMemory(void* buffer)
    {

    	std::cout << " &&&&&&&&&&&& --- FlowVRCegTexture::saveToMemory(void* buffer) called" << std::endl;
    }

    void FlowVRCegTexture::updateCachedScaleValues()
    {
        //
        // calculate what to use for x scale
        //
        const float orgW = m_originalSize.d_width;
        const float texW = m_size.d_width;

        // if texture and original data width are the same, scale is based
        // on the original size.
        // if texture is wider (and source data was not stretched), scale
        // is based on the size of the resulting texture.
        m_texelScaling.d_x = 1.0f / ((orgW == texW) ? orgW : texW);

        //
        // calculate what to use for y scale
        //
        const float orgH = m_originalSize.d_height;
        const float texH = m_size.d_height;

        // if texture and original data height are the same, scale is based
        // on the original size.
        // if texture is taller (and source data was not stretched), scale
        // is based on the size of the resulting texture.
        m_texelScaling.d_y = 1.0f / ((orgH == texH) ? orgH : texH);

        std::cout << "new texel scaling for [" << m_strFileName << "]: (" << m_texelScaling.d_x << " : " << m_texelScaling.d_y << ")" << std::endl;
    }
}
