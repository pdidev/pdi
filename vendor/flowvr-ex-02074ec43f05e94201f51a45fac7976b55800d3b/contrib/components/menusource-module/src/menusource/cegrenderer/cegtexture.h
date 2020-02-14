/******************************************************************
*                                                                 *
*       File : cegtexture.h                                       *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#ifndef CEGTEXTURE_H_
#define CEGTEXTURE_H_

#include <CEGUI/CEGUITexture.h>
#include <CEGUI/CEGUISize.h>
#include <CEGUI/CEGUIVector.h>


namespace CEGUI
{
	class Renderer;


	class FlowVRCegTexture : public Texture
	{
	public:
		FlowVRCegTexture(Renderer &owner, int nId, const std::string &strName );
		~FlowVRCegTexture();


	    virtual const Size& getSize() const;
	    void setSize( const Size & );

	    virtual const Size& getOriginalDataSize() const;
	    virtual const Vector2& getTexelScaling() const;
	    virtual void loadFromFile(const String& filename,
	                              const String& resourceGroup);
	    virtual void loadFromMemory(const void* buffer,
	                                const Size& buffer_size,
	                                PixelFormat pixel_format);
	    virtual void saveToMemory(void* buffer);


	    // ++++++++++++++++++++++++++++++++++++++
	    // FLOWVR API
	    // ++++++++++++++++++++++++++++++++++++++
		int getCgId() const;
		void unload();
		std::string getFileName() const;
	private:
		void updateCachedScaleValues();

	    Size m_size,
	         m_originalSize;
	    Vector2 m_texelScaling;


	    Renderer &m_owner;
	    // ++++++++++++++++++++++++++++++++++++++
	    // FLOWVR RESSOURCES
	    // ++++++++++++++++++++++++++++++++++++++
		int m_nTxId;
		std::string m_strFileName;
	};
}

#endif /* CEGTEXTURE_H_ */
