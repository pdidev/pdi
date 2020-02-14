/******************************************************************
*                                                                 *
*       File : menusourceconfig.h                                       *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#ifndef MENUSOURCECONFIG_H_
#define MENUSOURCECONFIG_H_

#include <string>
#include <vector>

namespace CEGUI
{
	class System;
	class Font;
	class Imageset;
}

namespace flowvr
{
	namespace xml
	{
		class TiXmlHandle;
	}
}

class MenuSourceConfig
{
public:
	MenuSourceConfig( CEGUI::System *pSys,
			          const std::string &strConfigFileName );

	~MenuSourceConfig();
private:
	bool SetupResourceGroups(const flowvr::xml::TiXmlHandle &);
	bool SetupImageSets( const flowvr::xml::TiXmlHandle &);
	bool SetupFonts( const flowvr::xml::TiXmlHandle & );
	bool SetupLookNFeels( const flowvr::xml::TiXmlHandle & );
	bool SetupSchemes( const flowvr::xml::TiXmlHandle & );

	CEGUI::System *m_system;
	typedef std::vector<CEGUI::Font*> FONTS;
	typedef std::vector<CEGUI::Imageset*> IMGS;
	FONTS m_vFonts;
	IMGS  m_vImageSets;
};

#endif /* MENUSOURCECONFIG_H_ */
