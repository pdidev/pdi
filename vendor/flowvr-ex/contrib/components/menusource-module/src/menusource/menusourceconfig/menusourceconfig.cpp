/******************************************************************
*                                                                 *
*       File : menusourceconfig.cpp                           *
*                                                                 *
*                                                                 *
*       Contact : Ingo Assenmacher (ingo.assenmacher@imag.fr)     *
*                                                                 *
******************************************************************/

#include "menusourceconfig.h"

#include <CEGUI/CEGUI.h>
#include <CEGUI/CEGUISystem.h>
#include <CEGUI/CEGUIDefaultResourceProvider.h>
#include <CEGUI/CEGUIFont.h>
#include <CEGUI/CEGUIWindowManager.h>
#include <CEGUI/CEGUIWindow.h>
#include <CEGUI/CEGUIImageset.h>
#include <CEGUI/CEGUIScheme.h>
#include <CEGUI/CEGUIScriptModule.h>

#include <flowvr/xml.h>

#include <algorithm>

namespace
{
template<class T>
 void setDefaultGroup( const flowvr::xml::TiXmlHandle &nd,
		                                const std::string &strName,
		                                const std::string &def )
 {
    flowvr::xml::TiXmlHandle is = nd.FirstChild(strName);
    if(is.Element())
    {
    	const char *pcUses = is.Element()->Attribute("uses");
    	if(pcUses)
    	{
    		T::setDefaultResourceGroup(pcUses);
    		CEGUI::Logger::getSingleton().logEvent(
						"Setting default group of ["
    		           + strName
    		           + "] to user value: ["
    		           + pcUses
    		           + "]", CEGUI::Informative);
    	}
    	else
    	{
    		T::setDefaultResourceGroup(def);
    		CEGUI::Logger::getSingleton().logEvent("Setting default group of ["
    		          + strName
    		          + "] to default value: ["
    		          + def
    		          + "]", CEGUI::Informative);
    	}
    }
    else
    {
		CEGUI::Logger::getSingleton().logEvent("Setting of default group for ["
		          + strName
		          + "] failed. Node points to no element.", CEGUI::Informative);
    }
 }

 void _freeFont( CEGUI::Font *font )
 {
	CEGUI::FontManager::getSingleton().destroy( *font );
 }

 void _freeImageset( CEGUI::Imageset *imageset )
 {
	CEGUI::ImagesetManager::getSingleton().destroy( *imageset );
 }

}

MenuSourceConfig::MenuSourceConfig( CEGUI::System *pSys,
			          const std::string &strConfigFileName )
: m_system(pSys)
{
	flowvr::xml::TiXmlDocument doc;
	if( doc.LoadFile( strConfigFileName ) )
	{
		flowvr::xml::TiXmlHandle root = doc.FirstChild( "config" );
		flowvr::xml::TiXmlHandle rsc  = root.FirstChild( "resources" );
		flowvr::xml::TiXmlHandle is   = root.FirstChild( "imagesets" );
		flowvr::xml::TiXmlHandle ft   = root.FirstChild( "fonts" );
		flowvr::xml::TiXmlHandle lf   = root.FirstChild( "looknfeels" );
		flowvr::xml::TiXmlHandle sm   = root.FirstChild( "schemes" );

		SetupResourceGroups( rsc );
		SetupImageSets( is );
		SetupFonts( ft );
		SetupLookNFeels( lf );
		SetupSchemes( sm );
	}
	else
	{
		std::cerr << "failed to open config file for CEGUI: ["
		          << strConfigFileName
		          << "]"
		          << std::endl;
	}
}

MenuSourceConfig::~MenuSourceConfig()
{
	std::for_each( m_vFonts.begin(), m_vFonts.end(), _freeFont );
}

bool MenuSourceConfig::SetupResourceGroups(const flowvr::xml::TiXmlHandle &rgRoot )
{
    CEGUI::DefaultResourceProvider* rp = static_cast<CEGUI::DefaultResourceProvider*>
        (m_system->getResourceProvider());

    if(!rp)
    	return false;

    flowvr::xml::TiXmlHandle dp = rgRoot.FirstChild("datapath");
    if( !dp.Element() )
    	return false;

    const char *dataPathPrefix = dp.Element()->Attribute("value");
    if(!dataPathPrefix)
    	return false;

    flowvr::xml::TiXmlHandle rg  = rgRoot.FirstChild("resourcegroups");
    flowvr::xml::TiXmlHandle rgs = rg.FirstChild( "resourcegroup" );

    flowvr::xml::TiXmlElement *child;
    for(child = rgs.Element(); child; child = child->NextSiblingElement("resourcegroup") )
    {
    	const char *pcName  = child->Attribute("name");
    	const char *pcValue = child->Attribute("value");

    	if(!pcName || !pcValue)
    	{
    		char buffer[256];
    		sprintf(buffer, "%d", child->Row());

    		CEGUI::Logger::getSingleton().logEvent(
    				"resource group @ "
    				+ std::string(buffer)
    				+ "does not contain 'name' or 'value' attribute. skipping.",
    				CEGUI::Errors);
    		continue;
    	}

		std::string _uses(pcValue);
		if( *(_uses.end()-1) != '/' )
			_uses +=  "/";

    	std::string resourcePath = std::string(dataPathPrefix)
    	                         + "/"
    	                         + _uses;
    	rp->setResourceGroupDirectory(pcName, resourcePath);
    	CEGUI::Logger::getSingleton().logEvent(
    			"setting resource group ["
    			+ std::string(pcName)
    			+ "] to path ["
    			+ resourcePath
    			+ "]", CEGUI::Informative );
    }

    // ok, create defaults for different known groups

    flowvr::xml::TiXmlHandle df = rgRoot.FirstChild("defaults");

//    setDefaultGroup<CEGUI::Imageset>( df, "Imageset", "imagesets" );
//    setDefaultGroup<CEGUI::Font>(df, "Font", "fonts" );
//    setDefaultGroup<CEGUI::Scheme>(df, "Scheme", "schemes");
//    setDefaultGroup<CEGUI::WindowManager>(df, "WindowManager", "layouts");
//    setDefaultGroup<CEGUI::ScriptModule>(df, "ScriptModule", "lua_scripts");
//    setDefaultGroup<CEGUI::WidgetLookManager>(df,"WidgetLookManager", "looknfeels");

	return true;
}

bool MenuSourceConfig::SetupImageSets( const flowvr::xml::TiXmlHandle &isRoot )
{
	flowvr::xml::TiXmlHandle els = isRoot.FirstChild("imageset");

	flowvr::xml::TiXmlElement *child;
	for(child = els.Element(); child; child = child->NextSiblingElement("imageset") )
	{
		const char *pcName = child->Attribute("name");
		if(!pcName)
			continue;

		CEGUI::Logger::getSingleton().logEvent(
				"creating imageset ["
				+ std::string(pcName)
				+ "]", CEGUI::Informative);

		try
		{
			m_vImageSets.push_back(&CEGUI::ImagesetManager::getSingleton().create(pcName));
			CEGUI::Logger::getSingleton().logEvent(
					"imageset creation fpr [" + std::string(pcName) + "] worked.", CEGUI::Informative);

		}
		catch( CEGUI::Exception &e )
		{
			CEGUI::Logger::getSingleton().logEvent(
					"imageset creation for ["+std::string(pcName)+"] failed.", CEGUI::Errors);

			CEGUI::Logger::getSingleton().logEvent(e.what(), CEGUI::Informative);

		}
	}
	return true;
}

bool MenuSourceConfig::SetupFonts( const flowvr::xml::TiXmlHandle &font )
{
	flowvr::xml::TiXmlHandle els = font.FirstChild("font");
	flowvr::xml::TiXmlElement *child;
	for(child = els.Element(); child; child = child->NextSiblingElement("font") )
	{
		const char *pcName = child->Attribute("name");
		if(!pcName)
			continue;
		CEGUI::Logger::getSingleton().logEvent(
				"creating font ["
				+ std::string(pcName)
				+ "]", CEGUI::Informative);

		try
		{
			m_vFonts.push_back(& (CEGUI::FontManager::getSingleton().create(pcName)) );
			CEGUI::Logger::getSingleton().logEvent(
					"font creation for " + std::string(pcName) + " worked.", CEGUI::Informative);
		}
		catch( CEGUI::Exception &e )
		{
			CEGUI::Logger::getSingleton().logEvent(
					"font creation for " + std::string(pcName) + " failed.", CEGUI::Errors);
			CEGUI::Logger::getSingleton().logEvent( e.what(), CEGUI::Informative );
		}

	}
	return true;
}

bool MenuSourceConfig::SetupLookNFeels( const flowvr::xml::TiXmlHandle &lfRoot )
{
	flowvr::xml::TiXmlHandle els = lfRoot.FirstChild("looknfeel");

	flowvr::xml::TiXmlElement *child;
	for(child = els.Element(); child; child = child->NextSiblingElement("looknfeel") )
	{
		const char *pcName = child->Attribute("name");
		if(!pcName)
			continue;

		CEGUI::Logger::getSingleton().logEvent(
				"creating looknfeel specification ["
				+ std::string(pcName)
				+ "]", CEGUI::Informative);
	    CEGUI::WidgetLookManager::getSingleton().parseLookNFeelSpecification(pcName);
	}
	return true;
}

bool MenuSourceConfig::SetupSchemes( const flowvr::xml::TiXmlHandle &smRoot )
{
	flowvr::xml::TiXmlHandle els = smRoot.FirstChild("scheme");

	flowvr::xml::TiXmlElement *child;
	for(child = els.Element(); child; child = child->NextSiblingElement("scheme") )
	{
		const char *pcName = child->Attribute("name");
		if(!pcName)
			continue;

		CEGUI::Logger::getSingleton().logEvent(
				"loading scheme ["
				+ std::string(pcName)
				+ "]", CEGUI::Informative);
		CEGUI::SchemeManager::getSingleton().create(pcName);
	}
	return true;
}
