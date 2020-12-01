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
*******************************************************************
*                                                                 *
*  Contact :                                                      *
*                                                                 *
******************************************************************/

#ifndef MENUSERVICE_H_
#define MENUSERVICE_H_

#include <flowvr/portutils/portservicelayer.h>
#include <flowvr/utils/timing.h>

namespace CEGUI
{
	class System;
	class FlowVRCegRenderer;
	class LuaScriptModule;
	class Window;
}


class MenuSourceConfig;

class menuservice : public flowvr::portutils::IServiceLayer
{
	public:
		menuservice( flowvr::portutils::Module & );
		~menuservice();

		virtual bool setParameters( const flowvr::portutils::ARGS & );

		int getMappedKey(int c, int mod, bool special) const;

		bool getIsValid() const;

		virtual bool Once();
		virtual bool PostLoop();

		void signalUpdate();

	public:

		CEGUI::System                   *m_pSystem;
		CEGUI::FlowVRCegRenderer        *m_pRenderer;
		CEGUI::LuaScriptModule          *m_scripts;

		MenuSourceConfig *m_config;

	private:
		void setupKeyCodes();

		typedef std::map<int, int> KEYCODES;

		KEYCODES smpKeyNormalCodes,
				 smpSpecialCodes;

		flowvr::utils::microtime m_dtLast;

		std::string m_strPortName,
					m_sLogFile,
					m_sMsXml,
					m_sIniScript;
		int m_nLogLevel;
		bool m_bWaitScript, m_bUpdate;
};


#endif // MENUSERVICE_H_
