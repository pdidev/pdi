/******* COPYRIGHT ************************************************
 *                                                                 *
 *                             FlowVR                              *
 *                     Application Library                         *
 *                                                                 *
 *-----------------------------------------------------------------*
 * COPYRIGHT (C) 2003-2011                by                       *
 * Laboratoire d'Informatique Fondamentale d'Orleans               *
 * (EA 4022) ALL RIGHTS RESERVED.                                  *
 *                                                                 *
 * This source is covered by the GNU LGPL, please refer to the     *
 * COPYING-LIB file for further information.                       *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Sebastien Limet                                              *
 *    Sophie Robert                                                *
 * 	  Yoann Kernoa												   *
 *                                                                 *
 *******************************************************************
 *                                                                 *
 * File: ./metamoduleSettingFreq.comp.h                            *
 *                                                                 *
 * Contacts:                                                       *
 *     22/07/2009 Y.Kernoa  <yoann.kernoa@etu.univ-orleans.fr>     *
 *                                                                 *
 ******************************************************************/

// flowvr-app core includes
//#include <flowvr/app/core/genclass.h> /// ----------------------------- nécessaire ???

// basic components includes
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>

// specific components includes
#include "flowvr/contrib/components/moduleSettingFreq.comp.h"

#ifndef _METAMODULE_SETTING_FREQ_H_
#define _METAMODULE_SETTING_FREQ_H_


namespace flowvr {	namespace render {

	class MetaModuleSettingFreq : public MetaModuleFlowvrRunSSHSingleton <ModuleSettingFreq>
	{
		public :
		
			//Interface construction : MetaModuleSingleDisplay clones Visu interface
			// param id_ : To identify component amongst its siblings
				//   id_ can be composed of, at least, 2 elements separeted by space ("settingFreq a_name_to_identify_GLUI_interface"). 
				//   Thanks to this, we can add a identifiant to know which GLUI window is associated to a synchronizor
			MetaModuleSettingFreq ( const std::string& id_) : MetaModuleFlowvrRunSSHSingleton <ModuleSettingFreq> ( id_ , CmdLine(id_) ) 
			{
			    setInfo("Metamodule launching 'settingFreq' module");
			    getRun()->openDisplay(":0"); // affichage sur l'écran principal
		    };

		  //Mandatory virtual destructor
		  virtual ~MetaModuleSettingFreq(){};
	
		  // Mandatory create  method
		  virtual Component* create() const { return new MetaModuleSettingFreq (getId()); };
	};
} }

#endif //_METAMODULE_SETTING_FREQ_H_
