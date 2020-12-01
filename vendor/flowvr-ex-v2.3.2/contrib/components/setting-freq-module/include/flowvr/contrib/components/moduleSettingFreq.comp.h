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
 * File: ./moduleSettingFreq.comp.h                                *
 *                                                                 *
 * Contacts:                                                       *
 *     22/07/2009 Y.Kernoa  <yoann.kernoa@etu.univ-orleans.fr>     *
 *                                                                 *
 ******************************************************************/

// basic components includes
#include <flowvr/app/components/module.comp.h>
#include <string>

#ifndef _MODULE_SETTING_FREQ_H_
#define _MODULE_SETTING_FREQ_H_


namespace flowvr { namespace render {

	class ModuleSettingFreq : public Module 
	{
		public :
		
		  ModuleSettingFreq ( const std::string& id_ ) : Module( id_ )
		  {
		    setInfo( "Module SettingFreq send a frequency to control VariableFrequency Synchronizor via a GLUI user interface" );
			
		    // parameters for "SettingFreq"
		    addParameter<float>("freqHz", 100);
		    addParameter<float>("freqMin", 1);
		    addParameter<float>("freqMax", 1000000);
		    addParameter<std::string>("titleHz", "Unidentified module");
		    
		    // Interface declaration
    		addPort( "Button" , INPUT ); // Wiimote
    		addPort( "oFreq" , OUTPUT );
    		
		    // beginIt and endIt ports created by the Module class
		  };


		  //Mandatory virtual destructor
		  virtual ~ModuleSettingFreq (){};

		  // Mandatory create method
		  virtual Component* create() const { return new ModuleSettingFreq( getId() );};
	};
}}
#endif //_MODULE_SETTING_FREQ_H_
