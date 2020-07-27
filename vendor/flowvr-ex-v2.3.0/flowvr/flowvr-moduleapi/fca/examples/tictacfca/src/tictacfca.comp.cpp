/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                     Application Library                         *
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
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Thomas Arcila,                                               *
*    Jean-Denis Lesage.                                           *	
*    Clement Menier,                                              *
*    Bruno Raffin                                                 *
*                                                                 *
*******************************************************************
 *                                                                 *
 *  Contact : Jean-Denis.Lesage@imag.fr                            *
 *                                                                 *
 ******************************************************************/

#include "flowvr/app/core/genclass.h"

#include "tictacfca/components/tictacfca.comp.h"


// TicTac specific components includes
#include "tictacfca/components/metamoduleputfca.comp.h"
#include "tictacfca/components/metamodulegetfca.comp.h"

// Contain all components header files (useless for this example, but enables to test there is no compilation errors when building flowvr-suite)
#include <flowvr/app/components/flowvr-app.comp.h>



using namespace flowvr::app;

namespace tictacfca
{

  // Required to enable dynamic component loading
  // Argument given in parameter is the class name (respect case) (also requires a TicTac(id) constructor)
  GENCLASS(TicTacFca)


  void TicTacFca::execute()
  {
    // Instantiation of metamodules tic and tac
    MetaModulePutFca * metamoduleputfca  = addObject<MetaModulePutFca >("putfca");
    MetaModuleGetFca * metamodulegetfca  = addObject<MetaModuleGetFca >("getfca");
    
    // A connection component will send message from Tic to Tac 
    //    Connection * connec  = addObject<Connection >("connec");
    
    // link all components
    //    link(metamoduleput->getPort("text"), connec->getPort("in"));
    //    link(connec->getPort("out"), metamoduleget->getPort("text"));
    link(metamoduleputfca->getPort("text"), metamodulegetfca->getPort("text"));
  };


};
