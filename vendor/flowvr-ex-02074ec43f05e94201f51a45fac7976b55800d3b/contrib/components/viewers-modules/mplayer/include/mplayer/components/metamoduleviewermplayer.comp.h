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

// basic components includes
#include <flowvr/app/components/metamoduleflowvr-run-ssh-singleton.comp.h>
#include <flowvr/render/components/moduleviewer.comp.h>

// primes specific components includes

#ifndef _METAMODULEVIEWERMPLAYER_H_
#define _METAMODULEVIEWERMPLAYER_H_

using namespace flowvr::app;
using namespace flowvr::render;

namespace mplayer {

    // The goal of this component is to encapsulate viewer component and add a command line to launch it
    // ModuleViewer is a generic class with the common interface of viewer components. Use it if your viewer has only a scene and a matrix ports.
class MetaModuleViewerMPlayer : public MetaModuleFlowvrRunSSHSingleton<ModuleViewer>
{
public :
 MetaModuleViewerMPlayer(const std::string& id_ ) : MetaModuleFlowvrRunSSHSingleton<ModuleViewer>(id_, CmdLine("mplayer") )
  {
    setInfo("Metamodule launching viewer mplayer");
    addParameter("MOVIEFILENAME");
  };

  virtual void configure()
  {
     FlowvrRunSSH* ssh = getRun();
     ssh->addArg("-vo flowvr -noflowvr -vf pp=fd ");
     ssh->addArg(getParameter<std::string>("MOVIEFILENAME"));
  }
  //Mandatory virtual destructor
  virtual ~MetaModuleViewerMPlayer(){};

  // Mandatory create  method
  virtual Component* create() const { return new MetaModuleViewerMPlayer(getId());};
};

};

#endif //_METAMODULEVIEWERMPLAYER_H_
