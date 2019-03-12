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

/** tictac application*/

// basic components includes
#include <flowvr/app/core/component.h>

#ifndef _TICTACFCA_H_
#define _TICTACFCA_H_

using namespace flowvr::app;

namespace tictacfca
{

class TicTacFca : public Composite
{
public :

  //  Constructor with id (required so this component can be dynamically loaded)
   TicTacFca(const std::string id_): Composite(id_)
  {
    setInfo("Connect a putfca and getfca  metamodules");
  };


  // virtual destructor
  virtual ~TicTacFca(){};


  // Composite components need an execute method. The network of tictac application is defined in this method.
  virtual void execute();
  
  // Mandatory create method
  virtual Component* create() const { return new TicTacFca(this->getId()); };

};




};

#endif //_TICTACFCA_H_
