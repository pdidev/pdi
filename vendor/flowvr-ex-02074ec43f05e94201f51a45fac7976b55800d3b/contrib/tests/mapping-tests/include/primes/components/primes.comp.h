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
 *  Contact : Jean-Denis Lesage                                    *
 *                                                                 *
 ******************************************************************/

// core library flowvr-app include
//#include <flowvr/app/core/component.h>
#include <flowvr/app/core/flowvr-app.h> // contain all core header files


#ifndef _PRIMES_H_
#define _PRIMES_H_

using namespace flowvr::app;

namespace primes
{
    
    // Primes is a composite component
    class Primes : public Composite
    {
    public :
        Primes(const std::string id_) : Composite(id_)
            {
                setInfo("Primes connects  capture, compute and visu metamodules");
                addParameter<int>("example",4); 
            };
        
        virtual ~Primes(){};
        
        // Composite components need an execute method. 
        virtual void execute();
        
        // Mandatory create method
        virtual Component* create() const	{ return new Primes(this->getId());	};
        
    };
    
    
};


#endif //_PRIMES_H_
