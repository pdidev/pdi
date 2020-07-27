/******* COPYRIGHT ************************************************
 *                                                                 *
 *                         FlowVR VRPN                             *
 *                    FlowVR VRPN Coupling Modules                 *
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
 *    Sebastien Limet,                                             *
 *    Sophie Robert.                                               *
 *                                                                 * 
 *******************************************************************
 *                                                                 *
 * File: ./examples/sphere_phantom/include/sphere_phantom/components/Phantom.comp.h*
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#ifndef SIMULATEDPHANTOM_COMP_H_
#define SIMULATEDPHANTOM_COMP_H_

#include <flowvr/app/core/component.h>

using namespace flowvr::app;

class SimulatedPhantom : public Composite {
    public :
    SimulatedPhantom(const std::string& id_) : Composite(id_) {
        addPort("buttons", OUTPUT);
        addPort("tracker", OUTPUT);                                        
    }
    
    virtual Component* create() const {
        return new SimulatedPhantom(getId());
    }
    
    virtual void execute();
};

#endif
