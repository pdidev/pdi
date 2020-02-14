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
 * This source is covered by the GNU GPL, please refer to the      *
 * COPYING file for further information.                           *
 *                                                                 *
 *-----------------------------------------------------------------*
 *                                                                 *
 *  Original Contributors:                                         *
 *    Sebastien Limet,                                             *
 *    Sophie Robert.                                               *
 *                                                                 * 
 *******************************************************************
 *                                                                 *
 * File: ./examples/sphere_phantom/src/force_generation.cpp        *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
//FlowVR IMPORTS
#include <flowvr/moduleapi.h>
#include <ftl/chunkevents.h>
#include <ftl/chunkwriter.h>
#include <ftl/quat.h>

#include <iostream>
#include <string>
#include <vector>

#include "flowvr-vrpn/core/forces.h"

using namespace std;
using namespace flowvr;



int main(int argc, const char** argv)
{
    
    flowvr::InputPort InTracker("Tracker");
    flowvr::OutputPort OutForce("Force");  
    std::vector<flowvr::Port*> ports;    
    ports.push_back(&InTracker);
    ports.push_back(&OutForce);
  
    flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
  
    if (flowvr == NULL)
        return 1;
    
    Message MsgTracker;

    float* _Origin = (float*) malloc(3*sizeof(float));  
    float* _Force = (float*) malloc(3*sizeof(float));
    float* _Jacobian = (float*) malloc(9*sizeof(float));
    
    while (flowvr->wait()) {        
        if (InTracker.isConnected()) {
            flowvr->get(&InTracker,MsgTracker);
            for (ftl::ChunkIterator it = ftl::chunkBegin(MsgTracker); it != ftl::chunkEnd(MsgTracker) ; it++) {
                const ftl::Chunk* c = (const ftl::Chunk*) it;  
                switch (c->type & 0x0F) {           	
                case ftl::ChunkEvent::POSITION:
                    const ftl::ChunkEventPosition* pos = (const ftl::ChunkEventPosition*) c;	
                    Vec3f Tracker;                    
                    for (int i=0; i<3; i++) {
                        _Origin[i] = pos->Transform[0][i];
                        Tracker[i] = _Origin[i];
                    }
                    
                    ftl::Quat qRotation;	
                    qRotation = ftl::Quat(pos->Transform[1][3], pos->Transform[1][0], pos->Transform[1][1], pos->Transform[1][2]);
                    Mat4x4f mRotation = matrixRotation(qRotation);

                    Vec3f Position;                    
                    for (int i=0; i<3; i++)
                        for (int j=0; j<3; j++)
                            Position[i]+= mRotation[i][j]*Tracker[j];
                    
                    Position.normalize();
                    for (int i=0; i<3; i++)
                        _Force[i] = 0.5*Position[i];
                }
            }
        }
        
        float _Radius = 0.01;
    
    
        for (int i=0; i<3; i++)
            for (int j=0; j<3; j++) {
                if (i==j)
                    _Jacobian[3*i+j] = 1.0;
                else
                    _Jacobian[3*i+j] = 0.0;
            }
        
        fieldforces ff(_Origin,_Force,_Jacobian,_Radius);
        
        cout << "Force envoyée : " << ff << endl;
        
        ff.put(&OutForce);
    }
    flowvr->close();
    return 0;
}
