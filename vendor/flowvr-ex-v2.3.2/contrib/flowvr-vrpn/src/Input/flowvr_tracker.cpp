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
 * File: ./src/Input/flowvr_tracker.cpp                            *
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

#include <vrpn_Tracker.h>
#include <quat.h>

#include <iostream>
#include <string>
#include <vector>

#include "flowvr_vrpn_stamps.h"

using namespace std;
using namespace flowvr;


int main(int argc, const char** argv)
{
    
    flowvr::InputPort pIn("vrpn_tracker");
    flowvr::OutputPort pOut("ftl_tracker");
    
    std::vector<flowvr::Port*> ports;
    
    
    StampInfo TypeMsgStamp("TypeMsg",TypeInt::create());
    (&pIn)->stamps->add(&TypeMsgStamp);
    
    ports.push_back(&pIn);
    ports.push_back(&pOut);
    
    int TypeMsg;
    int Iteration;
    float posori[16];
    memset(posori,0,16*sizeof(float));
    ftl::ChunkEventWriter* trackerMsg = NULL;
    
    
    
    flowvr::ModuleAPI* flowvr = flowvr::initModule(ports);
    
    if (flowvr == NULL)
        return 1;
    
    flowvr::Message m;    
    vrpn_TRACKERCB t;
    

    while (flowvr->wait()) {          
        trackerMsg = new ftl::ChunkEventWriter();
        // Get Message
        flowvr->get(&pIn,m);
        m.stamps.read(pIn.stamps->it,Iteration);
        m.stamps.read(TypeMsgStamp,TypeMsg);    
        
        //    cout << "Type du message reçu : " << TypeMsg << endl;
        
        if (TypeMsg==flowvr_vrpn_TRACKERCB) {      
            memcpy((void*)(&t),m.data.readAccess(),m.data.getSize());      
	    /*  cout << "tracker : " << t.sensor << " : " << " Pos = (" <<  t.pos[0] <<","
                    << t.pos[1] << "," << t.pos[2] << ") Ori = ("<< t.quat[0] << "," 
                    <<  t.quat[1] << "," << t.quat[2] << "," << t.quat[3] << ")" 
                    << " à l'itération : " << Iteration << endl;
	    */
            // the position + quaternion given by vrpn are encoded in a ftl position 
            // matrix where the first line gives the position and the second the
            // orientation vrpn quaternion
            for(int i=0; i<3; i++)
                posori[i]=static_cast<float>(t.pos[i]);
            posori[3]=0;
            for(int i=0;i<4;i++)
                posori[4+i]=static_cast<float>(t.quat[i]);
        }
        trackerMsg->addEventPosition(0,posori);
        trackerMsg->put(&pOut);            
        delete trackerMsg;
    }
    
    flowvr->close();
    return 0;
}
