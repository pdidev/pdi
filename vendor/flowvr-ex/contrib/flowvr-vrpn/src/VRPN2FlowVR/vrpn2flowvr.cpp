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
 * File: ./src/VRPN2FlowVR/vrpn2flowvr.cpp                         *
 *                                                                 *
 * Contacts:                                                       *
 *  01/06/2008 Sophie Robert <Sophie.Robert@univ-orleans.fr>       *
 *                                                                 *
 ******************************************************************/
#include <iostream>
#include <unistd.h>
#include <vector>

//VRPN IMPORTS
#include <vrpn_Tracker.h>
#include <vrpn_ForceDevice.h>
#include <vrpn_Button.h>
#include <vrpn_Analog.h>
#include <vrpn_Dial.h>
#include <vrpn_Text.h>
//FlowVR IMPORTS
#include <flowvr/moduleapi.h>

//FlowVR-VRPN IMPORTS
//This file should be placed somewhere in the FlowVR installation tree 
#include "flowvr_vrpn_stamps.h"

using namespace std;
using namespace flowvr;

ModuleAPI* flowvrmodule = 0;

OutputPort pOut("vrpnmsg");
std::vector<flowvr::Port*> ports;

StampInfo *TypeMsg = new StampInfo("TypeMsg",TypeInt::create());



//-----------------------------------------------------------
// Handlers for all kinds of VRPN buttons
//-----------------------------------------------------------

void handle_button(void *userdata, const vrpn_BUTTONCB b) 
{
    MessageWrite m;
    
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);        
    }
    //    cout << "Button :" << b.button << " " <<  b.state << endl;
    
    
    int taille = sizeof(vrpn_BUTTONCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&b),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_BUTTONCB);
    //    cout << "handle_button envoi " << endl;
    flowvrmodule->put(&pOut,m);
    //    cout << "handle_button envoi " << endl;
}

//-----------------------------------------------------------
// Handlers for all kinds of VRPN analogs
//-----------------------------------------------------------

void handle_analog(void * analog, const vrpn_ANALOGCB a)
{
    
    MessageWrite m;
    
    if (!flowvrmodule->wait()){
        flowvrmodule->close();

        exit(0);
    }
    
    //    for (int i=0; i< a.num_channel; i++)
      //        cout << "channel " << i << ":" << a.channel[i] << endl;
    
    
    int taille = sizeof(vrpn_ANALOGCB);  
    
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&a),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_ANALOGCB);  
    
    flowvrmodule->put(&pOut,m);
    //    cout << "handle_analog envoi " << endl;
}


//-----------------------------------------------------------
// Handlers for all kinds of VRPN tracker information
//-----------------------------------------------------------

// tracker orientation and position
void handle_tracker(void *userdata, const vrpn_TRACKERCB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    
    //    cout << "Tracker. Sensor :" <<  t.sensor << " " <<  t.pos[0] << " " << t.pos[1] << " " <<  t.pos[2] << " " << t.quat[0]  << " " << t.quat[1] << " " <<  t.quat[2] << " " <<  t.quat[3] << endl;

    int taille = sizeof(vrpn_TRACKERCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERCB);
    
    flowvrmodule->put(&pOut,m);
    //    cout << "handle_tracker envoi " << endl;
}

//  tracker velocity
void handle_trackervel(void *userdata, const vrpn_TRACKERVELCB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "Velocity. Sensor :" <<  t.sensor << "vel " <<  t.vel[0] << " " << t.vel[1] << " " <<  t.vel[2] << "future orientation " << t.vel_quat[0]  << " " << t.vel_quat[1] << " " <<  t.vel_quat[2] << " " <<  t.vel_quat[3] << "delta " <<  t.vel_quat_dt << endl;
    
    int taille = sizeof(vrpn_TRACKERVELCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERVELCB);
    
    flowvrmodule->put(&pOut,m);
}

// tracker acceleration
void handle_trackeracc(void *userdata, const vrpn_TRACKERACCCB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "Acceleration. Sensor :" <<  t.sensor << "acc " <<  t.acc[0] << " " << t.acc[1] << " " <<  t.acc[2] << "???? " << t.acc_quat[0]  << " " << t.acc_quat[1] << " " <<  t.acc_quat[2] << " " <<  t.acc_quat[3] << "delta " <<  t.acc_quat_dt << endl;
    
    int taille = sizeof(vrpn_TRACKERACCCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERACCCB);
    
    flowvrmodule->put(&pOut,m);
}

// tracker to room (???)
void handle_trackertracker2room(void *userdata, const vrpn_TRACKERTRACKER2ROOMCB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "Tracker2room. pos offset" <<  t.tracker2room[0] << " " << t.tracker2room[1] << " " <<  t.tracker2room[2] << " ori offset " << t.tracker2room_quat[0]  << " " << t.tracker2room_quat[1] << " " <<  t.tracker2room_quat[2] << " " <<  t.tracker2room_quat[3] << endl;
    
    int taille = sizeof(vrpn_TRACKERTRACKER2ROOMCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERTRACKER2ROOMCB);
    
    flowvrmodule->put(&pOut,m);
    
}

// tracker unit to sensor (???)
void handle_trackerunit2sensor(void *userdata, const vrpn_TRACKERUNIT2SENSORCB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "Unit2sensor. Sensor "<< t.sensor << "pos offset" <<  t.unit2sensor[0] << " " << t.unit2sensor[1] << " " <<  t.unit2sensor[2] << " ori offset " << t.unit2sensor_quat[0]  << " " << t.unit2sensor_quat[1] << " " <<  t.unit2sensor_quat[2] << " " <<  t.unit2sensor_quat[3] << endl;
    
    int taille = sizeof(vrpn_TRACKERTRACKER2ROOMCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERTRACKER2ROOMCB);
    
    flowvrmodule->put(&pOut,m);
    
}

// tracker workspace 
void handle_trackerworkspace(void *userdata, const vrpn_TRACKERWORKSPACECB t) {
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "Unit2sensor. Min" <<  t.workspace_min[0] << " " << t.workspace_min[1] << " " <<  t.workspace_min[2] << " Max " << t.workspace_max[0]  << " " << t.workspace_max[1] << " " <<  t.workspace_max[2] << endl;
    
    int taille = sizeof(vrpn_TRACKERWORKSPACECB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&t),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_TRACKERWORKSPACECB);
    
    flowvrmodule->put(&pOut,m);
    
}


//------------------------------------------------------------
// Handlers for all kinds of VRPN dials
//-----------------------------------------------------------

void handle_dial(void *userdata, const vrpn_DIALCB d) 
{
    
    MessageWrite m;
    if (!flowvrmodule->wait()){
        flowvrmodule->close();
        exit(0);
    }
    //  cout << "DIAL :" << d.dial << " " <<  d.change << endl;
    
    int taille = sizeof(vrpn_DIALCB);
    m.data = flowvrmodule->alloc(taille);
    memcpy(m.data.writeAccess(),(void*)(&d),taille);
    m.stamps.write(*TypeMsg,flowvr_vrpn_DIALCB);
    
    flowvrmodule->put(&pOut,m);
}


int main(int argc, char **argv) {       
    
    // Partie FlowVR

    pOut.stamps->add(TypeMsg);  
    ports.push_back(&pOut);
    
    flowvrmodule = initModule(ports);
    if (flowvrmodule == NULL)
        return 1;
    
    // Partie VRPN
    
    char * server;
    
    vrpn_Button_Remote *btn;
    vrpn_Analog_Remote *ana;  
    vrpn_Tracker_Remote *tkr;
    vrpn_Dial_Remote *dia;
    
    
    if (argc < 2) {
        cout << argv[0] << ": Invalid parms " << endl;
        cout << "Usage : " << endl;
        cout << argv[0] << "peripheric@host " << endl;
        return -1;
    }
    
    server = argv[1];
    // buttons  
    btn = new vrpn_Button_Remote(server);
    btn->register_change_handler(NULL, handle_button);
    
    // analogs
    ana = new vrpn_Analog_Remote(server);   
    ana->register_change_handler(NULL, handle_analog); 
    
    // trackers
    tkr = new vrpn_Tracker_Remote(server);   
    tkr->register_change_handler(NULL, handle_tracker);
    tkr->register_change_handler(NULL, handle_trackervel);
    tkr->register_change_handler(NULL, handle_trackeracc);
    tkr->register_change_handler(NULL, handle_trackertracker2room);
    tkr->register_change_handler(NULL, handle_trackerunit2sensor);
    tkr->register_change_handler(NULL, handle_trackerworkspace);
    
    // dial
    dia =  new vrpn_Dial_Remote(server);   
    dia->register_change_handler(NULL, handle_dial);
    
    while (1) {
        ana->mainloop();
        btn->mainloop();
        tkr->mainloop();
        dia->mainloop();
        
        if (!flowvrmodule->getStatus()){
            flowvrmodule->close();
            exit(0);
        }
	vrpn_SleepMsecs(10);
    }   
}
