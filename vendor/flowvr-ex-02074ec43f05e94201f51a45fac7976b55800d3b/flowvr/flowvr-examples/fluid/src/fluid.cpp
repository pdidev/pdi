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
* File: ./flowvr.cpp                                              *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/

#include "fluid/std.h"
#include "fluid/mytime.h"
#include "fluid/Turbulent.h"

#include "flowvr/module.h"


namespace flowvrmod
{ // FLOWVR DATA
static flowvr::ModuleAPI* Module;
static flowvr::BufferPool OutPool(10);
// input
static flowvr::InputPort PortPositions("positions");
// output
static flowvr::StampList PortOutStamps;
static flowvr::StampInfo StampP("P",flowvr::TypeArray::create(2,flowvr::TypeInt::create()));
static flowvr::StampInfo StampN("N",flowvr::TypeArray::create(2,flowvr::TypeInt::create()));
static flowvr::OutputPort PortDensity("density",&PortOutStamps);
static flowvr::OutputPort PortVelocity("velocity",&PortOutStamps);
}

//void prog11( const int ni, const int nj, float threshold );

int main( int argc, char **argv )
{
    
    /*  Parse command line
     */

    // read simulation grid size
    int nx = 256, ny = 256;
    for ( int i=0 ; i<argc ; i++) {
        printf("argv[%d]=\"%s\"\n",i,argv[i]);
    }
    if ( argc >= 3 ) {//&& argv[argc-2][0]!='-' && argv[argc-1][0]!='-') {
        const int x = atoi(argv[argc-2]);
        const int y = atoi(argv[argc-1]);
        if ( x > 0  && y > 0 ) {
            nx = x;
            ny = y;
        }
    }
    
    /*  Start FlowVR
     */
    
    // PortOutStamps is the stamps list of both output ports
    flowvrmod::PortOutStamps.add(&flowvrmod::StampP);
    flowvrmod::PortOutStamps.add(&flowvrmod::StampN);
    
    std::vector<flowvr::Port*> ports;
    ports.push_back(&flowvrmod::PortPositions);
    ports.push_back(&flowvrmod::PortDensity);
    ports.push_back(&flowvrmod::PortVelocity);
    
    flowvrmod::Module = flowvr::initModule(ports);
    if ( flowvrmod::Module == NULL ) {
        return EXIT_FAILURE;
    }
    
    /*  Start Simulation
     */
    
    Turbulent sim(nx,ny);
    sim.Init();
    std::cout << "Simulation local grid at <" << sim.getLPx() << ',' << sim.getLPy() << "> size <" << sim.getLNx() << ',' << sim.getLNy() << ">" << std::endl;
    const int datasize = sim.getLNx()*sim.getLNy()*2; //*sizeof(float);
    
    /*  Main Loop
     */
    
    const mytime t0 = gettime();
    mytime lastt = t0;
    int it = 0;
    
    while ( flowvrmod::Module->wait() ) // loop until error
    {
        // Send Velocity
        if (flowvrmod::PortVelocity.isConnected()) {
            flowvr::MessageWrite msgvel;
            msgvel.stamps.write(flowvrmod::StampP[0],sim.getLPx());
            msgvel.stamps.write(flowvrmod::StampP[1],sim.getLPy());
            msgvel.stamps.write(flowvrmod::StampN[0],sim.getLNx());
            msgvel.stamps.write(flowvrmod::StampN[1],sim.getLNy());
            msgvel.data = flowvrmod::OutPool.alloc(flowvrmod::Module->getAllocator(),datasize);
            sim.getLocalVelocity(msgvel.data.getWrite<unsigned char>(0));
            flowvrmod::Module->put(&flowvrmod::PortVelocity,msgvel);
        }
        // Send Density
        if (flowvrmod::PortDensity.isConnected()) {
            flowvr::MessageWrite msgdens;
            msgdens.stamps.write(flowvrmod::StampP[0],sim.getLPx());
            msgdens.stamps.write(flowvrmod::StampP[1],sim.getLPy());
            msgdens.stamps.write(flowvrmod::StampN[0],sim.getLNx());
            msgdens.stamps.write(flowvrmod::StampN[1],sim.getLNy());
            msgdens.data = flowvrmod::OutPool.alloc(flowvrmod::Module->getAllocator(),datasize);
            sim.getLocalDensity(msgdens.data.getWrite<unsigned char>(0));
            flowvrmod::Module->put(&flowvrmod::PortDensity,msgdens);
        }
        // Read Positions
        if (flowvrmod::PortPositions.isConnected()) {
            // Gets the message from flowvr system
            flowvr::Message msgpos;
            flowvrmod::Module->get( &flowvrmod::PortPositions, msgpos );
            if ( msgpos.data.valid() && ! msgpos.data.empty() ) {
                const float * const mouse = msgpos.data.getRead<float>();
                Vec2D pos( sim.getNx() * mouse[0] + 1, sim.getNy() * mouse[1] + 1 );
                // Feeds the simulation with mouse input
                sim.MouseMotion( pos, (unsigned char) mouse[2] );
            }
        }
        
        // Compute new velocity & density
        sim.update();
        
        if (!(++it%100)) {
            const mytime t = gettime();
            std::cout << "fps "<<it/100<<" = "<<100/time2s(t-lastt)<<"  mean = "<<it/time2s(t-t0)<<std::endl;
            lastt=t;
        }
    }
    
    flowvrmod::Module->close();
    
    return 0;
}
