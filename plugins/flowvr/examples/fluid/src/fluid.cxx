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
*  15/10/2018 Reworked for PDI:       Karol Sierocinski           *
*                                                                 *
*******************************************************************
*                                                                 *
* File: ./flowvr.cxx                                              *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*  15/10/2018 Karol Sierocinski <ksiero@man.poznan.pl>            *
*                                                                 *
******************************************************************/

#include "fluid/std.h"
#include "fluid/mytime.h"
#include "fluid/Turbulent.h"

#include <memory>

#include <pdi.h>


void prog11( const int ni, const int nj, float threshold );

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
    PC_tree_t config = PC_parse_path("config/fluid.yml");
    PDI_init(config);
        
    /*  Start Simulation
     */
    
    Turbulent sim(nx,ny);
    sim.Init();
    std::cout << "Simulation local grid at <" << sim.getLPx() << ',' << sim.getLPy() << "> size <" << sim.getLNx() << ',' << sim.getLNy() << ">" << std::endl;
    const int datasize = sim.getLNx()*sim.getLNy()*2; //*sizeof(float);
    
    {
        int nx_shr = sim.getLNx();
        int ny_shr = sim.getLNy();
        PDI_expose("NX", &nx_shr, PDI_OUT);
        PDI_expose("NY", &ny_shr, PDI_OUT);
    }

    /*  Main Loop
     */
    
    const mytime t0 = gettime();
    mytime lastt = t0;
    int it = 0;
    
    int wait;
    PDI_expose("wait", &wait, PDI_IN);
    while (wait) // loop until error
    {
        // Send Velocity
        int vel_stampP_xy[2] = {sim.getLPx(),  sim.getLPy()};
        PDI_expose("vel_P_xy", vel_stampP_xy, PDI_OUT);

        int vel_stampN_xy[2] = {sim.getLNx(),  sim.getLNy()};
        PDI_expose("vel_N_xy", vel_stampN_xy, PDI_OUT);

        unsigned char* vel_data;
        PDI_access("velocity", (void**)&vel_data, PDI_OUT);
        sim.getLocalVelocity(vel_data);
        PDI_release("velocity");

        // Send Density
        int den_stampP_xy[2] = {sim.getLPx(),  sim.getLPy()};
        PDI_expose("den_P_xy", den_stampP_xy, PDI_OUT);
 
        int den_stampN_xy[2] = {sim.getLNx(),  sim.getLNy()};
        PDI_expose("den_N_xy", den_stampN_xy, PDI_OUT);

        unsigned char* den_data;
        PDI_access("density", (void**)&den_data, PDI_OUT);
        sim.getLocalDensity(den_data);
        PDI_release("density");
        
        // Read Positions
        {
            float mouse[2];
            PDI_expose("pos_xy", mouse, PDI_IN);

            Vec2D pos( sim.getNx() * mouse[0] + 1, sim.getNy() * mouse[1] + 1 );
            
            int left_button;
            int middle_button;
            int right_button;
            PDI_expose("left_button", &left_button, PDI_IN);
            PDI_expose("middle_button", &middle_button, PDI_IN);
            PDI_expose("right_button", &right_button, PDI_IN);
            unsigned char buttons = 0;
            if (left_button) {
                buttons |= 0x01;
            }
            if (middle_button) {
                buttons |= 0x02;
            }
            if (right_button) {
                buttons |= 0x04;
            }
            sim.MouseMotion(pos, buttons);
        }
        
        // Compute new velocity & density
        sim.update();
        
        if (!(++it%100)) {
            const mytime t = gettime();
            std::cout << "fps "<<it/100<<" = "<<100/time2s(t-lastt)<<"  mean = "<<it/time2s(t-t0)<<std::endl;
            lastt=t;
        }
        PDI_expose("wait", &wait, PDI_IN);
    }
    
    PDI_finalize();
    
    return 0;
}
