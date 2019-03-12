#ifndef FLOWVR_VRPN_VRPNWRAPPER
#define FLOWVR_VRPN_VRPNWRAPPER

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include <vrpn_ForceDevice.h>


using namespace std;

void _setFF_Origin(float* data, vrpn_ForceDevice_Remote* fdr);
void _setFF_Force(float* data, vrpn_ForceDevice_Remote* fdr);
void _setFF_Jacobian(float* data, vrpn_ForceDevice_Remote* fdr);
void _setFF_Radius(float* data, vrpn_ForceDevice_Remote* fdr);
void _sendForceField(float* data, vrpn_ForceDevice_Remote* fdr);
void _stopForceField(float* data, vrpn_ForceDevice_Remote* fdr);

#endif

