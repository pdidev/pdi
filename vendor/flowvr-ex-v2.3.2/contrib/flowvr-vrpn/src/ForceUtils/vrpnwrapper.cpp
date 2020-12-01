#include "flowvr-vrpn/core/vrpnwrapper.h"

void _setFF_Origin(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction application de l'origine de la force" << endl;
  fdr->setFF_Origin(data[0],data[1],data[2]);
}

void _setFF_Force(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction application de la force" << endl;
  fdr->setFF_Force(data[0],data[1],data[2]);
}

void _setFF_Jacobian(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction application de la matrice" << endl;
  fdr->setFF_Jacobian(data[0],data[1],data[2],
				    data[3],data[4],data[5],
				    data[6],data[7],data[8]);
}

void _setFF_Radius(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction application du rayon de la force" << endl;
  fdr->setFF_Radius(data[0]);
}

void _sendForceField(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction send force " << endl;
  fdr->sendForceField();
}

void _stopForceField(float* data, vrpn_ForceDevice_Remote* fdr)
{
  //  cout << "appel de la fonction arrêt de la force" << endl;
  fdr->stopForceField();
}

