#ifndef FLOWVR_VRPN_FORCES_H
#define FLOWVR_VRPN_FORCES_H

#include <iostream>
#include <string>
#include <vector>
#include <map>

#include "flowvr/module.h"
#include <ftl/chunkevents.h>
#include <ftl/chunkwriter.h>
#include <ftl/chunkreader.h>


#include "ftl/vec.h"
#include "ftl/mat.h"

#include <vrpn_ForceDevice.h>

using namespace std;
using namespace flowvr;
using namespace ftl;

enum ForceTypes {
  SURFACE      = 0x01,
  TRIMESH      = 0x02,
  FIELD        = 0x03,
};

enum FeedbackForceMessage {
  /* Force Field */
  ORIGIN,
  FORCE,
  JACOBIAN,
  RADIUS,
  SENDFORCE,
  STOPFORCE
};

struct _constraint{
  int currentMode;
  Vec3f constraintPoint;
  Vec3f constraintLinePoint;
  Vec3f constraintLineDirection;
  Vec3f constraintPlanePoint;
  Vec3f constraintPlaneNormal;
  Vec3f constraintKSpring;
};

// surface parameters
struct _surfaceForces {
  Vec4f plane;
  float surfaceKSpring;
  float surfaceFDamping;
  float surfaceFstatic;
  float surfaceFDynamic;
  float surfaceKAdhesionNormal;
  float surfaceKAdhesionLateral;
  float surfaceBuzzFrequency;
  float surfaceBuzzAmplitude;
  float surfaceTextureWaveLength;
  float surfaceTextureAmplitude;
}; 


// force field parameters
/*struct _FieldForces {
  Vec3f FFOrigin;
  Vec3f FFForce;
  Mat3x3f FFJacobian;
  Vec3f FFRadius;  
  };*/


struct _FieldForces {
  float* FFOrigin;
  float* FFForce;
  float* FFJacobian;
  float  FFRadius;  
  
};


class forces {
 public:
  forces(int id) {
    force_type = id;
  };
  ~forces(){};
  int getType() {
    return(force_type);
  };
  //  virtual void ForceWriter()=0;
 protected:
  int force_type;  
  _constraint constraint;
};

class surfaceforces : public forces {
 public:
  surfaceforces() : forces(SURFACE) {};
  ~surfaceforces(){};  
  //  virtual void ForceWriter();
};

class trimeshforces : public forces {
 public:
  trimeshforces() : forces(TRIMESH) {};
  ~trimeshforces(){};
  //  virtual void ForceWriter();
};

class fieldforces : public forces, _FieldForces {
 protected:
  _FieldForces Fieldforces;
  
 public:  
  void setFFOrigin(float* values){
    for (int i=0; i<3; i++)
      Fieldforces.FFOrigin[i] = values[i];
  };
  
  float* getFFOrigin() {
    return Fieldforces.FFOrigin;
  };
  
  void setFFForce(float* values){
    for (int i=0; i<3; i++)
      Fieldforces.FFForce[i] = values[i];
  };
  
  
  float* getFFForce() {
    return Fieldforces.FFForce;
  };
  
  
  void setFFJacobian(float* values) {    
    for (int i=0; i<9; i++)
      Fieldforces.FFJacobian[i] = values[i];
  };
  
  
  float* getFFJacobian() {
    return Fieldforces.FFJacobian;
  };  
  
  void setFFRadius(float value){
    Fieldforces.FFRadius = value;
  };  
  
  float getFFRadius(){
    return Fieldforces.FFRadius;
  };

  bool isEmpty(){
    for (int i=0; i<3; i++) 
      if (Fieldforces.FFForce[i]!=0) 
	return false;
    return true;
  }
  
  friend std::ostream& operator<<(std::ostream& out, fieldforces& ff) {
    out << "The force of ForceField type : " << endl;
    out << "The force origine : " ;
    for (int i=0; i<3; i++)
      out << ff.getFFOrigin()[i] << " ";
    out << endl;
    out << "The vector force : " ;
    for (int i=0; i<3; i++) 
      out << ff.getFFForce()[i] << " ";
    out << endl;    
    out << "The force radius : " << ff.getFFRadius() << endl;
    out << "The Jacobian : " ;
    for (int i=0; i<3; i++) {
      for (int j=0; j<3; j++)
	out << ff.getFFJacobian()[3*i+j] << " ";
      out << endl;
    }
  };
  
  
  //  virtual void ForceWriter();
  void put(OutputPort* port);
  void ForceRecv(InputPort* port);
  void ForceMsg(Message msg);
  
  fieldforces(float* Origin, float* Force, float* Jacobian, float Radius) : forces(FIELD) {
    Fieldforces.FFOrigin = (float*) malloc(3*sizeof(float));
    Fieldforces.FFForce = (float*) malloc(3*sizeof(float));
    Fieldforces.FFJacobian = (float*) malloc(9*sizeof(float));
    setFFOrigin(Origin);
    setFFForce(Force);
    setFFJacobian(Jacobian);
    setFFRadius(Radius);
  };
  
  
  fieldforces() : forces(FIELD){
    Fieldforces.FFOrigin = (float*) malloc(3*sizeof(float));
    Fieldforces.FFForce = (float*) malloc(3*sizeof(float));
    Fieldforces.FFJacobian = (float*) malloc(9*sizeof(float));
  };
  
  fieldforces(float value) : forces(FIELD){
    Fieldforces.FFOrigin = (float*) malloc(3*sizeof(float));
    Fieldforces.FFForce = (float*) malloc(3*sizeof(float));
    Fieldforces.FFJacobian = (float*) malloc(9*sizeof(float));
    float* vec = (float*) malloc(3*sizeof(float));
    float* mat = (float*) malloc(9*sizeof(float));
    for (int i=0; i<3; i++) vec[i] = value;
    for (int i=0; i<9; i++) mat[i] = value;
    setFFOrigin(vec);
    setFFForce(vec);
    setFFJacobian(mat);
    setFFRadius(value);
  };
  
  ~fieldforces(){
    delete[] Fieldforces.FFOrigin;
    delete[] Fieldforces.FFForce;
    delete[] Fieldforces.FFJacobian;
  };   
};


/*struct _VRPNMsg {
  FeedbackForceMessage type;
  int n;
  float* data;
  };*/

class _VRPNMsg {
 public:
  FeedbackForceMessage type;
  int n;
  float* data;
  
  _VRPNMsg() {
    n = 0;
  };
  
  _VRPNMsg(FeedbackForceMessage _type, int _n, float* _data) {
    type = _type;
    n = _n;
    if (n!=0) {
      data = (float*) malloc(n*sizeof(float));
      for (int i=0; i<n; i++) 
	data[i] = _data[i];
    }
  };
  ~_VRPNMsg(){};

  void ApplyVRPNFunc(vrpn_ForceDevice_Remote* fdr);  

};

class forces2vrpn : public _VRPNMsg {
 public:
  forces2vrpn(){
    size = 0;
    len = 0;
  };
  ~forces2vrpn(){};
  void AddMessage(FeedbackForceMessage _type, int _n, float* _data) {
    _VRPNMsg message(_type, _n, _data);
    /*    message.type = _type;
	  message.n = _n;
	  message.data = (float*) malloc(_n*sizeof(float));
	  for (int i=0; i<_n; i++)
	  message.data[i] = _data[i];*/
    MessageVRPN.push_back(message);
    len++;
    size += 2*sizeof(int)+_n*sizeof(float);
  };
  void ClearMessage() {
    MessageVRPN.clear();
    size = 0;
    len = 0;
  };
  void Print() {
    cout << "MessageVRPN contient " << len << " messages." << endl;
    for (std::vector<_VRPNMsg>::iterator it = MessageVRPN.begin(); it != MessageVRPN.end(); it++) {      
      cout << "type : " << (*it).type << endl;
      cout << "n : " << (*it).n << endl;
      for (int i=0; i<(*it).n; i++) cout << (*it).data[i] << " ";
      cout << endl;
    }
  };
  int getSize() {
    return size;
  }
  int getLength() {
    return len;
  }
  
  void put(OutputPort* port);
  void VRPNMsg(Message msg);
  
 public:
  std::vector<_VRPNMsg> MessageVRPN;
  int size;
  int len;
};

#endif
