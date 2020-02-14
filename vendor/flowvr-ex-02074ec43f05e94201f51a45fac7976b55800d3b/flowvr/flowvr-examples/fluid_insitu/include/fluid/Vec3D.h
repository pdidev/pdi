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
* File: ./Vec3D.h                                                 *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef VEC3D_H__
#define VEC3D_H__

class Vec3D {
public:

  Vec3D(float _x=0.0f,float _y=0.0f,float _z=0.0f)
   : x(_x), y(_y), z(_z)
  {;}

  float x,y,z;

  void operator +=(const Vec3D& a)
  {
    x+=a.x; y+=a.y; z+=a.z;
  }

  void operator *=(float f)
  {
    x*=f; y*=f; z*=f;
  }

  float length2() const
  {
    return x*x+y*y+z*z;
  }

  float length() const
  {
    return (float)sqrt(length2());
  }
#ifdef USEGL
  void glVertex()
  {
    glVertex3f(x,y,z);
  }
#endif
  static const Vec3D Null;

};

extern inline Vec3D operator+(const Vec3D& a,const Vec3D& b)
{
  return Vec3D(a.x+b.x,a.y+b.y,a.z+b.z);
}

extern inline Vec3D operator-(const Vec3D& a,const Vec3D& b)
{
  return Vec3D(a.x-b.x,a.y-b.y,a.z-b.z);
}

extern inline Vec3D operator-(const Vec3D& a)
{
  return Vec3D(-a.x,-a.y,-a.z);
}

extern inline float operator*(const Vec3D& a,const Vec3D& b)
{
  return a.x*b.x+a.y*b.y+a.z*b.z;
}

extern inline Vec3D operator*(const Vec3D& a,float b)
{
  return Vec3D(a.x*b,a.y*b,a.z*b);
}

extern inline Vec3D operator*(float b,const Vec3D& a)
{
  return Vec3D(a.x*b,a.y*b,a.z*b);
}

extern inline Vec3D operator/(const Vec3D& a,float b)
{
  return Vec3D(a.x/b,a.y/b,a.z/b);
}

#endif
