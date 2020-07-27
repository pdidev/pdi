/******* COPYRIGHT ************************************************
*                                                                 *
*                             FlowVR                              *
*                       Template Library                          *
*                                                                 *
*-----------------------------------------------------------------*
* COPYRIGHT (C) 2003-2011                by                       *
* INRIA.  ALL RIGHTS RESERVED.                                    *
*                                                                 *
* This source is covered by the GNU LGPL, please refer to the     *
* COPYING file for further information.                           *
*                                                                 *
*-----------------------------------------------------------------*
*                                                                 *
*  Original Contributors:                                         *
*    Jeremie Allard,                                              *
*    Clement Menier.                                              *
*                                                                 * 
*******************************************************************
*                                                                 *
* File: include/ftl/quat.h                                        *
*                                                                 *
* Contacts: 20/09/2005 Clement Menier <clement.menier.fr>         *
*                                                                 *
******************************************************************/
#ifndef FTL_QUAT_H
#define FTL_QUAT_H

#include "vec.h"
#include "mat.h"

namespace ftl
{

// Basic angle-related definitions

static const double Pi = 3.1415926535897932384626433832795029;

inline double RAD2DEG(double a) { return a*180/Pi; }
inline double DEG2RAD(double a) { return a*Pi/180; }

/// Quaternion
template <class real> class Quat_
{
 public:

  real w,x,y,z;

  Quat_() : w(1),x(0),y(0),z(0) {}
  Quat_(real _w,real _x,real _y,real _z) : w(_w),x(_x),y(_y),z(_z) {}
  Quat_(real _w,const Vec<3,real> &_v) : w(_w), x(_v[0]),y(_v[1]),z(_v[2]) {}

  Quat_<real> & operator+=(const Quat_<real> &q)
	{
	  w+=q.w;
	  x+=q.x;
	  y+=q.y;
	  z+=q.z;
	  return *this;
	}

  Quat_<real> & operator-=(const Quat_<real> &q)
{
  w-=q.w;
  x-=q.x;
  y-=q.y;
  z-=q.z;
  return *this;
}
  Quat_<real> & operator*=(const Quat_<real> &q)
	{
	  return *this=(*this)*q;
	}
  //Quat_ & operator/=(Quat_ &q);
  Quat_<real> & operator*=(real f)
{
  w*=f;
  x*=f;
  y*=f;
  z*=f;
  return *this;
}
  Quat_<real> & operator/=(real f)
{
  w/=f;
  x/=f;
  y/=f;
  z/=f;
  return *this;
}

  Quat_<real> operator+(const Quat_<real> &q) const
		  {
		    return Quat_<real>(
		  	      w+q.w
		  	      ,x+q.x
		  	      ,y+q.y
		  	      ,z+q.z
		  	      );
		  }
  Quat_<real> operator-(const Quat_<real> &q) const
		  {
		    return Quat_<real>(
		  	      w-q.w
		  	      ,x-q.x
		  	      ,y-q.y
		  	      ,z-q.z
		  	      );
		  }
  Quat_<real> operator*(const Quat_<real> &q) const
  {
	return Quat_<real>(
		  w*q.w-x*q.x-y*q.y-z*q.z
		  ,w*q.x+x*q.w+y*q.z-z*q.y
		  ,w*q.y+y*q.w+z*q.x-x*q.z
		  ,w*q.z+z*q.w+x*q.y-y*q.x
		  );
  }
  //Quat_ operator/(Quat_ &q);
  Quat_<real> operator*(real f) const
		  {
		    return Quat_<real>(
		  	      w*f
		  	      ,x*f
		  	      ,y*f
		  	      ,z*f
		  	      );
		  }
  Quat_<real> operator/(real f) const
		  {
		    return Quat_<real>(
		  	      w/f
		  	      ,x/f
		  	      ,y/f
		  	      ,z/f
		  	      );
		  }


  friend Quat_<real> operator*(real f,const Quat_<real> &q)
  {
    return Quat_<real>(
  	      f*q.w
  	      ,f*q.x
  	      ,f*q.y
  	      ,f*q.z
  	      );
  }

  Quat_<real> operator-() const
		  {
		    return Quat_<real>(
		  	      -w
		  	      ,-x
		  	      ,-y
		  	      ,-z
		  	      );
		  }

  Quat_<real> operator~() const
  {
    return Quat_<real>(
  	      w
  	      ,-x
  	      ,-y
  	      ,-z
  	      );
  }

  void fromAngAxis(real ang,Vec<3,real> axis)
  {
  // quaternions can represent a rotation.  The rotation is an angle t, around a
  // unit vector u.   q=(s,v);  s= cos(t/2);   v= u*sin(t/2).
    axis.normalize();

    w=(real)cos(ang/2);
    real f=(real)sin(ang/2);
    x=axis.x()*f;
    y=axis.y()*f;
    z=axis.z()*f;
  }

  void toAngAxis(real *ang,Vec<3,real> *axis) const
  {
	*ang = (real)acos (w) * 2;
	axis->x() = x;
	axis->y() = y;
	axis->z() = z;
	axis->normalize();
  }

  void fromDegreeAngAxis(real ang,Vec<3,real> axis)
  {
    fromAngAxis(ang*Pi/180.0f, axis);
  }

  void toDegreeAngAxis(real *ang,Vec<3,real> *axis) const
  {
	toAngAxis(ang, axis);
	*ang *= 180.0f/Pi;
  }

  void fromMatrix(const Mat<3,3,real> &m)
  {
    real   tr, s;

    tr = m.x().x() + m.y().y() + m.z().z();

    // check the diagonal
    if (tr > 0)
    {
      s = (real)sqrt (tr + 1);
      w = s * 0.5f; // w OK
      s = 0.5f / s;
      x = (m.y().z() - m.z().y()) * s; // x OK
      y = (m.z().x() - m.x().z()) * s; // y OK
      z = (m.x().y() - m.y().x()) * s; // z OK
    }
    else
    {
      if (m.y().y() > m.x().x() && m.z().z() <= m.y().y())
      {
        s = (real)sqrt ((m.y().y() - (m.z().z() + m.x().x())) + 1.0f);

        y = s * 0.5f; // y OK

        if (s != 0.0f)
          s = 0.5f / s;

        z = (m.z().y() + m.y().z()) * s; // z OK
        x = (m.y().x() + m.x().y()) * s; // x OK
        w = (m.z().x() - m.x().z()) * s; // w OK
      }
      else if ((m.y().y() <= m.x().x()  &&  m.z().z() > m.x().x())  ||  (m.z().z() > m.y().y()))
      {
        s = (real)sqrt ((m.z().z() - (m.x().x() + m.y().y())) + 1.0f);

        z = s * 0.5f; // z OK

        if (s != 0.0f)
          s = 0.5f / s;

        x = (m.x().z() + m.z().x()) * s; // x OK
        y = (m.z().y() + m.y().z()) * s; // y OK
        w = (m.x().y() - m.y().x()) * s; // w OK
      }
      else
      {
        s = (real)sqrt ((m.x().x() - (m.y().y() + m.z().z())) + 1.0f);

        x = s * 0.5f; // x OK

        if (s != 0.0f)
            s = 0.5f / s;

        y = (m.y().x() + m.x().y()) * s; // y OK
        z = (m.x().z() + m.z().x()) * s; // z OK
        w = (m.y().z() - m.z().y()) * s; // w OK
      }
    }
  }

  void toMatrix(Mat<3,3,real> *m) const
  {
	real wx, wy, wz, xx, yy, yz, xy, xz, zz;

	xx = 2 * x * x;   xy = 2 * x * y;   xz = 2 * x * z;
	yy = 2 * y * y;   yz = 2 * y * z;   zz = 2 * z * z;
	wx = 2 * w * x;   wy = 2 * w * y;   wz = 2 * w * z;

	m->x().x() = 1 - yy - zz;  m->y().x() = xy - wz;      m->z().x() = xz + wy;
	m->x().y() = xy + wz;      m->y().y() = 1 - xx - zz;  m->z().y() = yz - wx;
	m->x().z() = xz - wy;      m->y().z() = yz + wx;      m->z().z() = 1 - xx - yy;
  }


  real length() const
  {
	return (real)sqrt(w*w+x*x+y*y+z*z);
  }

  void clear()
  {
    w=1;
    x=0;
    y=0;
    z=0;
  }
  void normalize()
  {
    real f=length();
    if (f>0)
    {
      f=1/f;
      w*=f; x*=f; y*=f; z*=f;
    }
  }

  bool isIdentity() const
  {
	return w>=1.0f;
  }


  typedef Quat_<float> Quat;
  typedef Quat_<double> Quatd;
};

// Assignement from typed data

namespace Type
{

template<class real> __inline__ Type get(const Quat_<real>&) { return get(Vec<4,real>()); }

template <>
bool assign(Quat& dest, int type, const void* data);


template <>
bool assign(Quatd& dest, int type, const void* data);


} // namespace Type

} // namespace ftl

template<typename real> std::ostream& operator<<(std::ostream& o, const ftl::Quat_<real>& q)
{
  o << ftl::Vec<4,real>(q.w,q.x,q.y,q.z);
  return o;
}


template<typename real> std::istream& operator>>(std::istream& in, ftl::Quat_<real>& q)
{
  ftl::Vec<4,real> v;
  in >> v;
  q.w = v[0];
  q.x = v[1];
  q.y = v[2];
  q.z = v[3];
  return in;
}

#endif
