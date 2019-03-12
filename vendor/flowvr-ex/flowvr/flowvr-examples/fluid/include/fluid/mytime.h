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
* File: ./mytime.h                                                *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#ifndef MYTIME_H
#define MYTIME_H

#include <sys/time.h>

//*

typedef double mytime;

inline mytime gettime()
{
  static timeval t;
  gettimeofday(&t,NULL);
  return ((mytime)t.tv_sec)+((mytime)(t.tv_usec*0.000001));
}

inline double time2ms(mytime t) { return t*1000.0; }

inline double time2us(mytime t) { return t*1000000.0; }

inline double time2s(mytime t) { return t; }

inline mytime ms2time(double ms) { return ms*0.001; }
inline mytime us2time(double us) { return us*0.000001; }
inline mytime s2time(double s) { return s; }

/*/

//typedef unsigned long long mytime;
typedef unsigned long mytime;

inline mytime gettime()
{
  unsigned int cnt1,cnt2;
  __asm__ (".byte 0x0f,0x31" :"=a" (cnt1),"=d" (cnt2));
  return cnt1; //(((mytime)(cnt2))<<32)+((mytime)(cnt1));
}

inline double gettime_()
{
  struct timeval t;
  gettimeofday (&t, 0);
  return  ((double)t.tv_sec)+((double)(t.tv_usec*0.000001));
}

double calcfreq()
{
  double t0=gettime_();
  mytime c0=gettime();
  usleep(100000);
  double t1=gettime_();
  mytime c1=gettime();
  double f=(c1-c0)/(t1-t0);
  printf("CPU freq= %lf MHz\n",f/1000000);
  return f;
}

double FREQ=calcfreq();

inline double time2s(mytime t) { return t/FREQ; }
inline double time2ms(mytime t) { return t*1000.0/FREQ; }
inline double time2us(mytime t) { return t*1000000.0/FREQ; }

inline mytime ms2time(double ms) { return mytime(ms*0.001*FREQ); }
inline mytime us2time(double us) { return mytime(us*0.000001*FREQ); }
inline mytime s2time(double s) { return mytime(s*FREQ); }

//*/

inline void waittime(mytime dt)
{
  mytime t=gettime()+dt;
  while (gettime()<t) ;
}

#endif
