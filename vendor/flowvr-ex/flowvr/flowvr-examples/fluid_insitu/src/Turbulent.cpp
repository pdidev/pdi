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
* File: ./Turbulent.cpp                                           *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "fluid/std.h"
#define FLUID_PERIODIC 0
#include "fluid/Turbulent.h"

scalar VelViscosity=0; //0.000001;

void Turbulent::setVisco(float v)
{
  VelViscosity = v;
}

scalar TextViscosity=0; //0.0001;

const scalar TextDecay=0.002;

Turbulent::Turbulent(int _nx,int _ny)
  : GridDistribution(_nx, _ny)
{
  mFirstMouseMotion=true;
}

Turbulent::~Turbulent()
{
}

int Turbulent::Init()
{
  mFirstMouseMotion=true;

  if (GridDistribution::init())
  {
    std::cerr << "GridDistribution failed."<<std::endl;
    return 1;
  }

  velocity.init(this);
  density.init(this);
  vtemp1.init(this);
  stemp1.init(this);
  stemp2.init(this);
  stemp3.init(this);

  // Init Forces
  DeplBuffer.ndepl=0;
  return 0;
}

inline Vec2D operator+(const Vec2D& a,const Vec2D& b)
{
  return Vec2D(a.x+b.x,a.y+b.y);
}

inline Vec2D operator-(const Vec2D& a,const Vec2D& b)
{
  return Vec2D(a.x-b.x,a.y-b.y);
}

inline Vec2D operator*(const Vec2D& v,scalar f)
{
  return Vec2D(v.x*f,v.y*f);
}

inline Vec2D operator*(scalar f,const Vec2D& v)
{
  return Vec2D(f*v.x,f*v.y);
}

int Turbulent::update()
{
  updateForces();
  updateVelocity();
  updateDensity();
  return 0;
}

int Turbulent::updateForces()
{
  Vec2D * vel = velocity.writeInnerBegin();
  Vec2D * dens = density.writeInnerBegin();

  for (int i=0;i<DeplBuffer.ndepl;i++)
  {
    Vec2D force = DeplBuffer.depl[i];
    float flen = sqrt(force.x*force.x+force.y*force.y);
    if (flen>1) force*=1/flen;
    int npoints = (int)flen+1;
    //std::cout << npoints << " <"<<force.x<<','<<force.y<<"> @ <"<<DeplBuffer.pos[i].x<<','<<DeplBuffer.pos[i].x<<">"<<std::endl;
    for (int j=1;j<=npoints;j++)
      calcForce(DeplBuffer.pos[i]+DeplBuffer.depl[i]*(((float)j)/npoints),force,vel);
    if (DeplBuffer.bt[i])
    {
      force.x = (DeplBuffer.bt[i]&3)?1.0f:0.0f;
      force.y = (DeplBuffer.bt[i]&6)?1.0f:0.0f;
      for (int j=1;j<=npoints;j++)
	addForce(DeplBuffer.pos[i]+DeplBuffer.depl[i]*(((float)j)/npoints),force,dens);
    }
  }
  
  velocity.writeInnerEnd();
  density.writeInnerEnd();


  /* ***********************************************************************
     Les variables suivantes positionnent les vecteurs vitesse sur la grille,
     La premiere source est entre les coordonnees <nx*d0/divis,0> et
     <nx*d1/divis,ny*l/divis>. La deuxieme source est entre les coordonnees
     <0,ny*d0/divis> et <nx*l/divis,d1*l/divis>
     *********************************************************************** */
  
  static const scalar source=0.8;
  static const scalar srcdens=1.0;
  static const int divis=40;
  static const int d0=19;
  static const int d1=21;
  static const int l=2;
  {

    GridPart zone0;
    zone0.P[0] = getNx()*d0/divis; zone0.N[0]=getNx()*(d1-d0)/divis;
    zone0.P[1] = 2;                zone0.N[1]=getNy()*l/divis;

    velocity.fillRegion(part2inner(zone0),Vec2D(0,source));
    density.fillRegion(part2inner(zone0),Vec2D(0,srcdens));

    GridPart zone1;
    zone1.P[0] = 2;                zone1.N[0]=getNx()*l/divis;
    zone1.P[1] = getNy()*d0/divis; zone1.N[1]=getNy()*(d1-d0)/divis;

    velocity.fillRegion(part2inner(zone1),Vec2D(source,0));
    density.fillRegion(part2inner(zone1),Vec2D(srcdens,0));

  }

  DeplBuffer.ndepl=0;
  return 0;
}

void Turbulent::MouseMotion(Vec2D pos, int bt)
{
  if (mFirstMouseMotion)
  {
    mFirstMouseMotion=false;
  }
  else
  {
    Vec2D depl=pos-mLastMousePos;
    if (DeplBuffer.ndepl<NDEPLMAX)
    {
      ++DeplBuffer.ndepl;
    }
    DeplBuffer.pos[DeplBuffer.ndepl-1]=mLastMousePos;
    DeplBuffer.depl[DeplBuffer.ndepl-1]=depl;
    DeplBuffer.bt[DeplBuffer.ndepl-1]=bt;
  }
  mLastMousePos=pos;
}

void Turbulent::calcForce(Vec2D pos,Vec2D force,Vec2D * grid)
{
  int x0=f2int_down(pos.x); //float fx=pos.x-x0;
  int y0=f2int_down(pos.y); //float fy=pos.y-y0;

  int p = global2pos(x0,y0);
  if (isInnerY(y0))
  {
    if (isInnerX(x0))
      grid[p]=force; //lerp(grid[p],force,(1-fx)*(1-fy));
    if (isInnerX(x0+1))
      grid[p+1]=force; //lerp(grid[p+1],force,(  fx)*(1-fy));
  }
  if (isInnerY(y0+1))
  {
    if (isInnerX(x0))
      grid[p+getLineSize()]=force; //lerp(grid[p+getLineSize()],force,(1-fx)*(  fy));
    if (isInnerX(x0+1))
      grid[p+1+getLineSize()]=force; //lerp(grid[p+1+getLineSize()],force,(  fx)*(  fy));
  }
}

void Turbulent::addForce(Vec2D pos,Vec2D force,Vec2D * grid)
{
  int x0=f2int_down(pos.x); float fx=pos.x-x0;
  int y0=f2int_down(pos.y); float fy=pos.y-y0;

  int p = global2pos(x0,y0);
  if (isInnerY(y0))
  {
    if (isInnerX(x0))   grid[p                ].add1(force,(1-fx)*(1-fy));
    if (isInnerX(x0+1)) grid[p+1              ].add1(force,(  fx)*(1-fy));
  }
  if (isInnerY(y0+1))
  {
    if (isInnerX(x0))   grid[p+getLineSize()  ].add1(force,(1-fx)*(  fy));
    if (isInnerX(x0+1)) grid[p+1+getLineSize()].add1(force,(  fx)*(  fy));
  }
}

int Turbulent::updateVelocity()
{
  diffuse(vtemp1, velocity, VelViscosity);
  advect (velocity, vtemp1, velocity);
  project(velocity);
  return 0;
}

int Turbulent::updateDensity()
{
  diffuse(vtemp1, density, TextViscosity);
  advect (density, vtemp1, velocity);
  return 0;
}

#define NX (local.N[0])
#define NY (local.N[1])

#define IX(i,j) ((i)+(NX+2)*(j))
#define DY (store.N[0])

#define FOR_GHOST_CELLS(cmd) \
{ \
  int i,j,p; \
  i = 0; j = 0; p = 0; \
  for ( ; i <= NX ; i++, p++) cmd; \
  for ( ; j <= NY ; j++, p+=DY) cmd; \
  for ( ; i > 0 ; i--, p--) cmd; \
  for ( ; j > 0 ; j--, p-=DY) cmd; \
}

#define FOR_BORDER_CELLS(cmd) \
{ \
  int i,j,p; \
  i = 1; j = 1; p = DY+1; \
  for ( ; i < NX ; i++, p++) cmd; \
  for ( ; j < NY ; j++, p+=DY) cmd; \
  for ( ; i > 1 ; i--, p--) cmd; \
  for ( ; j > 1 ; j--, p-=DY) cmd; \
}

#define FOR_LOCAL_CELLS(T,res,cmd) \
{ \
  T* r = (res).writeLocalBegin(); \
  FOR_BORDER_CELLS(cmd); \
  (res).writeBorderEnd(); \
  { \
    int i,j,p; \
    for ( j = 2, p=2*DY+2 ; j<NY ; j++, p+=4 ) \
      for ( i=2 ; i<NX ; i++,p++ ) \
        cmd; \
  } \
}

#define FOR_INNER_CELLS(T,res,cmd) \
{ \
  T* r = (res).writeInnerBegin(); \
  int i,j,p; \
  for ( j = 0, p=0 ; j<=NY+1 ; j++ ) \
    for ( i=0 ; i<=NX+1 ; i++, p++ ) \
      cmd; \
  (res).writeInnerEnd(); \
}

#define FOR_ALL_CELLS(T,res,cmd) \
{ \
  T* r = (res).writeAllBegin(); \
  int i,j,p; \
  for ( j = 0, p=0 ; j<=NY+1 ; j++ ) \
    for ( i=0 ; i<=NX+1 ; i++, p++ ) \
      cmd; \
  (res).writeAllEnd(); \
}

void Turbulent::lin_solve_step_v(VGrid& result, VGrid& ref, VGrid& prev, float a, float c)
{
  float inv_c=1.0f/c;
  const Vec2D* x0 = ref.readLocal();
  const Vec2D* xprev = prev.readAll();
  FOR_LOCAL_CELLS(Vec2D,result,
  {
    r[p].x = (x0[p].x + a*(xprev[p-1].x+xprev[p+1].x+xprev[p-DY].x+xprev[p+DY].x))*inv_c;
    r[p].y = (x0[p].y + a*(xprev[p-1].y+xprev[p+1].y+xprev[p-DY].y+xprev[p+DY].y))*inv_c;
  }
  )
}

void Turbulent::lin_solve_step_s(SGrid& result, SGrid& ref, SGrid& prev, float a, float c)
{
  float inv_c=1.0f/c;
  const scalar* x0 = ref.readLocal();
  const scalar* xprev = prev.readAll();
  FOR_LOCAL_CELLS(scalar,result, r[p] = (x0[p] + a*(xprev[p-1]+xprev[p+1]+xprev[p-DY]+xprev[p+DY]))*inv_c )
}

void Turbulent::diffuse(VGrid& result, VGrid& prev, float diff)
{
  if (diff>0.0f)
  {
    float a=diff*getNx()*getNy();
    lin_solve_step_v ( result, prev, prev, a, 1.0001f+4*a);
  }
  else
  {
    float inv_c=1.0f/1.0001f;
    const Vec2D* x0 = prev.readAll();
    FOR_INNER_CELLS(Vec2D,result,
    {
      r[p].x = x0[p].x*inv_c;
      r[p].y = x0[p].y*inv_c;
    }
    )
  }
}

void Turbulent::advect(VGrid& result, VGrid& prev, VGrid& vel)
{
  int p0;
  float x, y;

  const Vec2D* v = vel.readLocal();
  const Vec2D* d0 = prev.readAll();

  FOR_LOCAL_CELLS(Vec2D,result,
  {
    x = -v[p].x;
    y = -v[p].y;
    p0=p;
    /*
    // BUG: This does not work with GCC 4
    int sx = (*(int*)&x)>>31;
    int sy = (*(int*)&y)>>31;
    p0+=sx+sy*DY;
    x-=sx;
    y-=sy;
    /*/
    if (x<0)
    {
      --p0;
      x+=1.0f;
    }
    if (y<0)
    {
      p0-=DY;
      y+=1.0f;
    }
    //*/
    r[p].x = (1-x)*((1-y)*d0[p0].x+y*d0[p0+DY].x)+x*((1-y)*d0[p0+1].x+y*d0[p0+DY+1].x);
    r[p].y = (1-x)*((1-y)*d0[p0].y+y*d0[p0+DY].y)+x*((1-y)*d0[p0+1].y+y*d0[p0+DY+1].y);
  }
  )
}

void Turbulent::project(VGrid& vel) // uses the 3 scalar temp grids
{
  scalar f = -1.0f/(getNx()+getNy());
  {
    float* pp = stemp1.writeInnerBegin();
    const Vec2D* v = vel.readAll();
    FOR_LOCAL_CELLS(scalar, stemp2,
    {
      r[p] = f*(v[p+1].x-v[p-1].x+v[p+DY].y-v[p-DY].y);
      pp[p] = r[p]*0.25f;
    }
    );
    const float* div = stemp2.readAll();
    FOR_GHOST_CELLS(pp[p] = div[p]*0.25f);
    stemp1.writeInnerEnd();
  }
  //lin_solve_step_s ( pp, div, pp, 1, 4 );
  lin_solve_step_s ( stemp3, stemp2, stemp1, 1, 4 );
  lin_solve_step_s ( stemp1, stemp2, stemp3, 1, 4 );
  lin_solve_step_s ( stemp3, stemp2, stemp1, 1, 4 );
  lin_solve_step_s ( stemp1, stemp2, stemp3, 1, 4 );
  
  f = (1.0f+getNx()+getNy())/2;
  
  const float* pp = stemp1.readAll();
  FOR_LOCAL_CELLS(Vec2D,vel,
  {
    r[p].x -= f*(pp[p+1]-pp[p-1]);
    r[p].y -= f*(pp[p+DY]-pp[p-DY]);
  }
  );
}

int Turbulent::getLocalVelocity(float* buf) // buf must be 2*lnx*lny big
{
  const Vec2D* src = velocity.readLocal();
  src+=DY+1;
  for (int j=1;j<=NY;j++)
  {
    memcpy(buf,src,NX*2*sizeof(float));
    buf+=2*NX;
    src+=DY;
  }
  return 0;
}

int Turbulent::getLocalDensity(float* buf) // buf must be 2*lnx*lny big
{
  const Vec2D* src = density.readLocal();
  src+=DY+1;
  for (int j=1;j<=NY;j++)
  {
    memcpy(buf,src,NX*2*sizeof(float));
    buf+=2*NX;
    src+=DY;
  }
  return 0;
}


int Turbulent::getLocalVelocity(unsigned char* buf) // buf must be 2*lnx*lny big
{
  const Vec2D* src = velocity.readLocal();
  src+=DY+1;
  for (int j=1;j<=NY;j++)
  {
    unsigned char* bend = buf+2*NX;
    while (buf < bend)
    {
      buf[0] = (unsigned char)(src->x*256);
      buf[1] = (unsigned char)(src->y*256);
      src++;
      buf+=2;
    }
    src+=DY-NX;
  }
  return 0;
}

int Turbulent::getLocalDensity(unsigned char* buf) // buf must be 2*lnx*lny big
{
  const Vec2D* src = density.readLocal();
  src+=DY+1;
  for (int j=1;j<=NY;j++)
  {
    unsigned char* bend = buf+2*NX;
    while (buf < bend)
    {
      buf[0] = (unsigned char)(src->x*256);
      buf[1] = (unsigned char)(src->y*256);
      src++;
      buf+=2;
    }
    src+=DY-NX;
  }
  return 0;
}

