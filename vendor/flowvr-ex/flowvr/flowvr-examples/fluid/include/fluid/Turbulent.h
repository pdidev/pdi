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
* File: ./Turbulent.h                                             *
*                                                                 *
* Contacts:                                                       *
*                                                                 *
******************************************************************/
#ifndef TURBULENT_H__
#define TURBULENT_H__

typedef float scalar;

class Vec2D
{
 public:
  scalar x,y;
  
  Vec2D(scalar _x=0,scalar _y=0) : x(_x), y(_y) {}
  
  operator scalar* () { return &x; }
  
  void operator += (const Vec2D& v)
  { x+=v.x; y+=v.y; }
  
  void operator *= (scalar s)
  { x*=s; y*=s; }

  void add(const Vec2D& v, scalar s)
  { x+=v.x*s; y+=v.y*s; }

  void add1(const Vec2D& v, scalar s)
  { x+=v.x*s; y+=v.y*s;
    if (x>=1.0f) x=0.99f;
    if (y>=1.0f) y=0.99f;
  }
  
};

/// Rectangular part of a grid.
class GridPart
{
 public:
  GridPart(int px=0, int py=0, int nx=0, int ny=0);
  int P[2]; ///< Position
  int N[2]; ///< Size
};


/// Manage the distribution of a grid on N process.
///
/// Each process is assigned a 'local' area. Each time this area is
/// modified the borders values must be send to the neighbour process.
/// The received values are stored as 'ghost' cells.
/// Outer cells are values around the global area. They must be set according to boundary conditions

class GridDistribution
{
 public:

  GridDistribution(int nx, int ny);
  ~GridDistribution();

  /// Compute the distribution
  bool init();

  /// Global grid size
  int N[2];

  int np,lp; ///< nb procs, local proc
  int np_dir[2]; ///< Number of procs in each dimension
  int coworkers[2][2]; ///< neighbours procs for each dim and each side

  GridPart local; ///< LOCAL part of the array
  GridPart inner; ///< INNER part of the array (= NOT OUTER)
  GridPart store; ///< STORE part of the array
  int size_read; ///< number of cells locally readable ( = local cells + ghosts )
  int size_write; ///< number of cells locally writable ( = local cells )
  //int getProc(int x, int y);

  int getNx() const { return N[0]; }
  int getNy() const { return N[1]; }
  int getLNx() const { return local.N[0]; }
  int getLNy() const { return local.N[1]; }
  int getLPx() const { return local.P[0]; }
  int getLPy() const { return local.P[1]; }
  int getLPNx() const { return local.P[0]+local.N[0]; }
  int getLPNy() const { return local.P[1]+local.N[1]; }
  int getLineSize() const { return store.N[0]; };
  int local2pos(int x, int y) const { return x+y*getLineSize(); }
  int global2pos(int x, int y) const { return (x-store.P[0])+(y-store.P[1])*getLineSize(); }
  bool isStoreX(int x) const { return ((unsigned)(x-store.P[0])<(unsigned)store.N[0]); }
  bool isStoreY(int y) const { return ((unsigned)(y-store.P[1])<(unsigned)store.N[1]); }
  bool isStore(int x, int y) const { return isStoreX(x) && isStoreY(y); }
  bool isInnerX(int x) const { return ((unsigned)(x-inner.P[0])<(unsigned)inner.N[0]); }
  bool isInnerY(int y) const { return ((unsigned)(y-inner.P[1])<(unsigned)inner.N[1]); }
  bool isInner(int x, int y) const { return isInnerX(x) && isInnerY(y); }
  bool isLocalX(int x) const { return ((unsigned)(x-local.P[0])<(unsigned)local.N[0]); }
  bool isLocalY(int y) const { return ((unsigned)(y-local.P[1])<(unsigned)local.N[1]); }
  bool isLocal(int x, int y) const { return isLocalX(x) && isLocalY(y); }
  GridPart part2local(const GridPart& part);
  GridPart part2inner(const GridPart& part);
  GridPart part2store(const GridPart& part);
};

class BaseDistGrid
{
 public:

  BaseDistGrid();
  virtual ~BaseDistGrid();

 protected:

  /// Init buffer and border communications
  bool init(GridDistribution* dist, void* data
	    );

  void startComm();

  void stopComm();

  virtual void writeOuter()=0;
  virtual void writeCorners()=0;

  GridDistribution* distrib;
  bool inComm;
};

template <class T>
class DistGrid : public BaseDistGrid
{
 public:
  typedef T cell;
  DistGrid()
    : data(NULL)
  {
  }
  ~DistGrid()
  {
    if (data!=NULL) delete[] data;
  }

  /// Init buffer and border communications
  bool init(GridDistribution* dist
	    )
  {
    data = new T[dist->size_write];
    memset(data,0,dist->size_write*sizeof(cell));
    return BaseDistGrid::init(dist, data);
  }

  /// Request to read local values
  const T* readLocal()
  {
    return data;
  }

  /// Request to read all values (requiring any pending border comm to
  /// finish)
  const T* readAll()
  {
    stopComm();
    return data;
  }

  /// Requesting to write local values. Any pending comm is finished.
  T* writeLocalBegin()
  {
    stopComm();
    return data;
  }

  /// Finished writing to border local values. New border comm is
  /// started. Note that we are only interested to known when borders
  /// have been updated. Interior values can still be writen to.
  void writeBorderEnd()
  {
    startComm();
  }

  /// Requesting to write all (local+ghosts) values. Any pending comm
  /// is finished.
  T* writeInnerBegin()
  {
    stopComm();
    return data;
  }

  /// Finished writing to all values. No border comm is started as the
  /// ghosts are supposed to be updated.
  /// Only outer ghosts must be updated
  void writeInnerEnd()
  {
    writeOuter();
    writeCorners();
  }

  void fillRegion(const GridPart& r, T d)
  {
    if (r.N[0]<=0 || r.N[1]<=0) return;
    writeInnerBegin();

    int pos=distrib->local2pos(r.P[0],r.P[1]);
    int dy = distrib->getLineSize()-r.N[0];
    for (int y=0;y<r.N[1];y++, pos+=dy)
    {
      for (int x=0;x<r.N[0];x++, pos++)
	data[pos] = d;
    }
    writeInnerEnd();
  }

 protected:
  T* data;

};

class SGrid : public DistGrid<scalar>
{
 public:

  void writeCorners();
  void writeOuter();

};

class VGrid : public DistGrid<Vec2D>
{
 public:

  void writeCorners();
  void writeOuter();

};

class Turbulent : public GridDistribution {
public:

  Turbulent(int _nx,int _ny);

  ~Turbulent();

  int Init();

  int update();
  int updateForces();
  int updateVelocity();
  int updateDensity();

  Vec2D getCurrentPos() { return mLastMousePos; }

  void MouseMotion(Vec2D pos, int bt=0);

  void setVisco(float visco);

  int getLocalVelocity(float* buf); // buf must be 2*lnx*lny big
  int getLocalDensity(float* buf); // buf must be 2*lnx*lny big
  int getLocalVelocity(unsigned char* buf); // buf must be 2*lnx*lny big
  int getLocalDensity(unsigned char* buf); // buf must be 2*lnx*lny big

protected:

  VGrid density;
  VGrid velocity;

  VGrid vtemp1;
  SGrid stemp1,stemp2,stemp3;

  void lin_solve_step_v(VGrid& result, VGrid& ref, VGrid& prev, float a, float c);

  void lin_solve_step_s(SGrid& result, SGrid& ref, SGrid& prev, float a, float c);

  void diffuse(VGrid& result, VGrid& prev, float diff);

  void advect(VGrid& result, VGrid& prev, VGrid& vel);

  void project(VGrid& vel); // uses the 3 scalar temp grids

  enum { NDEPLMAX=10 };

  struct DeplData
  {
    int ndepl;
    int bt[NDEPLMAX];
    Vec2D pos [NDEPLMAX];
    Vec2D depl[NDEPLMAX];
  } DeplBuffer;

  void calcForce(Vec2D pos,Vec2D force,Vec2D* grid);
  void addForce(Vec2D pos,Vec2D force,Vec2D* grid);

  // mouse mouvements

  Vec2D mLastMousePos;
  bool mFirstMouseMotion;

};

#endif
