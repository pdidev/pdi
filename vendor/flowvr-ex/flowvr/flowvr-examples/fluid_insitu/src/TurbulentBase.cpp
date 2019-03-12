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
* File: ./TurbulentBase.cpp                                       *
*                                                                 *
* Contacts:                                                       *
*  2001-2004  Jeremie Allard <Jeremie.Allard@imag.fr>             *
*                                                                 *
******************************************************************/
#include "fluid/std.h"
#define FLUID_PERIODIC 0
#include "fluid/Turbulent.h"

GridPart::GridPart(int px, int py, int nx, int ny)
{
  P[0]=px; P[1]=py;
  N[0]=nx; N[1]=ny;
}

GridDistribution::GridDistribution(int nx, int ny)
  : np(1), lp(0), local(1,1,nx,ny), inner(1,1,nx,ny), store(0,0,nx+2,ny+2)
  , size_read(nx*ny), size_write((nx+2)*(ny+2))
  , columnScalar (MPI_DATATYPE_NULL), columnVector (MPI_DATATYPE_NULL)
{
  N[0]=nx; N[1]=ny;
  np_dir[0] = np_dir[1] = 1;
  coworkers[0][0] = coworkers[0][1] = coworkers[1][0] = coworkers[1][1] = -1;
}

bool GridDistribution::init()
{
  int d;
  MPI_Comm_size(MPI_COMM_WORLD,&np);
  MPI_Comm_rank(MPI_COMM_WORLD,&lp);
  if (np<=1) return false; // sequential computations.

  // Find the distribution scheme with the least overhead (computed as the number of values exchanged on borders)

  // Splitting vertically is always possible (provided NY >= np )
  np_dir[0] = 1;
  np_dir[1] = np;
  int best_cost = (np-1)*N[0];
  for (int npx = 2 ; npx <= np ; npx++)
  {
    int npy =  np/npx;
    if (npx*npy!=np) continue; // np not divisible by npx
    int cost = (npx-1)*N[1]+(npy-1)*N[0];
    if (cost<best_cost)
    {
      np_dir[0] = npx;
      np_dir[1] = npy;
      best_cost = cost;
    }
  }

  std::cout << "Best distribution: "<<np_dir[0]<<"x"<<np_dir[1]<<std::endl;

  int ppos[2];
  ppos[0] = lp % np_dir[0];
  ppos[1] = (lp / np_dir[0]) % np_dir[1];

  int sd = 1;
  // Compute local dimensions
  for (d=0;d<2;d++)
  {
    local.P[d] = 1+(   ppos[d] *N[d])/np_dir[d];
    local.N[d] = 1+((1+ppos[d])*N[d])/np_dir[d] - local.P[d];
    store.P[d] = local.P[d]-1;
    store.N[d] = local.N[d]+2;
    if (ppos[d]==0)
    {
      inner.P[d] = local.P[d];
      coworkers[d][0] = -1;
    }
    else
    {
      inner.P[d] = store.P[d];
      coworkers[d][0] = lp-sd;
    }
    if (ppos[d]==np_dir[d]-1)
    {
      inner.N[d] = local.P[d]+local.N[d]-inner.P[d];
      coworkers[d][1] = -1;
    }
    else
    {
      inner.N[d] = store.P[d]+store.N[d]-inner.P[d];
      coworkers[d][1] = lp+sd;;
    }
    sd *= np_dir[d];
  }
  size_read = local.N[0]*local.N[1];
  size_write = store.N[0]*store.N[1];

  std::cout << "Local Grid: <"<<local.P[0]<<','<<local.P[1]<<"> size <"<<local.N[0]<<','<<local.N[1]<<">"<<std::endl;
  std::cout << "Coworkers:";
  for (d=0;d<2;d++)
  {
    if (coworkers[d][0]>=0) std::cout << ' '<<coworkers[d][0];
    if (coworkers[d][1]>=0) std::cout << ' '<<coworkers[d][1];
  }
  std::cout << std::endl;

  MPI_Type_vector(local.N[1],1,store.N[0],MPI_FLOAT,&columnScalar);
  MPI_Type_commit(&columnScalar);
  MPI_Type_vector(local.N[1],2,store.N[0]*2,MPI_FLOAT,&columnVector);
  MPI_Type_commit(&columnVector);

  return false;
}

GridDistribution::~GridDistribution()
{
  if (columnScalar != MPI_DATATYPE_NULL)
    MPI_Type_free(&columnScalar);
  if (columnVector != MPI_DATATYPE_NULL)
    MPI_Type_free(&columnVector);
}

GridPart GridDistribution::part2local(const GridPart& part)
{
  GridPart ploc(part.P[0]-store.P[0],part.P[1]-store.P[1],part.N[0],part.N[1]);
  for (int d=0;d<2;d++)
  {
    if (ploc.P[d]<1) { ploc.N[d] -= 1-ploc.P[d]; ploc.P[d] = 1; }
    if (ploc.P[d]+ploc.N[d]>1+local.N[d]) { ploc.N[d] = 1+local.N[d]-ploc.P[d]; }
    if (ploc.N[d]<0) ploc.N[d]=0;
  }
  return ploc;
}

GridPart GridDistribution::part2inner(const GridPart& part)
{
  GridPart p2 = part;
  for (int d=0;d<2;d++)
  {
    if (p2.P[d]==0) { ++p2.P[d]; --p2.N[d]; }
    if (p2.P[d]+p2.N[d]==N[d]+2) { --p2.N[d]; }
  }
  return part2store(p2);
}

GridPart GridDistribution::part2store(const GridPart& part)
{
  GridPart psto(part.P[0]-store.P[0],part.P[1]-store.P[1],part.N[0],part.N[1]);
  for (int d=0;d<2;d++)
  {
    if (psto.P[d]<0) { psto.N[d] -= -psto.P[d]; psto.P[d] = 0; }
    if (psto.P[d]+psto.N[d]>store.N[d]) { psto.N[d] = store.N[d]-psto.P[d]; }
    if (psto.N[d]<0) psto.N[d]=0;
  }
  return psto;
}


BaseDistGrid::BaseDistGrid()
  : distrib(NULL),
    nrequest(0),
    inComm(false)
{
}

BaseDistGrid::~BaseDistGrid()
{
}

bool BaseDistGrid::init(GridDistribution* dist, void* data
			, MPI_Datatype celltype, int cellcount
			, MPI_Datatype columntype, int cellsize
			)
{
  distrib = dist;
  nrequest = 0;
  int lnx = dist->getLNx();
  int lny = dist->getLNy();

  // Using a checkboard pattern half of the processes must first send then receive data while the other half do the opposite.
  // As on a checkboard black cells are only connected to white cells we can add our coordinate and use the parity as indicator.

  int coordsum = (dist->lp % dist->np_dir[0]) + ((dist->lp / dist->np_dir[0]) % dist->np_dir[1]) + (dist->lp / (dist->np_dir[0]*dist->np_dir[1]));

  for (int step=0;step<2;step++)
  {
    if (step^(coordsum&1))
    {
      if (dist->coworkers[1][0]>=0)
	MPI_Recv_init((char*)data+cellsize*dist->local2pos(1,0), lnx*cellcount, celltype, dist->coworkers[1][0], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[1][1]>=0)
	MPI_Recv_init((char*)data+cellsize*dist->local2pos(1,lny+1), lnx*cellcount, celltype, dist->coworkers[1][1], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[0][0]>=0)
	MPI_Recv_init((char*)data+cellsize*dist->local2pos(0,1), 1, columntype, dist->coworkers[0][0], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[0][1]>=0)
	MPI_Recv_init((char*)data+cellsize*dist->local2pos(lnx+1,1), 1, columntype, dist->coworkers[0][1], 0, MPI_COMM_WORLD, comms+nrequest++);
    }
    else
    {
      if (dist->coworkers[1][1]>=0)
	MPI_Send_init((char*)data+cellsize*dist->local2pos(1,lny), lnx*cellcount, celltype, dist->coworkers[1][1], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[1][0]>=0)
	MPI_Send_init((char*)data+cellsize*dist->local2pos(1,1), lnx*cellcount, celltype, dist->coworkers[1][0], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[0][1]>=0)
	MPI_Send_init((char*)data+cellsize*dist->local2pos(lnx,1), 1, columntype, dist->coworkers[0][1], 0, MPI_COMM_WORLD, comms+nrequest++);
      if (dist->coworkers[0][0]>=0)
	MPI_Send_init((char*)data+cellsize*dist->local2pos(1,1), 1, columntype, dist->coworkers[0][0], 0, MPI_COMM_WORLD, comms+nrequest++);
    }
  }
  return false;
}

void BaseDistGrid::startComm()
{
  MPI_Startall(nrequest, comms);
  inComm = true;
  writeOuter();
  writeCorners();
}

void BaseDistGrid::stopComm()
{
  if (!inComm) return;
  MPI_Waitall(nrequest, comms, MPI_STATUSES_IGNORE);
  writeCorners();
  inComm = false;
}


void SGrid::writeCorners()
{
  const int lsize = distrib->getLineSize();
  int p;
  p = distrib->local2pos(0,0); data[p] = (data[p+1]+data[p+lsize])*0.5f;
  p = distrib->local2pos(distrib->getLNx()+1, 0); data[p] = (data[p-1]+data[p+lsize])*0.5f;
  p = distrib->local2pos(0,distrib->getLNy()+1); data[p] = (data[p+1]+data[p-lsize])*0.5f;
  p = distrib->local2pos(distrib->getLNx()+1, distrib->getLNy()+1); data[p] = (data[p-1]+data[p-lsize])*0.5f;
}

void SGrid::writeOuter()
{
  const int lsize = distrib->getLineSize();
  int p,p1;
  if (distrib->coworkers[0][0]<0)
  { // left ghost must be written
    p = distrib->local2pos(0,1);
    p1 = distrib->local2pos(0,distrib->getLNy());
    for (;p<=p1;p+=lsize) 
    {
      data[p]=data[p+1];
    }
  }
  if (distrib->coworkers[0][1]<0)
  { // right ghost must be written
    p = distrib->local2pos(distrib->getLNx()+1,1);
    p1 = distrib->local2pos(distrib->getLNx()+1,distrib->getLNy());
    for (;p<=p1;p+=lsize)
    {
      data[p]=data[p-1];
    }
  }
  if (distrib->coworkers[1][0]<0)
  { // top ghost must be written
    p = distrib->local2pos(1,0);
    p1 = distrib->local2pos(distrib->getLNx(),0);
    for (;p<=p1;p++)
    {
      data[p]=data[p+lsize];
    }
  }
  if (distrib->coworkers[1][1]<0)
  { // bottom ghost must be written
    p = distrib->local2pos(1,distrib->getLNy()+1);
    p1 = distrib->local2pos(distrib->getLNx(),distrib->getLNy()+1);
    for (;p<=p1;p++)
    {
      data[p]=data[p-lsize];
    }
  }
}

void VGrid::writeCorners()
{
  int lsize = distrib->getLineSize();
  int p;
  p = distrib->local2pos(0,0);
    data[p].x = (data[p+1].x+data[p+lsize].x)*0.5f;
    data[p].y = (data[p+1].y+data[p+lsize].y)*0.5f;
  p = distrib->local2pos(distrib->getLNx()+1, 0);
    data[p].x = (data[p-1].x+data[p+lsize].x)*0.5f;
    data[p].y = (data[p-1].y+data[p+lsize].y)*0.5f;
  p = distrib->local2pos(0,distrib->getLNy()+1);
    data[p].x = (data[p+1].x+data[p-lsize].x)*0.5f;
    data[p].y = (data[p+1].y+data[p-lsize].y)*0.5f;
  p = distrib->local2pos(distrib->getLNx()+1,distrib->getLNy()+1);
    data[p].x = (data[p-1].x+data[p-lsize].x)*0.5f;
    data[p].y = (data[p-1].y+data[p-lsize].y)*0.5f;
}

void VGrid::writeOuter()
{
  int lsize = distrib->getLineSize();
  int p,p1;
  if (distrib->coworkers[0][0]<0)
  { // left ghost must be written
    p = distrib->local2pos(0,1);
    p1 = distrib->local2pos(0,distrib->getLNy());
    for (;p<=p1;p+=lsize)
    {
      data[p].x=-data[p+1].x;
      data[p].y= data[p+1].y;
    }
  }
  if (distrib->coworkers[0][1]<0)
  { // right ghost must be written
    p = distrib->local2pos(distrib->getLNx()+1,1);
    p1 = distrib->local2pos(distrib->getLNx()+1,distrib->getLNy());
    for (;p<=p1;p+=lsize)
    {
      data[p].x=-data[p-1].x;
      data[p].y= data[p-1].y;
    }
  }
  if (distrib->coworkers[1][0]<0)
  { // top ghost must be written
    p = distrib->local2pos(1,0);
    p1 = distrib->local2pos(distrib->getLNx(),0);
    for (;p<=p1;p++)
    {
      data[p].x= data[p+lsize].x;
      data[p].y=-data[p+lsize].y;
    }
  }
  if (distrib->coworkers[1][1]<0)
  { // bottom ghost must be written
    p = distrib->local2pos(1,distrib->getLNy()+1);
    p1 = distrib->local2pos(distrib->getLNx(),distrib->getLNy()+1);
    for (;p<=p1;p++)
    {
      data[p].x= data[p-lsize].x;
      data[p].y=-data[p-lsize].y;
    }
  }
}

