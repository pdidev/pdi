#include "Grid.h"

#include <exception>
#include <iostream>
#include <iterator>
#include <mpi.h>

Grid::Grid(const unsigned int numPoints[3], const double spacing[3])
{
  if (numPoints[0] == 0 || numPoints[1] == 0 || numPoints[2] == 0)
  {
    throw std::runtime_error("Must have a non-zero amount of points in each direction.");
  }
  // in parallel, we do a simple partitioning in the x-direction.
  int mpiSize = 1;
  int mpiRank = 0;
  MPI_Comm_rank(MPI_COMM_WORLD, &mpiRank);
  MPI_Comm_size(MPI_COMM_WORLD, &mpiSize);

  unsigned int startXPoint = mpiRank * numPoints[0] / mpiSize;
  unsigned int endXPoint = (mpiRank + 1) * numPoints[0] / mpiSize;
  if (mpiSize != mpiRank + 1)
  {
    endXPoint++;
  }

  // create the points -- slowest in the x and fastest in the z directions
  double coord[3] = { 0, 0, 0 };
  for (unsigned int i = startXPoint; i < endXPoint; i++)
  {
    coord[0] = i * spacing[0];
    for (unsigned int j = 0; j < numPoints[1]; j++)
    {
      coord[1] = j * spacing[1];
      for (unsigned int k = 0; k < numPoints[2]; k++)
      {
        coord[2] = k * spacing[2];
        // add the coordinate to the end of the vector
        std::copy(coord, coord + 3, std::back_inserter(this->Points));
      }
    }
  }
  // create the hex cells
  unsigned int cellPoints[8];
  unsigned int numXPoints = endXPoint - startXPoint;
  for (unsigned int i = 0; i < numXPoints - 1; i++)
  {
    for (unsigned int j = 0; j < numPoints[1] - 1; j++)
    {
      for (unsigned int k = 0; k < numPoints[2] - 1; k++)
      {
        cellPoints[0] = i * numPoints[1] * numPoints[2] + j * numPoints[2] + k;
        cellPoints[1] = (i + 1) * numPoints[1] * numPoints[2] + j * numPoints[2] + k;
        cellPoints[2] = (i + 1) * numPoints[1] * numPoints[2] + (j + 1) * numPoints[2] + k;
        cellPoints[3] = i * numPoints[1] * numPoints[2] + (j + 1) * numPoints[2] + k;
        cellPoints[4] = i * numPoints[1] * numPoints[2] + j * numPoints[2] + k + 1;
        cellPoints[5] = (i + 1) * numPoints[1] * numPoints[2] + j * numPoints[2] + k + 1;
        cellPoints[6] = (i + 1) * numPoints[1] * numPoints[2] + (j + 1) * numPoints[2] + k + 1;
        cellPoints[7] = i * numPoints[1] * numPoints[2] + (j + 1) * numPoints[2] + k + 1;
        std::copy(cellPoints, cellPoints + 8, std::back_inserter(this->Cells));
      }
    }
  }
}

size_t Grid::GetNumberOfPoints() const
{
  return this->Points.size() / 3;
}

size_t Grid::GetNumberOfCells() const
{
  return this->Cells.size() / 8;
}

const double* Grid::GetPointsArray() const
{
  if (this->Points.empty())
  {
    return nullptr;
  }
  return this->Points.data();
}

const double* Grid::GetPoint(size_t pointId) const
{
  if (pointId >= this->Points.size())
  {
    return nullptr;
  }
  return &(this->Points[pointId * 3]);
}

const unsigned int* Grid::GetCellPoints(size_t cellId) const
{
  if (cellId >= this->Cells.size())
  {
    return nullptr;
  }
  return &(this->Cells[cellId * 8]);
}
