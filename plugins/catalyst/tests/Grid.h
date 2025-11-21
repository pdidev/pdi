#ifndef GRID_H
#define GRID_H

#include <cstddef>
#include <vector>

class Grid
{
public:
  Grid(const unsigned int numPoints[3], const double spacing[3]);
  size_t GetNumberOfPoints() const;
  size_t GetNumberOfCells() const;
  const double* GetPointsArray() const;
  const double* GetPoint(size_t pointId) const;
  const unsigned int* GetCellPoints(size_t cellId) const;

private:
  std::vector<double> Points;
  std::vector<unsigned int> Cells;
};

#endif
