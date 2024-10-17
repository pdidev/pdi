#ifndef GRID_H
#define GRID_H

#include <cstddef>
#include <vector>

class Grid
{
public:
  Grid();
  void Initialize(const unsigned int numPoints[3], const double spacing[3]);
  size_t GetNumberOfPoints();
  size_t GetNumberOfCells();
  double* GetPointsArray();
  double* GetPoint(size_t pointId);
  unsigned int* GetCellPoints(size_t cellId);

private:
  std::vector<double> Points;
  std::vector<unsigned int> Cells;
};

#endif
