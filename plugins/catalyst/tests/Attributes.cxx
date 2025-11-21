#include "Attributes.h"

#include "Grid.h"

Attributes::Attributes(Grid* grid)
{
  this->GridPtr = grid;
}

Attributes::~Attributes()
{
  this->GridPtr = nullptr;
}

void Attributes::UpdateFields(double time)
{
  size_t numPoints = this->GridPtr->GetNumberOfPoints();
  this->Velocity.resize(numPoints * 3);
  for (size_t pt = 0; pt < numPoints; pt++)
  {
    const double* coord = this->GridPtr->GetPoint(pt);
    this->Velocity[pt] = coord[1] * time;
  }
  std::fill(this->Velocity.begin() + numPoints, this->Velocity.end(), 0.);

  size_t numCells = this->GridPtr->GetNumberOfCells();
  this->Pressure.resize(numCells);

  double tmp_var = (numCells * time * 0.5);
  size_t first_cells;
  if (tmp_var < 0)
  {
    first_cells = 0;
  }
  else
  {
    first_cells = (size_t)tmp_var;
  }

  std::fill(this->Pressure.begin(), this->Pressure.end(), -1.f);
  std::fill(this->Pressure.begin() + first_cells, this->Pressure.end(), 1.f);
}

double* Attributes::GetVelocityArray()
{
  if (this->Velocity.empty())
  {
    return nullptr;
  }
  return &this->Velocity[0];
}

float* Attributes::GetPressureArray()
{
  if (this->Pressure.empty())
  {
    return nullptr;
  }
  return &this->Pressure[0];
}
