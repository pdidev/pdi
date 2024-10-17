#include "Attributes.h"
#include "Grid.h"
#include "PDIAdaptor.h"

#include <cstdlib>
#include <iostream>
#include <mpi.h>

int main(int argc, char* argv[])
{
  MPI_Init(&argc, &argv);
  Grid grid;
  unsigned int numPoints[3] = { 70, 60, 44 };
  double spacing[3] = { 1, 1.1, 1.3 };
  grid.Initialize(numPoints, spacing);
  Attributes attributes;
  attributes.Initialize(&grid);

  if (argc < 2)
  {
    std::cerr << "expecting the pdi yaml config as argument" << std::endl;
    return EXIT_FAILURE;
  }
  auto code = PDIAdaptor::Initialize(std::string(argv[1]), grid);
  if (!code)
  {
    std::cerr << "PDIAdaptor::Initialize failure" << std::endl;
    return EXIT_FAILURE;
  }

  unsigned int numberOfTimeSteps = 10;
  for (unsigned int timeStep = 0; timeStep < numberOfTimeSteps; timeStep++)
  {
    // use a time step length of 0.1
    double time = timeStep * 0.1;
    attributes.UpdateFields(time);

    code = PDIAdaptor::Execute(timeStep, time, grid, attributes);
    if (!code)
    {
      std::cerr << "PDIAdaptor::Execute failure" << std::endl;
      return EXIT_FAILURE;
    }
  }

  code = PDIAdaptor::Finalize();
  if (!code)
  {
    std::cerr << "PDIAdaptor::Finalize failure" << std::endl;
    return EXIT_FAILURE;
  }

  MPI_Finalize();
  return EXIT_SUCCESS;
}
