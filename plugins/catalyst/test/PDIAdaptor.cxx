#include "PDIAdaptor.h"

#include "Attributes.h"
#include "Grid.h"

#include <pdi.h>
#include <string>
#include <iostream>

namespace PDIAdaptor
{

bool Initialize(const std::string& pdi_yaml_config_file_path, const Grid& grid)
{
  PC_tree_t conf = PC_parse_path(pdi_yaml_config_file_path.c_str());
  auto status = PDI_init(PC_get(conf, ""));
  if (status != PDI_status_t::PDI_OK)
  {
    return false;
  }

  auto points_array_size = grid.GetNumberOfPoints() * 3;
  status = PDI_expose("points_array_size", &points_array_size, PDI_OUT);
  if (status != PDI_status_t::PDI_OK)
  {
    return false;
  }

  auto number_of_cells = grid.GetNumberOfCells() * 8;
  status = PDI_expose("cell_points_size", &number_of_cells, PDI_OUT);
  if (status != PDI_status_t::PDI_OK)
  {
    return false;
  }

  auto velocity_array_size = grid.GetNumberOfPoints() * 3;
  status = PDI_expose("velocity_array_size", &velocity_array_size, PDI_OUT);
  if (status != PDI_status_t::PDI_OK)
  {
    return false;
  }

  auto pressure_array_size = grid.GetNumberOfCells();
  status = PDI_expose("pressure_array_size", &pressure_array_size, PDI_OUT);
  if (status != PDI_status_t::PDI_OK)
  {
    return false;
  }

  return true;
}

bool Execute(int cycle, double time, Grid& grid, Attributes& attribs)
{
  auto number_of_points = grid.GetNumberOfPoints();
  auto number_of_cells = grid.GetNumberOfCells();

  std::cout << "#### begin false_catalyst_execute ####" << std::endl;

  auto status_false = PDI_multi_expose(
    //
    "false_catalyst_execute",
    //
    "number_of_cells", &number_of_cells, PDI_OUT,
    //
    "cell_points", grid.GetCellPoints(0), PDI_OUT,
    //
    "velocity_array", attribs.GetVelocityArray(), PDI_OUT,
    //
    "pressure_array", attribs.GetPressureArray(), PDI_OUT,
    //
    NULL);

  auto status = PDI_multi_expose(
    //
    "catalyst_execute",
    //
    "cycle", &cycle, PDI_OUT,
    //
    "time", &time, PDI_OUT,
    //
    "number_of_points", &number_of_points, PDI_OUT,
    //
    "points_array", grid.GetPointsArray(), PDI_OUT,
    //
    "number_of_cells", &number_of_cells, PDI_OUT,
    //
    "cell_points", grid.GetCellPoints(0), PDI_OUT,
    //
    "velocity_array", attribs.GetVelocityArray(), PDI_OUT,
    //
    "pressure_array", attribs.GetPressureArray(), PDI_OUT,
    //
    NULL);

  return status == PDI_status_t::PDI_OK;
}

bool Finalize()
{
  auto status = PDI_finalize();
  return status == PDI_status_t::PDI_OK;
}
}
