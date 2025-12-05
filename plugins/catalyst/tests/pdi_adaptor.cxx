#include "pdi_adaptor.h"

#include "attributes.h"
#include "grid.h"

#include <iostream>
#include <string>
#include <pdi.h>

namespace pdi_adaptor {

bool initialize(const std::string& pdi_yaml_config_file_path, const Grid& grid)
{
	PC_tree_t conf = PC_parse_path(pdi_yaml_config_file_path.c_str());
	auto status = PDI_init(PC_get(conf, ""));
	if (status != PDI_status_t::PDI_OK) {
		return false;
	}

	auto points_array_size = grid.get_number_of_points() * 3;
	status = PDI_expose("points_array_size", &points_array_size, PDI_OUT);
	if (status != PDI_status_t::PDI_OK) {
		return false;
	}

	auto number_of_cells = grid.get_number_of_cells() * 8;
	status = PDI_expose("cell_points_size", &number_of_cells, PDI_OUT);
	if (status != PDI_status_t::PDI_OK) {
		return false;
	}

	auto velocity_array_size = grid.get_number_of_points() * 3;
	status = PDI_expose("velocity_array_size", &velocity_array_size, PDI_OUT);
	if (status != PDI_status_t::PDI_OK) {
		return false;
	}

	auto pressure_array_size = grid.get_number_of_cells();
	status = PDI_expose("pressure_array_size", &pressure_array_size, PDI_OUT);
	if (status != PDI_status_t::PDI_OK) {
		return false;
	}

	return true;
}

bool execute(int cycle, double time, Grid& grid, Attributes& attribs)
{
	auto number_of_points = grid.get_number_of_points();
	auto number_of_cells = grid.get_number_of_cells();

	auto status = PDI_multi_expose(
		//
		"catalyst_execute",
		//
		"cycle",
		&cycle,
		PDI_OUT,
		//
		"time",
		&time,
		PDI_OUT,
		//
		"number_of_points",
		&number_of_points,
		PDI_OUT,
		//
		"points_array",
		grid.get_points_array(),
		PDI_OUT,
		//
		"number_of_cells",
		&number_of_cells,
		PDI_OUT,
		//
		"cell_points",
		grid.get_cell_points(0),
		PDI_OUT,
		//
		"velocity_array",
		attribs.get_velocity_array(),
		PDI_OUT,
		//
		"pressure_array",
		attribs.get_pressure_array(),
		PDI_OUT,
		//
		NULL
	);

	return status == PDI_status_t::PDI_OK;
}

bool finalize()
{
	auto status = PDI_finalize();
	return status == PDI_status_t::PDI_OK;
}
} // namespace PDI_adaptor
