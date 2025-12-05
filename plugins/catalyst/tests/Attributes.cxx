#include "Attributes.h"

#include "Grid.h"

Attributes::Attributes(Grid* grid)
{
	this->m_grid_ptr = grid;
}

Attributes::~Attributes()
{
	this->m_grid_ptr = nullptr;
}

void Attributes::update_fields(double time)
{
	size_t num_points = this->m_grid_ptr->get_number_of_points();
	this->m_velocity.resize(num_points * 3);
	for (size_t pt = 0; pt < num_points; pt++) {
		const double* coord = this->m_grid_ptr->get_point(pt);
		this->m_velocity[pt] = coord[1] * time;
	}
	std::fill(this->m_velocity.begin() + num_points, this->m_velocity.end(), 0.);

	size_t num_cells = this->m_grid_ptr->get_number_of_cells();
	this->m_pressure.resize(num_cells);

	double tmp_var = (num_cells * time * 0.5);
	size_t first_cells;
	if (tmp_var < 0) {
		first_cells = 0;
	} else {
		first_cells = (size_t)tmp_var;
	}

	std::fill(this->m_pressure.begin(), this->m_pressure.end(), -1.f);
	std::fill(this->m_pressure.begin() + first_cells, this->m_pressure.end(), 1.f);
}

double* Attributes::get_velocity_array()
{
	if (this->m_velocity.empty()) {
		return nullptr;
	}
	return &this->m_velocity[0];
}

float* Attributes::get_pressure_array()
{
	if (this->m_pressure.empty()) {
		return nullptr;
	}
	return &this->m_pressure[0];
}
