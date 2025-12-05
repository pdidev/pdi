#include "Grid.h"

#include <mpi.h>
#include <exception>
#include <iostream>
#include <iterator>

Grid::Grid(const unsigned int num_points[3], const double spacing[3])
{
	if (num_points[0] == 0 || num_points[1] == 0 || num_points[2] == 0) {
		throw std::runtime_error("Must have a non-zero amount of points in each direction.");
	}
	// in parallel, we do a simple partitioning in the x-direction.
	int mpi_size = 1;
	int mpi_rank = 0;
	MPI_Comm_rank(MPI_COMM_WORLD, &mpi_rank);
	MPI_Comm_size(MPI_COMM_WORLD, &mpi_size);

	unsigned int start_x_point = mpi_rank * num_points[0] / mpi_size;
	unsigned int end_x_point = (mpi_rank + 1) * num_points[0] / mpi_size;
	if (mpi_size != mpi_rank + 1) {
		end_x_point++;
	}

	// create the points -- slowest in the x and fastest in the z directions
	double coord[3] = {0, 0, 0};
	for (unsigned int i = start_x_point; i < end_x_point; i++) {
		coord[0] = i * spacing[0];
		for (unsigned int j = 0; j < num_points[1]; j++) {
			coord[1] = j * spacing[1];
			for (unsigned int k = 0; k < num_points[2]; k++) {
				coord[2] = k * spacing[2];
				// add the coordinate to the end of the vector
				std::copy(coord, coord + 3, std::back_inserter(this->m_points));
			}
		}
	}
	// create the hex cells
	unsigned int cell_points[8];
	unsigned int numXPoints = end_x_point - start_x_point;
	for (unsigned int i = 0; i < numXPoints - 1; i++) {
		for (unsigned int j = 0; j < num_points[1] - 1; j++) {
			for (unsigned int k = 0; k < num_points[2] - 1; k++) {
				cell_points[0] = i * num_points[1] * num_points[2] + j * num_points[2] + k;
				cell_points[1] = (i + 1) * num_points[1] * num_points[2] + j * num_points[2] + k;
				cell_points[2] = (i + 1) * num_points[1] * num_points[2] + (j + 1) * num_points[2] + k;
				cell_points[3] = i * num_points[1] * num_points[2] + (j + 1) * num_points[2] + k;
				cell_points[4] = i * num_points[1] * num_points[2] + j * num_points[2] + k + 1;
				cell_points[5] = (i + 1) * num_points[1] * num_points[2] + j * num_points[2] + k + 1;
				cell_points[6] = (i + 1) * num_points[1] * num_points[2] + (j + 1) * num_points[2] + k + 1;
				cell_points[7] = i * num_points[1] * num_points[2] + (j + 1) * num_points[2] + k + 1;
				std::copy(cell_points, cell_points + 8, std::back_inserter(this->m_cells));
			}
		}
	}
}

size_t Grid::get_number_of_points() const
{
	return this->m_points.size() / 3;
}

size_t Grid::get_number_of_cells() const
{
	return this->m_cells.size() / 8;
}

const double* Grid::get_points_array() const
{
	if (this->m_points.empty()) {
		return nullptr;
	}
	return this->m_points.data();
}

const double* Grid::get_point(size_t pointId) const
{
	if (pointId >= this->m_points.size()) {
		return nullptr;
	}
	return &(this->m_points[pointId * 3]);
}

const unsigned int* Grid::get_cell_points(size_t cellId) const
{
	if (cellId >= this->m_cells.size()) {
		return nullptr;
	}
	return &(this->m_cells[cellId * 8]);
}
