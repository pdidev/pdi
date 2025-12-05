#ifndef GRID_H
#define GRID_H

#include <cstddef>
#include <vector>

class Grid
{
public:
	Grid(const unsigned int num_points[3], const double spacing[3]);
	size_t get_number_of_points() const;
	size_t get_number_of_cells() const;
	const double* get_points_array() const;
	const double* get_point(size_t pointId) const;
	const unsigned int* get_cell_points(size_t cellId) const;

private:
	std::vector<double> m_points;
	std::vector<unsigned int> m_cells;
};

#endif
