#ifndef ATTRIBUTES_H
#define ATTRIBUTES_H

#include <vector>
class Grid;

class Attributes
{
	// A class for generating and storing point and cell fields.
	// Velocity is stored at the points and pressure is stored
	// for the cells. The current velocity profile is for a
	// shearing flow with U(y,t) = y*t, V = 0 and W = 0.
	// Pressure is constant through the domain.
public:
	Attributes(Grid* grid);
	~Attributes();
	void update_fields(double time);
	double* get_velocity_array();
	float* get_pressure_array();

private:
	std::vector<double> m_velocity;
	std::vector<float> m_pressure;
	Grid* m_grid_ptr;
};

#endif // ATTRIBUTES_H
