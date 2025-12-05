#include "attributes.h"
#include "grid.h"
#include "pdi_adaptor.h"

#include <mpi.h>
#include <cstdlib>
#include <iostream>

int main(int argc, char* argv[])
{
	MPI_Init(&argc, &argv);
	unsigned int num_points[3] = {70, 60, 44};
	double spacing[3] = {1, 1.1, 1.3};
	Grid grid(num_points, spacing);
	Attributes attributes(&grid);

	if (argc < 2) {
		std::cerr << "expecting the pdi yaml config as argument" << std::endl;
		return EXIT_FAILURE;
	}
	auto code = pdi_adaptor::initialize(std::string(argv[1]), grid);
	if (!code) {
		std::cerr << "pdi_adaptor::Initialize failure" << std::endl;
		return EXIT_FAILURE;
	}

	unsigned int number_of_time_steps = 10;
	for (unsigned int time_step = 0; time_step < number_of_time_steps; time_step++) {
		// use a time step length of 0.1
		double time = time_step * 0.1;
		attributes.update_fields(time);

		code = pdi_adaptor::execute(time_step, time, grid, attributes);
		if (!code) {
			std::cerr << "pdi_adaptor::Execute failure" << std::endl;
			return EXIT_FAILURE;
		}
	}

	code = pdi_adaptor::finalize();
	if (!code) {
		std::cerr << "pdi_adaptor::Finalize failure" << std::endl;
		return EXIT_FAILURE;
	}

	MPI_Finalize();
	return EXIT_SUCCESS;
}
