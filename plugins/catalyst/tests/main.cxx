#include "Attributes.h"
#include "Grid.h"
#include "PDIAdaptor.h"

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
	auto code = PDI_adaptor::initialize(std::string(argv[1]), grid);
	if (!code) {
		std::cerr << "PDIAdaptor::Initialize failure" << std::endl;
		return EXIT_FAILURE;
	}

	unsigned int number_of_time_steps = 10;
	for (unsigned int time_step = 0; time_step < number_of_time_steps; time_step++) {
		// use a time step length of 0.1
		double time = time_step * 0.1;
		attributes.update_fields(time);

		code = PDI_adaptor::execute(time_step, time, grid, attributes);
		if (!code) {
			std::cerr << "PDIAdaptor::Execute failure" << std::endl;
			return EXIT_FAILURE;
		}
	}

	code = PDI_adaptor::finalize();
	if (!code) {
		std::cerr << "PDIAdaptor::Finalize failure" << std::endl;
		return EXIT_FAILURE;
	}

	MPI_Finalize();
	return EXIT_SUCCESS;
}
