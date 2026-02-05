// SPDX-FileCopyrightText: 2025-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
//
// SPDX-License-Identifier: BSD-3-Clause

#include "test_api.h"

#include <iostream>

int main(int argc, char* argv[])
{
	tests(argc, argv);
	std::cout << "Disabled PDI ok for C++." << std::endl;
}
