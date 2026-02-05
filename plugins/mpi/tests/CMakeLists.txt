# SPDX-FileCopyrightText: 2015-2026 Commissariat a l'energie atomique et aux energies alternatives (CEA)
# SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
#
# SPDX-License-Identifier: BSD-3-Clause

cmake_minimum_required(VERSION 3.22...4.2)

set(RUNTEST_DIR "${CMAKE_CURRENT_LIST_DIR}/../cmake/runtest-dir")

# Add the plugin path to PDI_PLUGIN_PATH
set_property(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}" PROPERTY TEST_INCLUDE_FILE "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake")
file(GENERATE OUTPUT "${CMAKE_CURRENT_BINARY_DIR}/TestPath.cmake"
     CONTENT "
set(PDI_PLUGIN_PATH \"\$ENV{PDI_PLUGIN_PATH}\")\n
if(\"x\${PDI_PLUGIN_PATH}x\" STREQUAL xx)\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_mpi_plugin>\")\n
else()\n
set(ENV{PDI_PLUGIN_PATH} \"\$<TARGET_FILE_DIR:pdi_mpi_plugin>:\${PDI_PLUGIN_PATH}\")\n
endif()
"
)

if("${BUILD_FORTRAN}")
	if(NOT "${MPI_Fortran_HAVE_F90_MODULE}")
		if("${MPI_Fortran_HAVE_F77_HEADER}")
			file(WRITE "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90" [=[
module mpi
implicit none
include "mpif.h"
end module
]=])
			add_library(MPI_with_mod STATIC "${CMAKE_CURRENT_BINARY_DIR}/mpi.F90")
			target_link_libraries(MPI_with_mod MPI::MPI_Fortran)
		else()
			message(FATAL_ERROR "Unable to compile a MPI program either with F90 module or F77 include")
		endif()
	else()
		add_library(MPI_with_mod INTERFACE)
		target_link_libraries(MPI_with_mod INTERFACE MPI::MPI_Fortran)
	endif()
endif()


# MPI plugin test
add_executable(mpi_01_C mpi_test_01.c)
target_link_libraries(mpi_01_C PDI::PDI_C MPI::MPI_C)
add_test(NAME mpi_01_C COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:mpi_01_C>" ${MPIEXEC_POSTFLAGS})
set_property(TEST mpi_01_C PROPERTY PROCESSORS 1)

# MPI plugin C predefines test
add_executable(mpi_02_C mpi_test_02.c)
target_link_libraries(mpi_02_C PDI::PDI_C MPI::MPI_C)
add_test(NAME mpi_02_C COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:mpi_02_C>" ${MPIEXEC_POSTFLAGS})
set_property(TEST mpi_02_C PROPERTY PROCESSORS 1)

# MPI plugin Fortran predefines test
if("${BUILD_FORTRAN}")
add_executable(mpi_02_F mpi_test_02.F90)
target_link_libraries(mpi_02_F PDI::PDI_f90 MPI_with_mod)
string(RANDOM TMP_MODULE_DIRECTORY)
set_target_properties(mpi_02_F PROPERTIES Fortran_MODULE_DIRECTORY "${TMP_MODULE_DIRECTORY}")
add_test(NAME mpi_02_F COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:mpi_02_F>" ${MPIEXEC_POSTFLAGS} "${CMAKE_CURRENT_SOURCE_DIR}/mpi_test_02.yml")
set_property(TEST mpi_02_F PROPERTY PROCESSORS 1)
endif("${BUILD_FORTRAN}")

# MPI plugin C predefines test
add_executable(mpi_03_C mpi_test_03.c)
target_link_libraries(mpi_03_C PDI::PDI_C MPI::MPI_C)
add_test(NAME mpi_03_C COMMAND "${MPIEXEC}" "${MPIEXEC_NUMPROC_FLAG}" 1 ${MPIEXEC_PREFLAGS} "$<TARGET_FILE:mpi_03_C>" ${MPIEXEC_POSTFLAGS})
set_property(TEST mpi_03_C PROPERTY PROCESSORS 1)
