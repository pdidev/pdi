##############################################################################
# SPDX-FileCopyrightText: 2014-2024 Centre national de la recherche scientifique (CNRS)
# SPDX-FileCopyrightText: 2014-2024 Commissariat a l'énergie atomique et aux énergies alternatives (CEA)
# SPDX-FileCopyrightText: 2014-2024 Julien Bigot <julien.bigot@cea.fr>
# SPDX-FileCopyrightText: 2014-2024 Université Paris-Saclay
# SPDX-FileCopyrightText: 2014-2024 Université de Versailles Saint-Quentin-en-Yvelines
#
# SPDX-License-Identifier: MIT
##############################################################################

cmake_policy(PUSH)
cmake_minimum_required(VERSION 3.16...3.28)

function(test_fort_type RESULT_VAR TYPE KIND)
	if(DEFINED "${RESULT_VAR}")
		return()
	endif()
	message(STATUS "Checking whether Fortran supports type ${TYPE}(KIND=${KIND})")
	set(TEST_FILE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_test_${RESULT_VAR}.f90")
	file(WRITE "${TEST_FILE}" "
program test_${RESULT_VAR}
  ${TYPE}(KIND=${KIND}):: tstvar
end program test_${RESULT_VAR}
")
	try_compile(COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${TEST_FILE}"
		OUTPUT_VARIABLE COMPILE_OUTPUT
	)
	if(COMPILE_RESULT)
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
		"${TYPE}(KIND=${KIND}) type successfully compiled with the following output:\n"
		"${COMPILE_OUTPUT}\n")
		message(STATUS "Checking whether Fortran supports type ${TYPE}(KIND=${KIND}) -- yes")
	else()
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
		"${TYPE}(KIND=${KIND}) type failed to compile with the following output:\n"
		"${COMPILE_OUTPUT}\n")
		message(STATUS "Checking whether Fortran supports type ${TYPE}(KIND=${KIND}) -- no")
	endif()
	set("${RESULT_VAR}" "${COMPILE_RESULT}" CACHE BOOL "Whether Fortran supports type ${TYPE}(KIND=${KIND})")
	mark_as_advanced("${RESULT_VAR}")
endfunction()

function(test_fort_hdf5_type RESULT_VAR TYPE KIND)
	if(DEFINED "${RESULT_VAR}")
		return()
	endif()
	find_package(HDF5)
	if(NOT "${HDF5_FOUND}")
		set("${RESULT_VAR}" "HDF5-NOTFOUND" CACHE STRING "HDF5 constant for Fortran type ${TYPE}(KIND=${KIND})")
	mark_as_advanced("${RESULT_VAR}")
		return()
	endif()
	message(STATUS "Detecting HDF5 constant for Fortran type ${TYPE}(KIND=${KIND})")
	set(TEST_FILE "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/cmake_test_${RESULT_VAR}.f90")
	if("${TYPE}" STREQUAL "INTEGER")
		math(EXPR SIZE "8*${KIND}")
		set(H5CST "H5T_STD_I${SIZE}LE")
	elseif("${TYPE}" STREQUAL "REAL")
		math(EXPR SIZE "8*${KIND}")
		set(H5CST "H5T_IEEE_F${SIZE}LE")
	else()
		set(H5CST "HDF5_CONSTANT-NOTFOUND")
	endif()
	file(WRITE "${TEST_FILE}" "
program test_${RESULT_VAR}
  use hdf5
  ${TYPE}(KIND=${KIND}):: tstvar
  integer(HID_T):: h5var
  h5var = ${H5CST}
end program test_${RESULT_VAR}
")
	try_compile(COMPILE_RESULT "${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}" "${TEST_FILE}"
		CMAKE_FLAGS
			"-DINCLUDE_DIRECTORIES=${HDF5_Fortran_INCLUDE_PATH}"
			"-DCMAKE_Fortran_FLAGS=${HDF5_Fortran_COMPILE_FLAGS}"
			"-DCMAKE_EXE_LINKER_FLAGS=${HDF5_Fortran_LINK_FLAGS}"
		LINK_LIBRARIES
			${HDF5_Fortran_LIBRARIES} ${HDF5_C_LIBRARIES}
		OUTPUT_VARIABLE COMPILE_OUTPUT
	)
	if(COMPILE_RESULT)
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeOutput.log
		"Fortran/HDF5 ${TYPE}(KIND=${KIND}) type successfully compiled with the following output:\n"
		"${COMPILE_OUTPUT}\n")
		set("${RESULT_VAR}" "${H5CST}" CACHE STRING "HDF5 constant for Fortran type ${TYPE}(KIND=${KIND})")
	else()
		file(APPEND ${CMAKE_BINARY_DIR}${CMAKE_FILES_DIRECTORY}/CMakeError.log
		"Fortran/HDF5 ${TYPE}(KIND=${KIND}) type failed to compile with the following output:\n"
		"${COMPILE_OUTPUT}\n")
		set("${RESULT_VAR}" "HDF5_CONSTANT-NOTFOUND" CACHE STRING "HDF5 constant for Fortran type ${TYPE}(KIND=${KIND})")
	endif()
	message(STATUS "Detecting HDF5 constant for Fortran type ${TYPE}(KIND=${KIND}) -- ${${RESULT_VAR}}")
	mark_as_advanced("${RESULT_VAR}")
endfunction()

cmake_policy(POP)
