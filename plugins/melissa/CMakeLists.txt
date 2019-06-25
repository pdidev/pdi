cmake_minimum_required(VERSION 3.5)
project(pdi_melissa_plugin LANGUAGES CXX)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_CURRENT_LIST_DIR}/cmake")

# Includes
include(GNUInstallDirs)

# Astyle
find_package(Astyle)

# PDI
find_package(PDI REQUIRED)

# Melissa
find_package(Melissa REQUIRED)

# MPI
set(MPI_COMPONENTS CXX)
find_package(MPI REQUIRED)

# The plugin
add_library(pdi_melissa_plugin MODULE melissa.cxx)
target_link_libraries(pdi_melissa_plugin PUBLIC PDI::pdi MPI::MPI_CXX)

target_link_libraries(pdi_melissa_plugin PUBLIC ${Melissa_LIBRARY})
target_include_directories(pdi_melissa_plugin PUBLIC ${Melissa_INCLUDE_DIR})

set_target_properties(pdi_melissa_plugin PROPERTIES
		CXX_STANDARD 11
		CXX_STANDARD_REQUIRED TRUE
		CXX_VISIBILITY_PRESET hidden)

# Indentation
if("${Astyle_FOUND}")
	Astyle_add_indent(indent TEST
			OPTIONS_FILE ../../docs/formating.astyle
			melissa.cxx)
endif()

# Installation
install(TARGETS pdi_melissa_plugin
		LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
		ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
)

# Configuration validation
if ("${BUILD_CFG_VALIDATOR}")
	set(Python_ADDITIONAL_VERSIONS 3.7 3.6 3.5 3.4)
	find_package(PythonPath REQUIRED)
	install(DIRECTORY "${CMAKE_CURRENT_SOURCE_DIR}/pdicfg_validator"
			DESTINATION "${PythonPath_INSTALL_SITELIBDIR}/")
endif()
