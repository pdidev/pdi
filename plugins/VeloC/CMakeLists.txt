cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_veloc_plugin LANGUAGES C CXX)

include(CTest)
include(GNUInstallDirs)

find_package(spdlog 1.9 REQUIRED)
find_package(PDI REQUIRED plugins)


# Let users control via environment/standard CMake vars
list(APPEND CMAKE_PREFIX_PATH "${HOME}/.local/share/veloc/cmake")
find_package(veloc REQUIRED)

add_library(pdi_veloc_plugin MODULE
		veloc_wrapper.cxx)

# Then link with imported target (like HDF5 did):
target_link_libraries(pdi_veloc_plugin 
    PUBLIC 
    PDI::PDI_plugins 
    veloc::client          
    MPI::MPI_C 
    MPI::MPI_CXX
)
target_include_directories(pdi_veloc_plugin PRIVATE ${veloc_INCLUDE_DIRS})

target_compile_options(pdi_veloc_plugin PRIVATE ${veloc_CFLAGS_OTHER})

set_target_properties(pdi_veloc_plugin PROPERTIES
    CXX_VISIBILITY_PRESET hidden)

set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_veloc_plugin
    LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}")
