cmake_minimum_required(VERSION 3.22...4.2)
project(pdi_checkpoint_plugin LANGUAGES C CXX)

include(CTest)
include(GNUInstallDirs)

find_package(spdlog 1.9 REQUIRED)
find_package(PDI REQUIRED plugins)

# HDF5: mirror decl_hdf5 logic as much as possible
set(HDF5_USE_STATIC_LIBRARIES OFF)
# If you want parallel, also set:
# set(HDF5_PREFER_PARALLEL ON)
find_package(HDF5 REQUIRED COMPONENTS C)

# Prefer imported target rather than ${HDF5_LIBRARIES}
set(HDF5_DEPS hdf5::hdf5)

# If you need parallel HDF5 and MPI like decl_hdf5:
if(HDF5_IS_PARALLEL)
  find_package(MPI REQUIRED COMPONENTS C CXX)
  list(APPEND HDF5_DEPS MPI::MPI_C MPI::MPI_CXX)
endif()

add_library(pdi_checkpoint_plugin MODULE
    checkpoint.cxx
    ../decl_hdf5/file_op.cxx
    ../decl_hdf5/attribute_op.cxx
    ../decl_hdf5/dataset_op.cxx
    ../decl_hdf5/decl_hdf5.cxx
    ../decl_hdf5/collision_policy.cxx
    ../decl_hdf5/hdf5_wrapper.cxx
    ../decl_hdf5/selection.cxx)

target_include_directories(pdi_checkpoint_plugin PUBLIC
    ../decl_hdf5
    ${HDF5_INCLUDE_DIRS}
)

target_link_libraries(pdi_checkpoint_plugin
    PUBLIC
      PDI::PDI_plugins
      ${HDF5_DEPS}
)

set_target_properties(pdi_checkpoint_plugin PROPERTIES
    CXX_VISIBILITY_PRESET hidden)

set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_checkpoint_plugin
    LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}")
