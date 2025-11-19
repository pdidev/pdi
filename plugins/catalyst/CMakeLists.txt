cmake_minimum_required(VERSION 3.13)
project(pdi_catalyst_plugin LANGUAGES C CXX)

find_package(PDI REQUIRED COMPONENTS plugins)
find_package(catalyst REQUIRED)
if(NOT ${CATALYST_USE_MPI})
    message(FATAL_ERROR "No MPI support in your Catalyst library, please activate MPI in your Catalyst build.")
endif()

add_library(pdi_catalyst_plugin MODULE pdi_catalyst_plugin.h pdi_catalyst_plugin.cxx)
target_link_libraries(pdi_catalyst_plugin PDI::PDI_plugins catalyst::catalyst)

# installation
set(INSTALL_PDIPLUGINDIR "${PDI_DEFAULT_PLUGINDIR}" CACHE PATH "PDI plugins (${PDI_DEFAULT_PLUGINDIR})")
install(TARGETS pdi_catalyst_plugin
                LIBRARY DESTINATION "${INSTALL_PDIPLUGINDIR}"
)

if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()
