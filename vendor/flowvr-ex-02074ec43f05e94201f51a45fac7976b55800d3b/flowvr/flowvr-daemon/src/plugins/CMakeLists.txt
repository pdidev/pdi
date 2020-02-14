INCLUDE(CheckFunctionExists)
INCLUDE(CheckSymbolExists)

# plugins
SET(plugins-sys-SRC flowvr.plugins.DefaultDispatcher.cpp
		    flowvr.plugins.DefaultLogger.cpp
		    flowvr.plugins.AutoCommander.cpp)
SET(plugins-sync-SRC flowvr.plugins.GreedySynchronizor.cpp
                     flowvr.plugins.VariableFrequencySynchronizor.cpp
		     flowvr.plugins.MaxFrequencySynchronizor.cpp)
SET(plugins-filters-SRC flowvr.plugins.FilterIt.cpp
			flowvr.plugins.Max.cpp
			flowvr.plugins.MergeIt.cpp
			flowvr.plugins.MergeMsg.cpp
			flowvr.plugins.Merge2D.cpp
			flowvr.plugins.Merge.cpp
			flowvr.plugins.MergeIndexPos.cpp
			flowvr.plugins.MinIt.cpp
			flowvr.plugins.MaxBuffer.cpp
			flowvr.plugins.It2Start.cpp
			flowvr.plugins.Scatter.cpp
			flowvr.plugins.SignalAnd.cpp
			flowvr.plugins.PreSignal.cpp
			flowvr.plugins.FrequencyFilter.cpp
			flowvr.plugins.RotateFilter.cpp
			flowvr.plugins.UnrotateFilter.cpp
			flowvr.plugins.FilterRank.cpp
			flowvr.plugins.MultiplexFilter.cpp
			flowvr.plugins.Stop.cpp
			flowvr.plugins.Discard.cpp
			flowvr.plugins.MaskScatter.cpp
			flowvr.plugins.MaskRouting.cpp
			flowvr.plugins.MaskMerge.cpp
			flowvr.plugins.UnfilterIt.cpp
			flowvr.plugins.FilterLastOrNull.cpp
			flowvr.plugins.FilterRenum.cpp
			flowvr.plugins.FilterIncrement.cpp
			flowvr.plugins.FilterFiniteQueue.cpp)
			
#SET(LIBRARY_OUTPUT_PATH ${CMAKE_BINARY_DIR}/lib/flowvr/plugins)

FOREACH(type sys sync filters)
  FOREACH(plugin-src ${plugins-${type}-SRC})
    STRING(REPLACE ".cpp" "" plugin ${plugin-src})
    ADD_LIBRARY(${plugin} MODULE ${type}/${plugin-src})
    SET_TARGET_PROPERTIES(${plugin} PROPERTIES PREFIX "")
    TARGET_LINK_LIBRARIES(${plugin} flowvr-base flowvr-plugd)
    INSTALL(TARGETS ${plugin} DESTINATION lib/flowvr/plugins)
  ENDFOREACH(plugin-src)
ENDFOREACH(type)

# MPI
IF(BUILD_FLOWVRD_MPI_PLUGIN)
  # MPI_THREAD_FUNNELED plugin
  MESSAGE( STATUS "Building MPI_THREAD_FUNNELED module" )
  INCLUDE_DIRECTORIES(${MPI_CXX_INCLUDE_PATH})
  ADD_LIBRARY(flowvr.plugins.NetMPI MODULE sys/flowvr.plugins.NetMPI.cpp)
  SET_TARGET_PROPERTIES(flowvr.plugins.NetMPI PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(flowvr.plugins.NetMPI ${MPI_CXX_LIBRARIES} flowvr-base flowvr-plugd)
  INSTALL(TARGETS flowvr.plugins.NetMPI DESTINATION lib/flowvr/plugins)
  # MPI_THREAD_MULTIPLE plugin
  MESSAGE( STATUS "Building MPI_THREAD_MULTIPLE module" )
  INCLUDE_DIRECTORIES(${MPI_CXX_INCLUDE_PATH})
  ADD_LIBRARY(flowvr.plugins.NetMPIm MODULE sys/flowvr.plugins.NetMPIm.cpp)
  SET_TARGET_PROPERTIES(flowvr.plugins.NetMPIm PROPERTIES PREFIX "")
  TARGET_LINK_LIBRARIES(flowvr.plugins.NetMPIm ${MPI_CXX_LIBRARIES} flowvr-base flowvr-plugd)
  INSTALL(TARGETS flowvr.plugins.NetMPIm DESTINATION lib/flowvr/plugins)
ENDIF(BUILD_FLOWVRD_MPI_PLUGIN)

ADD_LIBRARY(flowvr.plugins.NetTCP MODULE 
               sys/flowvr.plugins.NetTCP.cpp )

SET_TARGET_PROPERTIES(flowvr.plugins.NetTCP PROPERTIES PREFIX "")
TARGET_LINK_LIBRARIES(flowvr.plugins.NetTCP 
                      flowvr-base 
                      flowvr-plugd )
INSTALL(TARGETS flowvr.plugins.NetTCP DESTINATION lib/flowvr/plugins)
