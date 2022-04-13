# function(python_test test_name)
#     #message("Source_dir: ${CMAKE_CURRENT_SOURCE_DIR}")
#     add_test(NAME ${test_name} COMMAND ${in_env} python3 -u ./tests.py ${test_name} WORKING_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR})
# endfunction()

add_subdirectory(simulation1)
# add_subdirectory(simulation2-pdaf)
# add_subdirectory(simulation3-empty)
# add_subdirectory(simulation5-python)

# if (WITH_P2P)  # does not work without P2P
#     add_subdirectory(simulation4-p2p)
#     add_subdirectory(lorenz)
# endif()
