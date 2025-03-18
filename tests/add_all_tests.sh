#!/bin/bash
cd tests_no-pdi
rm -rf CMakeCache.txt CMakeFiles/ cmake_install.cmake && cmake .
cd ../tests_pdi
rm -rf CMakeCache.txt CMakeFiles/ cmake_install.cmake && cmake .
cd ..
