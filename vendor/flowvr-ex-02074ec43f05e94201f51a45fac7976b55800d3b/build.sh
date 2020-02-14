#!/bin/sh

OVERWRITE=0

if [ -z "$1" ]; then
  BUILDDIR="BUILD"
else
  BUILDDIR=$1
fi
if [ -d ${BUILDDIR} ]; then
  while true; do
    echo "Building FlowVR in ${BUILDDIR} directory."
    read -p "Overwrite previous contents? (y/n): " yn
    case $yn in
      [Yy]* ) echo "Cleaning and launching CMake."; BUILDDIR="BUILD"; OVERWRITE=1; break;;
      [Nn]* ) echo "Launching CMake"; break;;
      * ) echo "Please answer yes or no (y/n): ";;
    esac
  done
else
  echo "Building FlowVR in $BUILDDIR directory."
  sleep 1
fi

if [ $OVERWRITE -eq 1 ]; then
  rm -rf $BUILDDIR 2> /dev/null
fi
mkdir $BUILDDIR 2> /dev/null

cd $BUILDDIR
ccmake ../. && make -j && echo "" &&  echo "Run \"make install\" in directory $BUILDDIR to install FlowVR."
