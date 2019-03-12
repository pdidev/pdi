#!/bin/sh

if [ -z "$1" ]; then
	echo "specify launch script to be one of those:"
	ls -1 *.prm
	exit 0;
else
	flowvr -x  \
	       -L \
	       -Q $1 \
	       --complib ${FLOWVR_PREFIX}/share/flowvr-contrib/components/libfinitequeuefiltertest.comp.so \
	       FiniteFilterQueueTest ;
fi       