#!/bin/bash
# plot results!
server_rank_0_log=`ls server.log.* | head -n 1`
sed -n '/Timing information/,/End Timing information/p' $server_rank_0_log | sed -e '$ d; 1,2 d' | gnuplot -e 'set terminal dumb; set datafile separator comma; set yrange [*:*]; plot "/dev/stdin" u 1:2'

