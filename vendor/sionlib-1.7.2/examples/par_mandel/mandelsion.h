#ifndef MANDELSION_H
#define MANDELSION_H

#include "infostruct.h"

void write_to_sion_file(int *sid, const _infostruct *infostruct, const int *iterations,
                        int width, int height, int xpos, int ypos);

void open_sion(int *sid, const _infostruct *infostruct, const int *blocksize, const int *start, int rank);

void close_sion(int *sid, const _infostruct *infostruct, int rank);

void collect_sion(int **iterations, int **proc_distribution, _infostruct *infostruct);

#endif
