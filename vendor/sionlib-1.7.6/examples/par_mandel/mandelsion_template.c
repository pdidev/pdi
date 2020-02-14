#include "mandelsion.h"
#include "infostruct.h"
#include "sion.h"

/* write block to SION file */
void write_to_sion_file(int *sid, const _infostruct *infostruct, const int *iterations, int width, int height, int xpos,
                        int ypos)
{
}

/* open SION file */
void open_sion(int *sid, const _infostruct *infostruct, const int *blocksize, const int *start, int rank)
{
}

/* close SION file */
void close_sion(int *sid, const _infostruct *infostruct, int rank)
{
}

/* read SION file */
void collect_sion(int **iterations, int **proc_distribution, _infostruct *infostruct)
{
}
