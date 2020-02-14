/*
 * mandelmpi.c
 *
 */
#include <stdlib.h>
#include <stdio.h>
#include <mpi.h>

#include "infostruct.h"
#include "mandelmpi.h"
#include "mandelsion.h"

/* master-worker communication flags */
#define STATE_MASTER -1
#define STATE_IDLE   0
#define STATE_WORK   1
#define TAG_WORK     27
#define TAG_DONE     28
#define TAG_DATA     29
#define TAG_FINISH   30

/* describe usage */
static void usage(char* name)
{
  fprintf(stderr, "Usage: %s options\n\nwith the following optional options (default values in parenthesis):\n\n", name);
  fprintf(stderr, "  [-x <x0> <x1> <y0> <y1>]  coordinates of initial area (-1.5 0.5 -1.0 1.0)\n");
  fprintf(stderr, "  [-w <width>]              image width in pixels (256)\n");
  fprintf(stderr, "  [-h <height>]             image height in pixels (256)\n");
  fprintf(stderr, "  [-i <maxiter>]            max. number of iterations per pixel (256)\n");
  fprintf(stderr, "  [-b <blocksize>]          blocksize used for strides and blockmaster\n");
  fprintf(stderr, "  [-p <xprocs>]             #xprocs for type = blocks\n");
  fprintf(stderr, "  [-q <yprocs>]             #yprocs for type = blocks\n");
  fprintf(stderr, "  [-t <type>]               0=stride, 1=static, 2=blockmaster, 3=single writer\n");
  fprintf(stderr, "  [-v]                      verbose (off)\n\n");
}

int main(int argc, char *argv[])
{
  double xmin = -1.5; /* coordinates of rectangle */
  double xmax = 0.5;
  double ymin = -1.0;
  double ymax = 1.0;
  int width = 256; /* size of rectangle in pixels */
  int height = 256;
  int maxiter = 256; /* max. number of iterations */
  int verbose = 0; /* per default only print error messages */
  int type = 0; /* type of calculation: 0=stride, 1=static, 2=blockmaster */
  int blocksize[2] = {64,64}; /* personal procs blocksize */
  int start[2] = {0,0}; /* personal start index (type = 1) */
  int (*blocksizes)[2] = NULL; /* all procs blocksizes (type = 1) */
  int (*starts)[2] = NULL; /* all procs starts (type = 1) */
  int procs[2] = {0,0}; /* procs distribution (type = 1) */

  int *iterations; /* data array */
  int check_ok = 1; /* size check */

  int numprocs, myid; /* MPI information */
  int i;
  MPI_Datatype master_worker_commtype;

  /* time measurement */
  double st, sta, runtime, calctime = 0.0, commtime = 0.0, waittime = 0.0, iotime = 0.0;

  double lxmin, lxmax, dx;
  double lymin, lymax, dy;
  int array_size;
  int io_id;

  _infostruct infostruct;

  /* parse command line */
  i = 1;
  while (i < argc) {
    if (argv[i][0] == '-') {
      switch (argv[i][1]) {
        case 'x':
          xmin = atof(argv[++i]);
          xmax = atof(argv[++i]);
          ymin = atof(argv[++i]);
          ymax = atof(argv[++i]);
          break;
        case 'i':
          maxiter = atoi(argv[++i]);
          break;
        case 'w':
          width = atoi(argv[++i]);
          break;
        case 'h':
          height = atoi(argv[++i]);
          break;
        case 't':
          type = atoi(argv[++i]);
          break;
        case 'b':
          blocksize[0] = atoi(argv[++i]);
          blocksize[1] = blocksize[0];
          break;
        case 'v':
          verbose++;
          break;
        case 'p':
          procs[0] = atoi(argv[++i]);
          break;
        case 'q':
          procs[1] = atoi(argv[++i]);
          break;
        default:
          fprintf(stderr, "%s\n", argv[i]);
          usage(argv[0]);
          exit(1);
      }
    } else {
      fprintf(stderr, "%s\n", argv[i]);
      usage(argv[0]);
      exit(1);
    }
    i++;
  }

  /* start MPI */
  MPI_Init(&argc, &argv);
  MPI_Comm_size(MPI_COMM_WORLD, &numprocs);
  MPI_Comm_rank(MPI_COMM_WORLD, &myid);

  /* type = 1 calculate blocksizes */
  if (type == 0) {
    blocksize[1] = width;
    start[0] = myid * blocksize[0];
  } else if (type == 1) {
    MPI_Dims_create(numprocs,2,procs);
    blocksizes = (int (*)[2]) malloc(numprocs*sizeof(int[2]));
    starts = (int (*)[2]) malloc(numprocs*sizeof(int[2]));
    starts[0][0] = 0;
    starts[0][1] = 0;
    for (i=0; i<numprocs; ++i) {
      /* calculate blocksizes */
      blocksizes[i][1] = width / procs[0];
      if (i % procs[0] < width % procs[0]) {
        blocksizes[i][1]++;
      }
      blocksizes[i][0] = height / procs[1];
      if (i % procs[1] < height % procs[1]) {
        blocksizes[i][0]++;
      }
      /* calculate startpoints */
      if (i > 0) {
        starts[i][1] = starts[i-1][1] + blocksizes[i-1][1];
        if (starts[i][1] >= width) {
          starts[i][1] = 0;
          starts[i][0] = starts[i-1][0] + blocksizes[i-1][0];
        } else
          starts[i][0] = starts[i-1][0];
      }
    }
    blocksize[0] = blocksizes[myid][0];
    blocksize[1] = blocksizes[myid][1];
    start[0] = starts[myid][0];
    start[1] = starts[myid][1];
    free(blocksizes);
    free(starts);
  }

  /* check sizes */
  if (type != 1) {
    check_ok = height % blocksize[0] == 0;
    if (check_ok) {
      check_ok = width % blocksize[1] == 0;
    }
  }
  if (!check_ok && myid == 0) {
    fprintf(stderr, "Blocksize, width and height are not compatible!\n");
    exit(1);
  }

  /* fill infostruct */
  infostruct.type = type;
  infostruct.width = width;
  infostruct.height = height;
  infostruct.numprocs = numprocs;
  infostruct.xmin = xmin;
  infostruct.xmax = xmax;
  infostruct.ymin = ymin;
  infostruct.ymax = ymax;
  infostruct.maxiter = maxiter;

  /* print info header */
  if (verbose && myid == 0) {
    printf("start calculation (x=%8.5g ..%8.5g,y=%10.7g ..%10.7g)\n", xmin, xmax, ymin, ymax);
    fflush(stdout);
  }

  sta = MPI_Wtime();

  /* IO preparation */
  array_size = blocksize[0] * blocksize[1];
  if (type == 3 && myid == 0) {
    array_size = width * height; /* proc 0 will collect all data */
  }

  /* initialize array */
  iterations = calloc(array_size, sizeof(int));

  st = MPI_Wtime();
  MPI_Barrier(MPI_COMM_WORLD);
  waittime += (MPI_Wtime() - st);

  /* open file */
  open_file(&io_id, &infostruct, blocksize, start, myid, &iotime);

  /**************************** Type 0 Stride *******************************/

  if (type == 0) {
    dy = (ymax - ymin) / height;
    if (verbose) {
      printf("calc_stride[%02d]: %dx%d\n", myid, blocksize[1], blocksize[0]);
    }
    for (; start[0] < height; start[0] += numprocs * blocksize[0]) {
      lymin = ymin + start[0] * dy;
      lymax = lymin + blocksize[0] * dy;
      calc(iterations, blocksize[1], blocksize[0], xmin, xmax, lymin, lymax, maxiter, &calctime);

      write_to_file(&io_id, &infostruct, iterations, blocksize[1], blocksize[0], start[1], start[0], &iotime);
    }
    /* call write routine using empty dataset to allow collectiv operations */
    if (((height / blocksize[0]) % numprocs > 0) && (myid >= (height / blocksize[0]) % numprocs)) {
      write_to_file(&io_id, &infostruct, iterations, 0, 0, start[1], start[0], &iotime);
    }
  }

  /**************************** Type 1 Static *******************************/

  if (type == 1) {
    /* Calculate blocks */
    dx = (xmax - xmin) / width;
    dy = (ymax - ymin) / height;
    lxmin = xmin + start[1] * dx;
    lxmax = lxmin + blocksize[1] * dx;
    lymin = ymin + start[0] * dy;
    lymax = lymin + blocksize[0] * dy;

    /* Calculation */
    if (verbose) {
      printf("calc_static[%02d]: %dx%d\n", myid, blocksize[1], blocksize[0]);
    }
    calc(iterations, blocksize[1], blocksize[0], lxmin, lxmax, lymin, lymax, maxiter, &calctime);

    /* additional barrier because of collective write */
    st = MPI_Wtime();
    MPI_Barrier(MPI_COMM_WORLD);
    waittime += (MPI_Wtime() - st);

    write_to_file(&io_id, &infostruct, iterations, blocksize[1], blocksize[0], start[1], start[0], &iotime);
  }

  /**************************** Type 2 Blockmaster **************************/

  if (type == 2 || type == 3) {
    if (numprocs > 1) {
      MPI_Type_vector(blocksize[1], blocksize[0], width, MPI_INT, &master_worker_commtype);
      MPI_Type_commit(&master_worker_commtype);
      if (myid == 0) {
        if (verbose) {
          printf("calc_master[%02d]: %dx%d\n", myid, width, height);
        }
        calc_master(iterations, width, height, numprocs, blocksize, type == 3, &calctime, &master_worker_commtype,
                    &commtime);
        if (type == 3) {
          write_to_file(&io_id, &infostruct, iterations, width, height, 0, 0, &iotime);
        }
      } else {
        if (verbose) {
          printf("calc_worker[%02d]: %dx%d\n", myid, blocksize[1], blocksize[0]);
        }
        calc_worker(iterations, &io_id, &infostruct, type == 3, &iotime, &calctime, &commtime);
      }
      MPI_Type_free(&master_worker_commtype);
    } else {
      fprintf(stderr, "ERROR: type 'blockmaster' needs at least two processes\n");
    }
  }

  /**************************************************************************/

  st = MPI_Wtime();
  MPI_Barrier(MPI_COMM_WORLD);
  waittime += (MPI_Wtime() - st);

  /* close file */
  close_file(&io_id, &infostruct, myid, &iotime);

  runtime = MPI_Wtime() - sta;

  if (verbose) {
    printf(
        "PE %02d of %02d: t= %1d %d x %d calc= %9.3f, wait= %9.3f, io= %9.3f, mpi= %9.3f, runtime= %9.3f (ms)\n",
        myid, numprocs, type, width, height, calctime * 1000, waittime * 1000, iotime * 1000,
        commtime * 1000, runtime * 1000);
  }

  free(iterations);

  MPI_Finalize();

  return EXIT_SUCCESS;
}

/* master task (type=2 || type=3) */
void calc_master(int *iterations, int width, int height, int numprocs, int *blocksize, int collect, double *calctime,
                 MPI_Datatype *commtype, double *commtime)
{
  double st, stc;
  int ix, iy, i;
  int lwidth, lheight;
  int workeratwork = 0, numworkers = numprocs - 1;
  int *workerstat;
  int work[4];
  MPI_Status status, status_data;

  *commtime = 0.0;

  workerstat = malloc(numprocs * sizeof(int));

  workerstat[0] = STATE_MASTER;
  for (i = 1; i < numprocs; i++) {
    workerstat[i] = STATE_IDLE;
  }

  stc = MPI_Wtime();
  ix = 0;
  iy = 0;
  i = 0;
  while ((iy < height) && (ix < width)) {

    /* calculate blocksize */
    lwidth = blocksize[1];
    lheight = blocksize[0];
    if (ix + lwidth > width) {
      lwidth = width - ix;
    }
    if (iy + lheight > height) {
      lheight = height - iy;
    }

    if (workeratwork < numworkers) {
      while (workerstat[i] != STATE_IDLE) {
        i = (i + 1) % numprocs;
      }
      /*  send work to worker #i */
      work[0] = ix;
      work[1] = iy;
      work[2] = lwidth;
      work[3] = lheight;
      st = MPI_Wtime();
      MPI_Send(work, 4, MPI_INT, i, TAG_WORK, MPI_COMM_WORLD);
      *commtime += MPI_Wtime() - st;
      workerstat[i] = STATE_WORK;
      workeratwork++;

      /* calculate location of next block */
      ix += lwidth;
      if (ix >= width) {
        ix = 0;
        iy += lheight;
      }
    } else {
      /*  collect result msg */
      st = MPI_Wtime();
      MPI_Recv(work, 4, MPI_INT, MPI_ANY_SOURCE, TAG_DONE, MPI_COMM_WORLD, &status);
      if (collect) {
        MPI_Recv(&iterations[work[1] * width + work[0]], 1, *commtype, status.MPI_SOURCE, TAG_DATA, MPI_COMM_WORLD,
                 &status_data);
      }
      *commtime += MPI_Wtime() - st;
      workerstat[status.MPI_SOURCE] = STATE_IDLE;
      workeratwork--;
    }
  }
  /*  get rest of result msg */
  while (workeratwork > 0) {
    st = MPI_Wtime();
    MPI_Recv(work, 4, MPI_INT, MPI_ANY_SOURCE, TAG_DONE, MPI_COMM_WORLD, &status);
    if (collect) {
      MPI_Recv(&iterations[work[1] * width + work[0]], 1, *commtype, status.MPI_SOURCE, TAG_DATA, MPI_COMM_WORLD,
               &status_data);
    }
    *commtime += MPI_Wtime() - st;
    workerstat[status.MPI_SOURCE] = STATE_IDLE;
    workeratwork--;
  }

  /*  send finish message */
  for (i = 1; i < numprocs; i++) {
    work[0] = -1;
    work[1] = -1;
    work[2] = -1;
    work[3] = -1;
    st = MPI_Wtime();
    MPI_Send(work, 4, MPI_INT, i, TAG_FINISH, MPI_COMM_WORLD);
    *commtime += MPI_Wtime() - st;
  }
  *calctime += MPI_Wtime() - stc - *commtime;

  free(workerstat);
}

/* worker task (type=2 || type=3) */
void calc_worker(int* iterations, int *io_id, _infostruct *infostruct, int collect, double *iotime, double* calctime,
                 double* commtime)
{
  double dx, dy, st;
  int xpos, ypos, lwidth, lheight;
  double lxmin, lxmax, lymin, lymax;
  int work[4];
  MPI_Status status;

  dx = (infostruct->xmax - infostruct->xmin) / infostruct->width;
  dy = (infostruct->ymax - infostruct->ymin) / infostruct->height;

  while (1) {
    st = MPI_Wtime();
    /* recv new work from master */
    MPI_Recv(work, 4, MPI_INT, 0, MPI_ANY_TAG, MPI_COMM_WORLD, &status);
    *commtime += MPI_Wtime() - st;
    if (status.MPI_TAG == TAG_FINISH) {
      return;
    }

    xpos = work[0];
    ypos = work[1];
    lwidth = work[2];
    lheight = work[3];

    lxmin = infostruct->xmin + xpos * dx;
    lxmax = lxmin + lwidth * dx;
    lymin = infostruct->ymin + ypos * dy;
    lymax = lymin + lheight * dy;

    calc(iterations, lwidth, lheight, lxmin, lxmax, lymin, lymax, infostruct->maxiter, calctime);

    if (!collect) {
      write_to_file(io_id, infostruct, iterations, lwidth, lheight, xpos, ypos, iotime);
    }

    st = MPI_Wtime();
    MPI_Send(work, 4, MPI_INT, 0, TAG_DONE, MPI_COMM_WORLD);
    if (collect) {
      MPI_Send(iterations, lwidth * lheight, MPI_INT, 0, TAG_DATA, MPI_COMM_WORLD);
    }
    *commtime += MPI_Wtime() - st;
  }
}

/* Mandelbrot calculation */
void calc(int *iterations, int width, int height, double xmin, double xmax, double ymin, double ymax, int maxiter,
          double *calctime)
{
  double x, y;
  double dx, dy;
  int ix, iy;
  double st;

  st = MPI_Wtime();
  dx = (xmax - xmin) / width;
  dy = (ymax - ymin) / height;

  /* calculate value in the center of the pixel */
  y = ymin + 0.5 * dy;
  for (iy = 0; iy < height; ++iy) {
    x = xmin + 0.5 * dx;
    for (ix = 0; ix < width; ++ix) {
      double zx = 0.0, zy = 0.0, zxnew;
      int count = 0;
      while (zx * zx + zy * zy < 16 * 16 && count < maxiter) {
        zxnew = zx * zx - zy * zy + x;
        zy = 2 * zx * zy + y;
        zx = zxnew;
        ++count;
      }
      iterations[iy * width + ix] = count;
      x += dx;
    }
    y += dy;
  }
  *calctime += MPI_Wtime() - st;
}

/* open file  */
void open_file(int *io_id, _infostruct *infostruct, int *blocksize, int *start,
               int myid, double *iotime)
{
  double st;

  st = MPI_Wtime();
  open_sion(io_id, infostruct, blocksize, start, myid);
  *iotime += (MPI_Wtime() - st);
}

/* write to file  */
void write_to_file(int *io_id, const _infostruct *infostruct, int *iterations,
                   int width, int height, int xpos, int ypos, double *iotime)
{
  double st;

  st = MPI_Wtime();
  write_to_sion_file(io_id, infostruct, iterations, width, height, xpos, ypos);
  *iotime += MPI_Wtime() - st;
}

/* close file */
void close_file(int *io_id, const _infostruct *infostruct, int myid, double *iotime)
{
  double st;

  st = MPI_Wtime();
  close_sion(io_id, infostruct, myid);
  *iotime += MPI_Wtime() - st;
}
