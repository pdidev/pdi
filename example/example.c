#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define VAL2D(arr, xx, yy) arr[(xx)+width*(yy)]

void init(double* dat, int width, int height, int px, int py)
{
	int xx, yy;
	for (yy=0; yy<height; ++yy) {
		for(xx=0; xx<width; ++xx) {
			VAL2D(dat,xx,yy) = 0;
		}
	}
	if ( px == 0 ) {
		for (yy=0; yy<height; ++yy) {
			VAL2D(dat,0,yy) = 100;
		}
	}
}

void iter(double* cur, double* next, int width, int height)
{
	int xx, yy;
	for (yy=1; yy<height-1; ++yy) {
		for(xx=1; xx<width-1; ++xx) {
			VAL2D(next,xx,yy) =
			VAL2D(cur,xx,yy) / 2
			+ (VAL2D(cur,xx-1,yy) / 8)
			+ (VAL2D(cur,xx+1,yy) / 8)
			+ (VAL2D(cur,xx,yy-1) / 8)
			+ (VAL2D(cur,xx,yy+1) / 8);
		}
	}
}

int main(int argc, char *argv[])
{
	int nb_iter, rank, rank_source, rank_dest, size, pheight, pwidth, height, width, ii;
	int cart_dims[2], cart_period[2], car_coord[2];
	MPI_Comm cart_com;
	double *cur, *next, *tmp;
	paraconf_t *conf, *pdi_conf;
	MPI_Datatype column, row;
	MPI_Status status;

	MPI_Init(&argc, &argv);
	conf = PC_load(MPI_COMM_WORLD, "example.yml", &argc, &argv);

	pdi_conf = PC_get(conf, "pdi")
	PDI_init(MPI_COMM_WORLD, pdi_conf);

	PDI_Event("initialization");

	nb_iter = PC_get_int(conf, "iter");
	height = PC_get_int(conf, "datasize[0]");
	width = PC_get_int(conf, "datasize[1]");
	pheight = PC_get_int(conf, "parallelism/height");
	pwidth = PC_get_int(conf, "parallelism/width");

	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	assert(pwidth*pheight == size);
	cart_dims[0] = pwidth;
	cart_dims[1] = pheight;
	cart_period[0]=0; cart_period[1]=0;
	MPI_Cart_create(MPI_COMM_WORLD, 2, cart_dims, cart_period, 1, &cart_com);
	MPI_Cart_coords(comm, rank, 2, car_coord);

	PDI_val(car_coord, "coord");
	PDI_val(&height, "height");
	PDI_val(&width, "width");

	cur = malloc(sizeof(double)*width*height);
	next = malloc(sizeof(double)*width*height);

	init(cur, width, height, car_coord[0], car_coord[1]);

	MPI_Type_vector(height, 1, width, MPI_Double, &column);
	MPI_Type_contiguous(width, MPI_Double, &row);

	PDI_Event("main_loop");
	for(ii=0; ii<nb_iter; ++ii) {
		if ( ii == (iter-1) ) PDI_Event("last_iter");
		PDI_val(&ii, "iter");
		PDI_val(cur, "main_field");
		iter(cur, next, width, height);

		/* send to the right */
		MPI_Cart_shift(cart_com, 0, 1, &rank_source, &rank_dest);
		MPI_Sendrecv(&VAL2D(cur,  width-2, 0), 1, column, rank_dest,   100, /* send column before ghost */
		             &VAL2D(next, 0,       0), 1, column, rank_source, 100, /* receive 1st column (ghost) */
		             cart_com, &status);

		/* send to the left */
		MPI_Cart_shift(cart_com, 0, -1, &rank_source, &rank_dest);
		MPI_Sendrecv(&VAL2D(cur,  1,       0), 1, column, rank_dest,   100, /* send column after ghost */
		             &VAL2D(next, width-1, 0), 1, column, rank_source, 100, /* receive last column (ghost) */
		             cart_com, &status);

		/* send down */
		MPI_Cart_shift(cart_com, 1, 1, &rank_source, &rank_dest);
		MPI_Sendrecv(&VAL2D(cur,  0, height-2), 1, row, rank_dest,   100, /* send row before ghost */
		             &VAL2D(next, 0, 0       ), 1, row, rank_source, 100, /* receive 1st row (ghost) */
		             cart_com, &status);

		/* send up */
		MPI_Cart_shift(cart_com, 1, -1, &rank_source, &rank_dest);
		MPI_Sendrecv(&VAL2D(cur,  0, 1       ), 1, row, rank_dest,   100, /* send column after ghost */
								 &VAL2D(next, 0, height-1), 1, row, rank_source, 100, /* receive last column (ghost) */
		             cart_com, &status);

		/* switch current & next fields */
		tmp = cur; cur = next; next = tmp;
	}

	PDI_Event("finalization");
	free(cur);
	free(next);

	MPI_Finalize();
	return 0;
}
