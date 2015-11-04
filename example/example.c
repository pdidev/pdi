#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>
#include <mpi.h>
#include <yaml.h>
#include <paraconf.h>

#include "pdi.h"

#define VAL2D(arr, xx, yy) (arr[(xx)+width*(yy)])

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

void iter(double* cur, double* next, int height, int width)
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

void exchange(MPI_Comm cart_com, double *cur, int height, int width)
{
	MPI_Status status;
	int rank_source, rank_dest;
	static MPI_Datatype column, row;
	static int initialized = 0;
	
	if ( !initialized ) {
		MPI_Type_vector(height, 1, width, MPI_DOUBLE, &column);
		MPI_Type_commit(&column);
		MPI_Type_contiguous(width, MPI_DOUBLE, &row);
		MPI_Type_commit(&row);
		initialized = 1;
	}
	
	
	/* send to the right */
	MPI_Cart_shift(cart_com, 0, 1, &rank_source, &rank_dest);
	MPI_Sendrecv(&VAL2D(cur, width-2, 0), 1, column, rank_dest,   100, /* send column before ghost */ 
	             &VAL2D(cur, 0,       0), 1, column, rank_source, 100, /* receive 1st column (ghost) */
	             cart_com, &status);

	/* send to the left */
	MPI_Cart_shift(cart_com, 0, -1, &rank_source, &rank_dest);
	MPI_Sendrecv(&VAL2D(cur, 1,       0), 1, column, rank_dest,   100, /* send column after ghost */
	             &VAL2D(cur, width-1, 0), 1, column, rank_source, 100, /* receive last column (ghost) */
	             cart_com, &status);

	/* send down */
	MPI_Cart_shift(cart_com, 1, 1, &rank_source, &rank_dest);
	MPI_Sendrecv(&VAL2D(cur, 0, height-2), 1, row, rank_dest,   100, /* send row before ghost */
	             &VAL2D(cur, 0, 0       ), 1, row, rank_source, 100, /* receive 1st row (ghost) */
	             cart_com, &status);

	/* send up */
	MPI_Cart_shift(cart_com, 1, -1, &rank_source, &rank_dest);
	MPI_Sendrecv(&VAL2D(cur, 0, 1       ), 1, row, rank_dest,   100, /* send column after ghost */
	             &VAL2D(cur, 0, height-1), 1, row, rank_source, 100, /* receive last column (ghost) */
	             cart_com, &status);
}

int main(int argc, char *argv[])
{
	int nb_iter, rank, size, pheight, pwidth, height, width, ii;
	int cart_dims[2], cart_period[2], car_coord[2];
	MPI_Comm main_comm, cart_com;
	double *cur, *next, *tmp;
	FILE *conf_file;
	yaml_parser_t conf_parser;
	yaml_document_t conf_doc;
	yaml_node_t *pdi_conf;

	MPI_Init(&argc, &argv);

	main_comm = MPI_COMM_WORLD;
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	
	conf_file = fopen("example.yml", "rb");
	assert(conf_file);
	assert(yaml_parser_initialize(&conf_parser));
	yaml_parser_set_input_file(&conf_parser, conf_file);
	assert(yaml_parser_load(&conf_parser, &conf_doc));
	
	assert(!PC_get(&conf_doc, NULL, ".pdi", &pdi_conf));
	assert(!PC_get_int(&conf_doc, NULL, ".iter", &nb_iter));
	assert(!PC_get_int(&conf_doc, NULL, ".datasize[0]", &height));
	assert(!PC_get_int(&conf_doc, NULL, ".datasize[1]", &width));
	assert(!PC_get_int(&conf_doc, NULL, ".parallelism.height", &pheight));
	assert(!PC_get_int(&conf_doc, NULL, ".parallelism.width", &pwidth));
	
	assert(!PDI_init(&conf_doc, pdi_conf, &main_comm));
	assert(pwidth*pheight == size);
	
	cart_dims[0] =   pwidth; cart_dims[1] =   pheight;
	cart_period[0] = 1;      cart_period[1] = 1;
	MPI_Cart_create(MPI_COMM_WORLD, 2, cart_dims, cart_period, 1, &cart_com);
	MPI_Cart_coords(cart_com, rank, 2, car_coord);

	PDI_expose("coord", car_coord);
	PDI_expose("height", &height);
	PDI_expose("width", &width);

	cur = malloc(sizeof(double)*width*height);
	next = malloc(sizeof(double)*width*height);

	if ( PDI_import("main_field", cur) ) {
		init(cur, width, height, car_coord[0], car_coord[1]);
	}
	
	PDI_event("main_loop");
	for(ii=0; ii<nb_iter; ++ii) {
		PDI_expose("iter", &ii);
		PDI_expose("main_field", cur);
		iter(cur, next, height, width);
		exchange(cart_com, cur, height, width);
		tmp = cur; cur = next; next = tmp; // 
	}
	PDI_event("finalization");
	PDI_expose("main_field", cur);

	yaml_document_delete(&conf_doc);
	yaml_parser_delete(&conf_parser);
	fclose(conf_file);
	
	free(cur);
	free(next);

	PDI_finalize();
	MPI_Finalize();
	return 0;
}
