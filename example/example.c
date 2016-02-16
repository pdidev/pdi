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
	MPI_Init(&argc, &argv);

	MPI_Comm main_comm = MPI_COMM_WORLD;
	
	FILE *conf_file = fopen("example.yml", "rb"); assert(conf_file);
	yaml_parser_t conf_parser; assert(yaml_parser_initialize(&conf_parser));
	yaml_parser_set_input_file(&conf_parser, conf_file);
	yaml_document_t conf_doc; 
	if ( !yaml_parser_load(&conf_parser, &conf_doc) ) {
		printf("%s:%d:%d: Error: %s\n",
				"example.yml",
				conf_parser.problem_mark.line,
				conf_parser.problem_mark.column,
				conf_parser.problem
  			);
		if ( conf_parser.context ) {
		printf("%s:%d:%d: Error: %s\n",
				"example.yml",
				conf_parser.context_mark.line,
				conf_parser.context_mark.column,
				conf_parser.context
  			);
		}
		exit(1);
	}
	
	PC_tree_t conf = PC_root(&conf_doc);
	
	int nb_iter; PC_int(PC_get(conf, ".iter"), &nb_iter);
	int height; PC_int(PC_get(conf, ".datasize[0]"), &height);
	int width; PC_int(PC_get(conf, ".datasize[1]"), &width);
	int pheight; PC_int(PC_get(conf, ".parallelism.height"), &pheight);
	int pwidth; PC_int(PC_get(conf, ".parallelism.width"), &pwidth);
	PDI_init(PC_get(conf, ".pdi"), &main_comm);
	
	int size; MPI_Comm_size(main_comm, &size);
	int rank; MPI_Comm_rank(main_comm, &rank);
	
	assert(pwidth*pheight == size);
	
	int cart_dims[2] = { pwidth, pheight };
	int cart_period[2] = { 1, 1 };
	MPI_Comm cart_com; MPI_Cart_create(MPI_COMM_WORLD, 2, cart_dims, cart_period, 1, &cart_com);
	int car_coord[2]; MPI_Cart_coords(cart_com, rank, 2, car_coord);

	PDI_expose("coord", car_coord);
	PDI_expose("height", &height);
	PDI_expose("width", &width);
	PDI_expose("pheight", &pheight);
	PDI_expose("pwidth", &pwidth);
	PDI_expose("nb_iter", &nb_iter);

	double *cur = malloc(sizeof(double)*width*height);
	double *next = malloc(sizeof(double)*width*height);

	if ( PDI_import("main_field", cur) ) {
		init(cur, width, height, car_coord[0], car_coord[1]);
	}
	
	PDI_event("main_loop");
	int ii;
	for(ii=0; ii<nb_iter; ++ii) {
		PDI_expose("iter", &ii);
		PDI_expose("main_field", cur);
		iter(cur, next, height, width);
		exchange(cart_com, cur, height, width);
		double *tmp = cur; cur = next; next = tmp;
	}
	PDI_event("finalization");
	PDI_expose("main_field", cur);

	PDI_finalize();
	
	yaml_document_delete(&conf_doc);
	yaml_parser_delete(&conf_parser);
	fclose(conf_file);
	
	free(cur);
	free(next);

	MPI_Finalize();
	return 0;
}
