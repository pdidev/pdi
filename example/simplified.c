int main(int argc, char *argv[])
{
	MPI_Init(&argc, &argv);
	
	FILE *conf_file = fopen("example.yml", "rb");
	yaml_parser_t conf_parser; yaml_parser_initialize(&conf_parser);
	yaml_parser_set_input_file(&conf_parser, conf_file);
	yaml_document_t conf_doc; yaml_parser_load(&conf_parser, &conf_doc);
	yaml_node_t *conf = yaml_document_get_root_node(&conf_doc);
	
	yaml_node_t *pdi_conf; PC_get(conf, "pdi", pdi_conf);
	MPI_Comm main_comm = MPI_COMM_WORLD;
	PDI_MPI_init(pdi_conf, &main_comm);

	int nb_iter, pheight, pwidth, height, width;
	PC_get_int(conf, "iter", &nb_iter);
	PC_get_int(conf, "datasize[0]", &height);
	PC_get_int(conf, "datasize[1]", &width);
	PC_get_int(conf, "parallelism/height", &pheight);
	PC_get_int(conf, "parallelism/width", &pwidth);

	PDI_Event("initialization");

	int rank, size, cart_dims[2], cart_period[2], car_coord[2];
	MPI_Comm_size(MPI_COMM_WORLD, &size);
	MPI_Comm_rank(MPI_COMM_WORLD, &rank);
	assert(pwidth*pheight == size);
	cart_dims[0] = pwidth;
	cart_dims[1] = pheight;
	cart_period[0]=0; cart_period[1]=0;
	MPI_Comm cart_com;
	MPI_Cart_create(MPI_COMM_WORLD, 2, cart_dims, cart_period, 1, &cart_com);
	MPI_Cart_coords(main_comm, rank, 2, car_coord);

	PDI_expose("coord", car_coord);
	PDI_expose("height", &height);
	PDI_expose("width", &width);

	double *cur = malloc(sizeof(double)*width*height);
	double *next = malloc(sizeof(double)*width*height);

	init(cur, width, height, car_coord[0], car_coord[1]);

	PDI_Event("main_loop");
	for(int ii=0; ii<nb_iter; ++ii) {
		PDI_expose("iter", &ii);
		PDI_expose("main_field", cur);
		iter(cur, next, width, height);
		exchange(cart_com,cur,next,height,width);
		tmp = cur; cur = next; next = tmp; // 
	}
	PDI_Event("finalization");
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
