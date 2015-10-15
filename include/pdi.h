
enum PDI_Status { PDI_OK=0 };

/** Initializes PDI
 */
int PDI_init(const paraconf_t *conf);

/** Initializes PDI
 */
int PDI_MPI_init(MPI_Comm *world, const paraconf_t *conf);

/**
 */
int PDI_finalize();

/**
 */
int PDI_Event(const char *event);

/**
 */
int PDI_val(const void *data, const char *name);
