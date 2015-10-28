#include "pdi.h"
#include "state.h"
#include "plugin.h"

PDI_state_t PDI_state;

PDI_status_t PDI_init(const yaml_node_t* conf, MPI_Comm* world)
{
	
	for ( int ii=0; ii<PDI_state.nb_loaded_plugins; ++ii ) {
		PDI_status_t result = PDI_state.loaded_plugins[ii].init(conf, world);
		if ( result ) return result;
	}
	return PDI_OK;
}

PDI_status_t PDI_finalize()
{
	return PDI_OK;
}

PDI_status_t PDI_Event(const char* event)
{
	return PDI_OK;
}

PDI_status_t PDI_share(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_access(const char* name, void* data)
{
	return PDI_UNAVAILABLE;
}

PDI_status_t PDI_reclaim(const char* name)
{
	return PDI_OK;
}

PDI_status_t PDI_release(const char* name)
{
	return PDI_OK;
}


PDI_status_t PDI_expose(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_export(const char* name, const void* data)
{
	return PDI_OK;
}

PDI_status_t PDI_import(const char* name, void* data)
{
	return PDI_UNAVAILABLE;
}
 