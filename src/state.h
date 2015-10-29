#ifndef PDI_STATE_H__
#define PDI_STATE_H__

typedef struct PDI_plugin_impl_s PDI_plugin_impl_t;

typedef struct PDI_loaded_plugin_s
{
	char *name;
	
	PDI_plugin_impl_t *impl;
	
} PDI_loaded_plugin_t;

typedef struct PDI_state_s
{
	PDI_plugin_impl_t *loaded_plugins;
	
	unsigned nb_loaded_plugins;
	
} PDI_state_t;

extern PDI_state_t PDI_state;

#endif // PDI_STATE_H__
