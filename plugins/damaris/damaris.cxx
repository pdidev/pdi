#include <string>
#include <err.h>
#include <stdio.h>

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include "Damaris.h"
#include <unistd.h>


namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Plugin;
using std::string;

struct damaris_plugin: Plugin {
	damaris_plugin(Context& ctx, PC_tree_t config):
		Plugin {ctx}
	{
        ctx.callbacks().add_init_callback([this, config](){
			this->context().logger().info("DAMARIS_OK : {} DAMARIS_NO_SERVER: {}", DAMARIS_OK,DAMARIS_NO_SERVER);
        });	

        ctx.callbacks().add_data_callback([this](const string& name, Ref ref) {
            this->context().logger().info(">>= data being available in the store: {}", name);
            const char* name_char = name.c_str();
            const void* ref_as_void = &ref;


            damaris_write(name, ref_as_void); //Envoi de la variable à Damaris
               
            // Sends a signal.
            damaris_signal("test_event",name); //On dit au serveur d’executer la fonction liée à l’event
            //sleep(5);
               
            // Finishes the current iteration.
            damaris_end_iteration(); 
           
        });

		ctx.callbacks().add_data_remove_callback([this](const string& name, Ref ref) {
			this->context().logger().info("<<= data stop being available in the store: {}", name);
			
		});
		ctx.callbacks().add_event_callback([this](const string& name) {
			this->context().logger().info("!!!                            named event: {}", name);
		});
		context().logger().info("Welcome!");
	}
	
	~damaris_plugin()
	{

		context().logger().info("Goodbye!");
		
	}
	
	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static string pretty_name()
	{
		return "damaris_plugin";
	}
	
}; // struct trace_plugin

} // namespace <anonymous>

PDI_PLUGIN(damaris)
