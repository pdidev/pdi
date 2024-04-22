#include <string>
#include <err.h>
#include <stdio.h>

#include <pdi/context.h>
#include <pdi/plugin.h>
#include <pdi/ref_any.h>
#include <Damaris.h>
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
            const void* ref_as_void ; // = ref.get();
            int err = DAMARIS_OK ;
            
             int i;
             /* int mpi_size = 4 ;
             int mpi_rank = 1 ;
            int NROW = 96,NCOL = 96;
            int data_size = 96*96/mpi_size ;
            int* data = (int*)malloc(data_size * sizeof(int));
            for(i=0; i<data_size; i++) {
                data[i] = (int) (mpi_rank + i) ;
            } */
            if (PDI::Ref_w wref = ref) {
                ref_as_void = wref.get() ;
            } else if (PDI::Ref_rw rwref = ref) {
                ref_as_void = rwref.get() ;
            }
            err = damaris_write("life/cells", ref_as_void);   // ref_as_void
            if (err != DAMARIS_OK) 
                printf("Error damaris_write()  : %s",damaris_error_string(err));
            // free(data) ;
            
            // Sends a signal.
            err = damaris_signal("life/cells");  // must match what is in the <actions><event name="" ...> XML 
            if (err != DAMARIS_OK) 
                printf("Error damaris_signal(life/cells)  : %s",damaris_error_string(err));
                
            // Finishes the current iteration. Runs all the Damaris in-built plugins
            err = damaris_end_iteration();
            if (err != DAMARIS_OK) 
                printf("Error damaris_end_iteration()  : %s",damaris_error_string(err));
            
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
