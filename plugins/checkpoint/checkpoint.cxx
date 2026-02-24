#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/expression.h>

#include <hdf5.h>
#include <iostream>
#include "../decl_hdf5/dataset_op.h"
#include "../decl_hdf5/collision_policy.h"
#include "../decl_hdf5/file_op.h"
#include "../decl_hdf5/hdf5_wrapper.h"

using PDI::Context;
using PDI::each;
using PDI::opt_each;
using PDI::Plugin;
using PDI::Ref;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;
using std::cout;
using std::endl;

using namespace decl_hdf5;


class checkpoint_plugin : public PDI::Plugin
{
private:

	char** prev_cp_file; 
    unordered_map<string, vector<File_op>> m_data;


public:
    checkpoint_plugin(PDI::Context& ctx, PC_tree_t config)
        : Plugin(ctx) // construct and initialize 
    {
		long int failure_value;
		
		PC_tree_t recovery_tree = PC_get(config, ".failure");
		int len;
		PC_len(recovery_tree, &len);

		if(len>0) { 

			cout << " Try out " << endl;
				
				PC_tree_t last_cp_node = PC_get(recovery_tree, ".last_checkpoint");
				if (PC_status(last_cp_node)) {
					PC_string(last_cp_node, prev_cp_file);
				}
			}
		
		else { 

			cout << " Try out 2 " << endl;
			
			PC_int(recovery_tree, &failure_value);

			cout << " failure value =  " << failure_value << endl;

			if (failure_value!=0){
				cout<< " A recovery file has to be defined if a failure occurred " << endl;
				return;
			}

			// PC_tree_t hdf5_tree = PC_get(config, ".file");

			Hdf5_error_handler _;
			if (0 > H5open()) handle_hdf5_err("Cannot initialize HDF5 library");
				
				each(config, [&](PC_tree_t elem) {

				for (auto&& op: File_op::parse(ctx, elem)) {
					auto&& events = op.event();
					
						// if there are no event names, this is data triggered
						assert(op.dataset_ops().size() <= 1);
						for (auto&& transfer: op.dataset_ops()) {
							m_data[transfer.value()].emplace_back(op);
						}
						for (auto&& transfer: op.attribute_ops()) {
							if (!transfer.desc().empty()) {
								m_data[transfer.desc()].emplace_back(op);
							}
						}
						for (auto&& transfer: op.dataset_size_ops()) {
							m_data[transfer.first].emplace_back(op);
						}				
				}
			});

			ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) { this->write_checkpoint(name, ref); });
    	}
	};

    ~checkpoint_plugin()
	{
		if (0 > H5close()) handle_hdf5_err("Cannot finalize HDF5 library");
		context().logger().info("Closing plugin");
	};

private:

    void write_checkpoint(const std::string& name, PDI::Ref ref)
    {
        Hdf5_error_handler _;
		for (auto&& op: m_data[name]) {
			op.execute(context());
		}
    
    };

};

PDI_PLUGIN(checkpoint)
