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

	string prev_cp_file; 

	long int failure_value;

    unordered_map<string, vector<File_op>> m_data;


public:
    checkpoint_plugin(PDI::Context& ctx, PC_tree_t config)
        : Plugin(ctx) // construct and initialize 
    {
		PDI::opt_each(config, [&](PC_tree_t item) {// Each sequence item is a mapping with keys

            PDI::each(item, [&](PC_tree_t key_tree, PC_tree_t value) {
                string key = PDI::to_string(key_tree);
                if (key == "failure") {
                    failure_value = PDI::to_long(value);
                } else if (key == "last_checkpoint") {
                    prev_cp_file = PDI::to_string(value);
                }
            });
        });

	
		
		if(failure_value!=0 && prev_cp_file.empty()){

			cout << " The last checkpoint file has to be defined if the failure key is set " << endl; 
			
			return; 
		}

		if(failure_value==0 && (!prev_cp_file.empty())){

			cout << " Ignoring the last checkpoint file as the failure key is not set" << endl; 

		}

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
