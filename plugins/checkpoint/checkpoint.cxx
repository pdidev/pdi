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
using PDI::Error;
using PDI::Plugin;
using PDI::Ref;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;
using std::cout;
using std::endl;
using std::tie; 

using namespace decl_hdf5;


class checkpoint_plugin : public Plugin
{
private:

	string prev_cp_file; 

	long int failure_value;

	bool start_from_last_checkpoint = true ; 

    unordered_map<string, vector<File_op>> m_data;


public:
    checkpoint_plugin(Context& ctx, PC_tree_t config)
        : Plugin(ctx) // construct and initialize 
    {
		opt_each(config, [&](PC_tree_t item) { 
			
			// handles sequences to be compatible with the File_op::parse 
			// method called in the decl_hdf5 plugin 
			// otherwise, could just use PDI::each with mappings 

            each(item, [&](PC_tree_t key_tree, PC_tree_t value) {
                string key = to_string(key_tree);
                if (key == "failure") {
                    failure_value = to_long(value);
                } else if (key == "last_checkpoint") {
                    prev_cp_file = to_string(value);
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

		ctx.callbacks().add_data_callback([this](const std::string& name, Ref ref) {
			
			
			if((!prev_cp_file.empty()) && start_from_last_checkpoint){
				this-> load_checkpoint(name, ref);
			}

			else{
				this->write_checkpoint(name, ref); 
			}

		});
};

    ~checkpoint_plugin()
	{
		if (0 > H5close()) handle_hdf5_err("Cannot finalize HDF5 library");
		context().logger().info("Closing plugin");
	};

private:

    void write_checkpoint(const std::string& name, Ref ref)
    {
        Hdf5_error_handler _;
		for (auto&& op: m_data[name]) {
			op.execute(context());
		}
    
    };


	void load_checkpoint(const std::string& name, Ref ref)
    {
        Hdf5_error_handler _;

		Context& ctx = context();
		
		string dataset_name = "/" + name;

		hid_t h5_file_raw = -1;
		Raii_hid file_lst = make_raii_hid(H5Pcreate(H5P_FILE_ACCESS), H5Pclose);
	
		ctx.logger().trace("Opening `{}' file to read", prev_cp_file);

		h5_file_raw = H5Fopen(prev_cp_file.c_str(), H5F_ACC_RDONLY, file_lst);
		
		Raii_hid h5_file = make_raii_hid(h5_file_raw, H5Fclose, ("Cannot open `" + prev_cp_file + "' file").c_str());
		
		Raii_hid read_lst = make_raii_hid(H5Pcreate(H5P_DATASET_XFER), H5Pclose);

		PDI::Ref_w write_ref{ref};
		if (!write_ref) {
			ctx.logger().warn("Cannot read `{}' dataset: `{}' data not available", dataset_name, name);
			return;
		}

		Raii_hid h5_mem_space, h5_mem_type;
		tie(h5_mem_space, h5_mem_type) = space(write_ref.type());

		// ctx.logger().trace("Applying `{}' memory selection", dataset_name);
		// m_memory_selection.apply(ctx, h5_mem_space);

		ctx.logger().trace("Opening `{}' dataset", dataset_name);
		Raii_hid h5_set
			= make_raii_hid(H5Dopen2(h5_file, dataset_name.c_str(), H5P_DEFAULT), H5Dclose, ("Cannot open `" + dataset_name + "' dataset").c_str());

		ctx.logger().trace("Inquiring `{}' dataset dataspace", dataset_name);
		Raii_hid h5_file_space = make_raii_hid(H5Dget_space(h5_set), H5Sclose, ("Cannot inquire `" + dataset_name + "' dataset dataspace").c_str());

		// ctx.logger().trace("Applying `{}' dataset selection", dataset_name);
		// m_dataset_selection.apply(ctx, h5_file_space, h5_mem_space);

		// ctx.logger().trace("Validating `{}' dataset dataspaces selection", dataset_name);
		// validate_dataspaces(m_dataset_selection.selection_tree(), h5_mem_space, h5_file_space, dataset_name);

		ctx.logger().trace("Reading `{}' dataset", dataset_name);
		if (0 > H5Dread(h5_set, h5_mem_type, h5_mem_space, h5_file_space, read_lst, write_ref)) handle_hdf5_err();

		ctx.logger().trace("`{}' dataset read finished", dataset_name);

		start_from_last_checkpoint=false;
    };

};

PDI_PLUGIN(checkpoint)
