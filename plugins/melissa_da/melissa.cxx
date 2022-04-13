/*
 * Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2022 Institut National de Recherche en Informatique et en Automatique (Inria)
 * All rights reserved.
 */

#include <cassert>
#include <cstdio>

#include <stdexcept>
#include <unordered_map>
#include <mpi.h>

#include <melissa_da_api.h>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

namespace {

using PDI::Context;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_rw;
using PDI::Ref_w;
using PDI::Plugin;
using PDI::Array_datatype;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;
using PDI::each;
using PDI::Datatype;
using PDI::Datatype_sptr;
using PDI::Expression;
using PDI::Config_error;
using PDI::Value_error;
using std::string;
using std::shared_ptr;
using std::dynamic_pointer_cast;
using std::unordered_map;


double* convert(void* data, Datatype_sptr datatype) {
    if (const Array_datatype* array_datatype = dynamic_cast<const Array_datatype*>(datatype.get())) {
        if (const Scalar_datatype* scalar_datatype = dynamic_cast<const Scalar_datatype*>(array_datatype->subtype().get())) {
            if (scalar_datatype->kind() == Scalar_kind::FLOAT && scalar_datatype->buffersize() == 8) {
                return static_cast<double*>(data);
            }
        }
    }
    throw PDI::Type_error{"Wrong Type in transmitted array. Must be 1D double Array!",};
}

struct Variable_properties {
    Expression m_communicator;
    Expression m_index_map;
    Expression m_hidden_values; // hidden_values
    Expression m_index_map_hidden;
};

class Melissa_variable {
	Context& m_context; // PDI context
    const char *m_field_name; // melissa variable name
    const size_t m_local_vect_size; // size of vector in bytes
    const int m_bytes_per_element; // size in bytes of element (now always sizeof(dobule))
    const int m_bytes_per_element_hidden; // size in bytes of hidden element (now always sizeof(dobule))

    Variable_properties m_properties; // variable comm, index_map, hidden_vect_size and index_map_hidden (may be null)

    Melissa_variable(Context& context, const char *field_name, size_t local_vect_size, int bytes_per_element,
    const int bytes_per_element_hidden, Variable_properties var_properties):
		m_context{context},
        m_field_name{field_name},
        m_local_vect_size{local_vect_size},
        m_bytes_per_element{bytes_per_element},
        m_bytes_per_element_hidden{bytes_per_element_hidden},
        m_properties{std::move(var_properties)}

    {
        m_context.logger().debug(
            "melissa_init(field_name={} local_vect_size={} local_hidden_vect_size bytes_per_element={} bytes_per_element_hidden={})", 
            field_name, local_vect_size, /*local_hidden_vect_size,*/ bytes_per_element, bytes_per_element_hidden
        );

        // get mpi comm
        MPI_Comm mpi_comm = *(static_cast<const MPI_Comm*>(Ref_r{m_properties.m_communicator.to_ref(m_context)}.get()));
        
        if (m_properties.m_index_map_hidden) {
            // index map and hidden state (requires m_index_map and m_hidden_values)
            if (!m_properties.m_index_map && !m_properties.m_hidden_values) {
                throw Value_error{"When index_map_hidden defined, index_map and hidden_vect_size must be defined also"};
            }
            melissa_init_with_index_map(field_name,
                                        local_vect_size,
                                        m_properties.m_hidden_values.to_ref(m_context).type()->datasize(),
                                        bytes_per_element,
                                        bytes_per_element_hidden,
                                        mpi_comm,
                                        static_cast<const INDEX_MAP_T*>(Ref_r{m_properties.m_index_map.to_ref(m_context)}.get()),
                                        static_cast<const INDEX_MAP_T*>(Ref_r{m_properties.m_index_map_hidden.to_ref(m_context)}.get()));
            m_context.logger().info("melissa_init index_map hidden state successful");
        } else if (m_properties.m_index_map) {
            // index map
            melissa_init_with_index_map(field_name,
                                        local_vect_size,
                                        0,
                                        bytes_per_element,
                                        bytes_per_element_hidden,
                                        mpi_comm,
                                        static_cast<const INDEX_MAP_T*>(Ref_r{m_properties.m_index_map.to_ref(m_context)}.get()),
                                        nullptr);
            m_context.logger().info("melissa_init index map successful");
        } else if (m_properties.m_hidden_values) {
            // hidden state
            melissa_init(field_name,
                        local_vect_size,
                        m_properties.m_hidden_values.to_ref(m_context).type()->datasize(),
                        bytes_per_element,
                        bytes_per_element_hidden,
                        mpi_comm);
            m_context.logger().info("melissa_init hidden state successful");
        } else {
            // no index_map nor hidden state
            melissa_init(field_name,
                        local_vect_size,
                        0,
                        bytes_per_element,
                        bytes_per_element_hidden,
                        mpi_comm);
            m_context.logger().info("melissa_init default successful");
        }
    }

public:
    Melissa_variable(Context& context, const char *field_name, size_t local_vect_size, Variable_properties var_properties):
		Melissa_variable(context, field_name, local_vect_size, sizeof(double), sizeof(double), std::move(var_properties))
    {}

    int expose_d(double *values) const {
        // get mpi comm
        MPI_Comm mpi_comm = *(static_cast<const MPI_Comm*>(Ref_r{m_properties.m_communicator.to_ref(m_context)}.get()));
        int comm_rank;
        MPI_Comm_rank(mpi_comm, &comm_rank);
        if (m_properties.m_hidden_values) {
            Ref_rw hidden_values_ref_rw {m_properties.m_hidden_values.to_ref(m_context)};
            double* hidden_values = convert(hidden_values_ref_rw.get(), hidden_values_ref_rw.type());
            m_context.logger().trace(
            "{}: melissa_expose_d(field_name={} values[0]={} hidden_values[0]={})",
			comm_rank, m_field_name, values[0], hidden_values[0]);
            return melissa_expose_d(m_field_name, values, hidden_values);
        } else {
            m_context.logger().trace(
            "{}: melissa_expose_d(field_name={} values[0]={})",
			comm_rank, m_field_name, values[0]);
            return melissa_expose_d(m_field_name, values, nullptr);
        }
    }

    size_t size() const {
        return m_local_vect_size;
    }
};


struct melissa_da_plugin: Plugin {

    /// status (nsteps)
    int m_status;

    /// default comm
    Expression m_default_comm;

    /// store melissa data (initialized)
    unordered_map<string, Melissa_variable> m_variables;

    /// store variable properties
    unordered_map<string, Variable_properties> m_variable_properties;

public:
    melissa_da_plugin(Context& ctx, PC_tree_t config):
        Plugin {ctx}
    {
        PC_tree_t current_step = PC_get(config, ".current_step");
        const int EXISTS = 0;
        if(PC_status(current_step) == EXISTS) {
            context().logger().debug("current_step defined: {}", PDI::to_string(current_step));
            ctx.callbacks().add_data_callback([&](const string& name, Ref ref) {
                Ref_w data_ref_w {ref};
                if (!data_ref_w) {
                    throw PDI::Right_error{"Cannot access write to variable: {}", name};
                }
                *static_cast<int*>(data_ref_w.get()) = melissa_get_current_step();
            }, PDI::to_string(current_step));
        }

        PC_tree_t status_step = PC_get(config, ".status");
        if(PC_status(status_step) == EXISTS) {
            context().logger().debug("status defined: {}", PDI::to_string(status_step));
            ctx.callbacks().add_data_callback([&](const string& name, Ref ref) {
                Ref_w data_ref_w {ref};
                if (!data_ref_w) {
                    throw PDI::Right_error{"Cannot access write to variable: {}", name};
                }
                *static_cast<int*>(data_ref_w.get()) = m_status;
            }, PDI::to_string(status_step));
        }

        PC_tree_t state_id = PC_get(config, ".current_state_id");
        if(PC_status(state_id) == EXISTS) {
            context().logger().debug("current_state_id defined: {}", PDI::to_string(state_id));
            ctx.callbacks().add_data_callback([&](const string& name, Ref ref) {
                Ref_w data_ref_w {ref};
                if (!data_ref_w) {
                    throw PDI::Right_error{"Cannot access write to variable: {}", name};
                }
                *static_cast<int*>(data_ref_w.get()) = melissa_get_current_state_id();
            }, PDI::to_string(state_id));
        }

        PC_tree_t comm_tree = PC_get(config, ".communicator");
        if(PC_status(comm_tree) == EXISTS) {
            m_default_comm = PDI::to_string(comm_tree);
        }

        PC_tree_t expose_tree = PC_get(config, ".expose");
        if (PDI::is_map(expose_tree)) {
            PDI::each(expose_tree, [&](PC_tree_t key, PC_tree_t value) {
                string data_name = PDI::to_string(key);
                context().logger().debug("expose defined: {}", data_name);
                Variable_properties var_property;
                PC_tree_t comm_tree = PC_get(value, ".communicator");
                if (PC_status(comm_tree) == EXISTS) {
                    var_property.m_communicator = PDI::to_string(comm_tree);
                } else {
                    var_property.m_communicator = m_default_comm;
                }
                if(!var_property.m_communicator) {
                    throw PDI::Config_error{key, "no MPI communicator set"};
                }

                PC_tree_t index_map_tree = PC_get(value, ".index_map");
                if (PC_status(index_map_tree) == EXISTS) {
                    context().logger().debug("    with index map");
                    var_property.m_index_map = PDI::to_string(index_map_tree);
                }

                PC_tree_t hidden_values_tree = PC_get(value, ".hidden_values");
                if (PC_status(hidden_values_tree) == EXISTS) {
                    context().logger().debug("    with hidden values");
                    var_property.m_hidden_values = PDI::to_string(hidden_values_tree);
                }

                PC_tree_t index_map_hidden_tree = PC_get(value, ".index_map_hidden");
                if (PC_status(index_map_hidden_tree) == EXISTS) {
                    context().logger().debug("    with index map hidden");
                    var_property.m_index_map_hidden = PDI::to_string(index_map_hidden_tree);
                }

                m_variable_properties.emplace(data_name, std::move(var_property));
            });
        } else {
            // all data use default properties
            if(!m_default_comm) {
                throw PDI::Config_error{config, "no MPI communicator set"};
            }

            PDI::opt_each(expose_tree, [&](PC_tree_t value) {
                string data_name = PDI::to_string(value);
                context().logger().debug("expose defined: {}", data_name);
                Variable_properties var_property;
                var_property.m_communicator = m_default_comm;
                m_variable_properties.emplace(data_name, std::move(var_property));
            });
        }

        for (auto&& data_property_pair : m_variable_properties) {
            ctx.callbacks().add_data_callback([&](const string& name, Ref ref) { //Ref is the pointer (with locks) and datatype
                // try to convert the data to array of doubles
                Ref_rw data_ref_rw {ref};
                if (!data_ref_rw) {
                    throw PDI::Right_error{"Cannot access read/write to variable: {}", name};
                }

                double* double_data = convert(data_ref_rw.get(), data_ref_rw.type());

                shared_ptr<const Array_datatype> array_datatype = dynamic_pointer_cast<const Array_datatype>(ref.type());
                // this will be called on main_field share
                if(m_variables.find(name) == m_variables.end()) {
                    // create Melissa_variable (initialize the variable in melissa)
                    m_variables.emplace(name, Melissa_variable(context(), name.c_str(), array_datatype->buffersize(), m_variable_properties[name]));
                }

                if (array_datatype->buffersize() != m_variables.at(name).size()) {
                    throw PDI::Type_error{"Size of the array changed since the initialization!"};
                }

                // now we are sure that variable is initialized
                context().logger().debug("expose_d: {}", name);
                m_status = m_variables.at(name).expose_d(double_data);

            }, data_property_pair.first); // get callback on each data defined in our map
        }

        context().logger().info("Melissa plug-in initialized");
    }

    ~melissa_da_plugin()
    {
        // melissa_finalize();
        context().logger().info("Melissa plug-in deinitialized");
    }

    /** Pretty name for the plugin that will be shown in the logger
     *
     * \return pretty name of the plugin
     */
    static std::string pretty_name()
    {
        return "Melissa_da";
    }

}; // struct melissa_plugin

} // namespace <anonymous>

PDI_PLUGIN(melissa_da)
