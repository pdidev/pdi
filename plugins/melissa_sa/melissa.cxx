/*
 * Copyright (C) 2022 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * Copyright (C) 2022 Institut National de Recherche en Informatique et en Automatique (Inria)
 * All rights reserved.
 */

#include <cassert>
#include <cstdio>

#include <stdexcept>
#include <unordered_map>

#include <melissa/api.h>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/context.h>
#include <pdi/error.h>
#include <pdi/expression.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

namespace {

using PDI::Array_datatype;
using PDI::Context;
using PDI::Datatype;
using PDI::Datatype_sptr;
using PDI::each;
using PDI::Expression;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_string;
using std::string;
using std::unordered_map;

class Melissa_variable
{
	Context& m_context;
	string m_name;
	size_t m_size;

public:
	Melissa_variable(Context& context, const string& name, size_t size, MPI_Comm comm)
		: m_context{context}
		, m_name{name}
		, m_size{size}
	{
		m_context.logger().debug("melissa_init(name={} size={})", name, size);
		melissa_init(name.c_str(), size, comm);
	}

	void send(const double* data) const
	{
		m_context.logger().trace("melissa_send(name={} data={})", m_name, reinterpret_cast<const void*>(data));
		melissa_send(m_name.c_str(), data);
	}

	size_t size() const { return m_size; }
};

const double* convert(const void* data, Datatype_sptr datatype)
{
	if (const Array_datatype* array_datatype = dynamic_cast<const Array_datatype*>(datatype.get())) {
		if (const Scalar_datatype* scalar_datatype = dynamic_cast<const Scalar_datatype*>(array_datatype->subtype().get())) {
			if (scalar_datatype->kind() == Scalar_kind::FLOAT && scalar_datatype->buffersize() == 8) {
				return static_cast<const double*>(data);
			}
		}
	}
	throw PDI::Type_error{
		"Wrong Type in transmitted array. Must be 1D double Array!",
	};
}

struct melissa_plugin: Plugin {
	/// default comm
	Expression m_default_comm;

	/// store melissa data (initialized)
	unordered_map<string, Melissa_variable> m_variables;

	/// store config send
	unordered_map<string, Expression> m_data_to_comm;

public:
	melissa_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
	{
		PC_tree_t comm_tree = PC_get(config, ".communicator");
		const int EXISTS = 0;
		if (PC_status(comm_tree) == EXISTS) {
			m_default_comm = PDI::to_string(comm_tree);
		}

		PC_tree_t send_tree = PC_get(config, ".send");
		if (PDI::is_map(send_tree)) {
			throw std::logic_error("send maps are not implemented");
			// some data will have MPI_Comm defined
			PDI::each(send_tree, [&](PC_tree_t key, PC_tree_t value) {
				string data_name = PDI::to_string(key);
				Expression comm_expr = PDI::to_string(value);
				if (!comm_expr && !m_default_comm) {
					throw PDI::Config_error{key, "no MPI communicator set"};
				}

				m_data_to_comm[data_name] = comm_expr;
			});
		} else {
			// all data use default MPI_Comm
			if (!m_default_comm) {
				throw PDI::Config_error{config, "no MPI communicator set"};
			}
			PDI::opt_each(send_tree, [&](PC_tree_t value) {
				string data_name = PDI::to_string(value);
				m_data_to_comm[data_name] = Expression{}; // add empty expression
			});
		}

		for (auto&& data_comm_pair: m_data_to_comm) {
			ctx.callbacks().add_data_callback(
				[&](const string& name, Ref ref) { //Ref is the pointer (with locks) and datatype
					// try to convert the data to array of doubles
					Ref_r data_ref_r{ref};
					assert(data_ref_r);
					const double* double_data = convert(data_ref_r.get(), data_ref_r.type());
					assert(double_data);

					// this will be called on main_field share
					if (m_variables.find(name) == m_variables.end()) {
						Expression comm = m_default_comm;
						if (m_data_to_comm[name]) {
							comm = m_data_to_comm[name];
						}
						assert(comm);

						Ref_r comm_ref = context()["mpi_communicator"].ref();
						if (!comm_ref) {
							throw PDI::Right_error{"MPI communicator unreadable"};
						}
						MPI_Comm mpi_comm = *(static_cast<const MPI_Comm*>(comm_ref.get()));

						// create Melissa_variable (initialize the variable in melissa)
						const Array_datatype* array_datatype = dynamic_cast<const Array_datatype*>(ref.type().get());
						assert(array_datatype);
						m_variables.emplace(name, Melissa_variable(context(), name, array_datatype->size(), mpi_comm));
					}
					const Array_datatype* array_datatype = dynamic_cast<const Array_datatype*>(ref.type().get());
					assert(array_datatype);
					if (array_datatype->size() != m_variables.at(name).size()) {
						throw PDI::Type_error{"Size of the array changed since the initialization!"};
					}

					// now we are sure that variable is initialized
					m_variables.at(name).send(double_data);

				},
				data_comm_pair.first
			); // get callback on each data defined in our map
		}

		context().logger().info("Melissa plug-in initialized");
	}

	~melissa_plugin()
	{
		melissa_finalize();
		context().logger().info("Melissa plug-in deinitialized");
	}

	/** Pretty name for the plugin that will be shown in the logger
     *
     * \return pretty name of the plugin
     */
	static std::string pretty_name() { return "Melissa"; }

}; // struct melissa_plugin

} // namespace

PDI_PLUGIN(melissa)
