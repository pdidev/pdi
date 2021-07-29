/*******************************************************************************
 * Copyright (C) 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright
 *   notice, this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright
 *   notice, this list of conditions and the following disclaimer in the
 *   documentation and/or other materials provided with the distribution.
 * * Neither the name of CEA nor the names of its contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written permission.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 ******************************************************************************/

#include <mpi.h>

#include <string>

#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/logger.h>
#include <pdi/plugin.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/scalar_datatype.h>

namespace {

using PDI::Config_error;
using PDI::Context;
using PDI::Context_proxy;
using PDI::Data_descriptor;
using PDI::Datatype;
using PDI::Datatype_uptr;
using PDI::Error;
using PDI::Impl_error;
using PDI::len;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::Ref_w;
using PDI::Right_error;
using PDI::Scalar_datatype;
using PDI::Scalar_kind;
using PDI::to_long;
using PDI::to_string;
using PDI::Value_error;
using std::string;

/** Transtype C mpi comm to Fortran mpi comm
 *
 * \param ctx PDI context
 * \param c_comm_desc descriptor name of C mpi comm (source)
 * \param f_comm_desc descriptor name of Fortran mpi comm (destination)
 */
void transtype_C_to_F(Context& ctx, const string& c_comm_desc, const string& fortran_comm_desc)
{
	ctx.callbacks().add_data_callback([&ctx, fortran_comm_desc](const string& c_comm_desc, Ref ref) {
		ctx.logger()->debug("Transtype `{}' to `{}' (C->F)", c_comm_desc, fortran_comm_desc);
		Datatype_uptr mpi_comm_f_type = ctx.datatype(PC_parse_string("MPI_Comm_f"))->evaluate(ctx);
		Ref fortran_comm_ref {new MPI_Fint, [](void* p){delete static_cast<MPI_Fint*>(p);}, std::move(mpi_comm_f_type), true, true};
		if (Ref_r ref_r{ref}) {
			*static_cast<MPI_Fint*>(Ref_w{fortran_comm_ref}.get()) = MPI_Comm_c2f(*static_cast<const MPI_Comm*>(ref_r.get()));
			ctx.desc(fortran_comm_desc).share(fortran_comm_ref, false, false);
		} else {
			throw Right_error{"Cannot read `{}' data to transtype to fortran communicator", c_comm_desc};
		}
	}, c_comm_desc);
	
	ctx.callbacks().add_data_remove_callback([&ctx, fortran_comm_desc](const std::string& c_comm_desc, Ref c_comm_ref) {
		ctx.logger()->debug("`{}' no longer available, reclaim transtyped `{}'", fortran_comm_desc, c_comm_desc);
		if (Ref_w c_comm_ref_w{c_comm_ref}) {
			ctx.logger()->debug("Transtype back `{}' to `{}' (F->C)", fortran_comm_desc, c_comm_desc);
			if (Ref_r fortran_comm_ref_r = ctx.desc(fortran_comm_desc).ref()) {
				*static_cast<MPI_Comm*>(c_comm_ref_w.get()) = MPI_Comm_f2c(*static_cast<const MPI_Fint*>(fortran_comm_ref_r.get()));
			} else {
				throw Right_error{"Cannot read `{}' data", c_comm_desc};
			}
		}
		ctx.desc(fortran_comm_desc).release();
	}, c_comm_desc);
}


/** Transtype Fortran mpi comm to C mpi comm
 *
 * \param ctx PDI context
 * \param f_comm_desc descriptor name of Fortran mpi comm (source)
 * \param c_comm_desc descriptor name of C mpi comm (destination)
 */
void transtype_F_to_C(Context& ctx, const string& fortran_comm_desc, const string& c_comm_desc)
{
	ctx.callbacks().add_data_callback([&ctx, c_comm_desc](const string& fortran_comm_desc, Ref ref) {
		ctx.logger()->debug("Transtype `{}' to `{}` (F->C)", fortran_comm_desc, c_comm_desc);
		Datatype_uptr mpi_comm_type = ctx.datatype(PC_parse_string("MPI_Comm"))->evaluate(ctx);
		Ref c_comm_ref {new MPI_Comm, [](void* p){delete static_cast<MPI_Comm*>(p);}, move(mpi_comm_type), true, true};
		if (Ref_r ref_r{ref}) {
			*static_cast<MPI_Comm*>(Ref_w{c_comm_ref}.get()) = MPI_Comm_f2c(*static_cast<const MPI_Fint*>(ref_r.get()));
			ctx.desc(c_comm_desc).share(c_comm_ref, false, false);
		} else {
			throw Right_error{"Cannot read `{}' data to transtype to C communicator", fortran_comm_desc};
		}
	}, fortran_comm_desc);
	
	ctx.callbacks().add_data_remove_callback([&ctx, c_comm_desc](const string& fortran_comm_desc, Ref fortran_comm_ref) {
		ctx.logger()->debug("`{}' no longer available, reclaim transtyped `{}'", fortran_comm_desc, c_comm_desc);
		if (Ref_w fortran_comm_ref_w{fortran_comm_ref}) {
			ctx.logger()->debug("Transtype back `{}' to `{}' (C->F)", c_comm_desc, fortran_comm_desc);
			if (Ref_r c_comm_ref_r = ctx.desc(c_comm_desc).ref()) {
				*static_cast<MPI_Fint*>(fortran_comm_ref_w.get()) = MPI_Comm_c2f(*static_cast<const MPI_Comm*>(c_comm_ref_r.get()));
			} else {
				throw Right_error{"Cannot read `{}' data", c_comm_desc};
			}
		}
		ctx.desc(c_comm_desc).release();
	}, fortran_comm_desc);
}

/** Transtypes C/Fortran communicator depending on specification tree
 *
 * \param ctx PDI context
 * \param tree PC tree of mpi plugin
 * \param C_mpi_comm_type C mpi comm datatype
 * \param F_mpi_comm_type Fortran mpi comm datatype
 */
void MPI_Comm_transtype(Context& ctx, PC_tree_t tree, const Scalar_datatype& C_mpi_comm_type, const Scalar_datatype& F_mpi_comm_type)
{
	PC_tree_t transtype_tree = PC_get(tree, ".transtype");
	if (!PC_status(transtype_tree)) {
		ctx.logger()->warn("`transtype' key is deprecated when transtyping C/Fortran MPI communicator, please use Comm_c2f/Comm_f2c");
		if (C_mpi_comm_type == F_mpi_comm_type) {
			throw Value_error{"Transtype failed, cannot detect the direction of transtype"};
		}
		int len; PC_len(transtype_tree, &len);
		for (int i = 0; i < len; i++) {
			string src_desc = to_string(PC_get(transtype_tree, "{%d}", i));
			string dst_desc = to_string(PC_get(transtype_tree, "<%d>", i));
			if (*ctx.desc(src_desc).default_type()->evaluate(ctx) == C_mpi_comm_type) {
				/// src is C_comm, dst is F_comm
				transtype_C_to_F(ctx, src_desc, dst_desc);
			} else {
				/// src is F_comm, dst is C_comm
				transtype_F_to_C(ctx, src_desc, dst_desc);
			}
		}
	}
	
	PC_tree_t c2f_tree = PC_get(tree, ".Comm_c2f");
	if (!PC_status(c2f_tree)) {
		int len; PC_len(c2f_tree, &len);
		for (int i = 0; i < len; i++) {
			transtype_C_to_F(ctx, to_string(PC_get(c2f_tree, "{%d}", i)), to_string(PC_get(c2f_tree, "<%d>", i)));
		}
	}
	PC_tree_t f2c_tree = PC_get(tree, ".Comm_f2c");
	if (!PC_status(f2c_tree)) {
		int len; PC_len(f2c_tree, &len);
		for (int i = 0; i < len; i++) {
			transtype_F_to_C(ctx, to_string(PC_get(f2c_tree, "{%d}", i)), to_string(PC_get(f2c_tree, "<%d>", i)));
		}
	}
}

struct mpi_plugin: Plugin {
	/// the MPI_Comm datatype
	Scalar_datatype m_mpi_comm_datatype{Scalar_kind::UNKNOWN, sizeof(MPI_Comm), alignof(MPI_Comm)};
	
	/// the MPI_Comm_f datatype
	Scalar_datatype m_mpi_comm_f_datatype{Scalar_kind::SIGNED, sizeof(MPI_Fint)};
	
	void set_up_logger(Context& ctx, PC_tree_t)
	{
		// pdi global logger
		try {
			Context_proxy& ctx_proxy = dynamic_cast<Context_proxy&>(ctx);
			ctx_proxy.pdi_core_logger()->default_pattern("[%T][%{MPI_COMM_WORLD_rank:06d}][%n] *** %^%l%$: %v");
			ctx_proxy.pdi_core_logger()->evaluate_pattern(ctx);
		} catch (std::bad_cast&) {
			ctx.logger()->warn("Cannot cast Context to Context_proxy");
		}
	}
	
	void add_predefined(Context& ctx, const string& name, void* data, Datatype_uptr type)
	{
		Data_descriptor& predef_desc = ctx.desc(name);
		if (!predef_desc.empty()) {
			throw Impl_error{"Predefined descriptor already defined `{}'", name};
		}
		
		predef_desc.metadata(true);
		// share a RO reference on comm_self with no memory destruction function (local variable)
		predef_desc.share({data, nullptr, move(type), true, false}, true, false);
		predef_desc.reclaim(); // reclaim the reference and let PDI keep a copy (metadata)
	}
	
	mpi_plugin(Context& ctx, PC_tree_t config):
		Plugin{ctx}
	{
		int world_rank;
		MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
		add_predefined(ctx, "MPI_COMM_WORLD_rank", &world_rank, Datatype_uptr{new Scalar_datatype{Scalar_kind::SIGNED, sizeof(int)}});
		
		// logger requires MPI_COMM_WORLD.rank data
		set_up_logger(ctx, PC_get(config, ".logging"));
		
		// share the MPI_Comm datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm", [this](Context&, PC_tree_t) {
			return m_mpi_comm_datatype.clone();
		});
		
		//load MPI_COMM_WORLD
		MPI_Comm comm_world = MPI_COMM_WORLD;
		add_predefined(ctx, "MPI_COMM_WORLD", &comm_world, m_mpi_comm_datatype.clone_type());
		
		//load MPI_COMM_SELF
		MPI_Comm comm_self = MPI_COMM_SELF;
		add_predefined(ctx, "MPI_COMM_SELF", &comm_self, m_mpi_comm_datatype.clone_type());
		
		//load MPI_COMM_NULL
		MPI_Comm comm_null = MPI_COMM_NULL;
		add_predefined(ctx, "MPI_COMM_NULL", &comm_null, m_mpi_comm_datatype.clone_type());
		
		// share the MPI_Comm_f datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm_f", [this](Context&, PC_tree_t) {
			return m_mpi_comm_f_datatype.clone();
		});
		
		//load MPI_COMM_WORLD_F
		MPI_Fint comm_world_f = MPI_Comm_c2f(MPI_COMM_WORLD);
		add_predefined(ctx, "MPI_COMM_WORLD_F", &comm_world_f, m_mpi_comm_f_datatype.clone_type());
		
		//load MPI_COMM_SELF_F
		MPI_Fint comm_self_f = MPI_Comm_c2f(MPI_COMM_SELF);
		add_predefined(ctx, "MPI_COMM_SELF_F", &comm_self_f, m_mpi_comm_f_datatype.clone_type());
		
		//load MPI_COMM_NULL_F
		MPI_Fint comm_null_f = MPI_Comm_c2f(MPI_COMM_NULL);
		add_predefined(ctx, "MPI_COMM_NULL_F", &comm_null_f, m_mpi_comm_f_datatype.clone_type());
		
		MPI_Comm_transtype(ctx, config, m_mpi_comm_datatype, m_mpi_comm_f_datatype);
		
		ctx.logger()->info("Plugin loaded successfully");
	}
	
	~mpi_plugin()
	{
		context().logger()->info("Closing plugin");
	}
	
	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name()
	{
		return "MPI";
	}
};

} // namespace <anonymous>

PDI_PLUGIN(mpi)
