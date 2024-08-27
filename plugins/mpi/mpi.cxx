/*******************************************************************************
 * Copyright (C) 2020-2022 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
#include <type_traits>

#include <pdi/context.h>
#include <pdi/context_proxy.h>
#include <pdi/logger.h>
#include <pdi/paraconf_wrapper.h>
#include <pdi/plugin.h>
#include <pdi/scalar_datatype.h>

namespace {

using PDI::Config_error;
using PDI::Context;
using PDI::Context_proxy;
using PDI::Data_descriptor;
using PDI::Datatype;
using PDI::Datatype_sptr;
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
using PDI::Type_error;
using PDI::Value_error;
using std::string;


template <size_t size, Scalar_kind signedness>
struct Type_from_size;

template <>
struct Type_from_size<1, Scalar_kind::SIGNED> {
	using type = std::int8_t;
};

template <>
struct Type_from_size<2, Scalar_kind::SIGNED> {
	using type = std::int16_t;
};

template <>
struct Type_from_size<4, Scalar_kind::SIGNED> {
	using type = std::int32_t;
};

template <>
struct Type_from_size<8, Scalar_kind::SIGNED> {
	using type = std::int64_t;
};

template <>
struct Type_from_size<1, Scalar_kind::UNSIGNED> {
	using type = std::uint8_t;
};

template <>
struct Type_from_size<2, Scalar_kind::UNSIGNED> {
	using type = std::uint16_t;
};

template <>
struct Type_from_size<4, Scalar_kind::UNSIGNED> {
	using type = std::uint32_t;
};

template <>
struct Type_from_size<8, Scalar_kind::UNSIGNED> {
	using type = std::uint64_t;
};

template <size_t size, Scalar_kind signedness>
using Type_from_size_t = typename Type_from_size<size, signedness>::type;

template <size_t size, class T>
static void sized_set_val(Ref_w& to, T const & from, std::shared_ptr<const Scalar_datatype> const & to_type)
{
	switch (to_type->kind()) {
	case Scalar_kind::SIGNED:
		*static_cast<Type_from_size_t<size, Scalar_kind::SIGNED>*>(to.get()) = from;
		break;
	case Scalar_kind::UNSIGNED:
		*static_cast<Type_from_size_t<size, Scalar_kind::UNSIGNED>*>(to.get()) = from;
		break;
	default:
		throw Type_error{"Integer scalar expected, got:\n{}", to.type()->debug_string()};
	}
}

template <class T>
static void set_val(Ref_w& to, T const & from)
{
	auto to_type = std::dynamic_pointer_cast<const Scalar_datatype>(to.type());
	if (!to_type) {
		throw Type_error{"Scalar expected, got:\n{}", to.type()->debug_string()};
	}
	switch (to_type->buffersize()) {
	case 1:
		sized_set_val<1>(to, from, to_type);
		break;
	case 2:
		sized_set_val<2>(to, from, to_type);
		break;
	case 4:
		sized_set_val<4>(to, from, to_type);
		break;
	case 8:
		sized_set_val<8>(to, from, to_type);
		break;
	}
}

template <size_t size, class T>
static void sized_set_val(T& to, Ref_r const & from, std::shared_ptr<const Scalar_datatype> const & from_type)
{
	switch (from_type->kind()) {
	case Scalar_kind::SIGNED:
		to = *static_cast<Type_from_size_t<size, Scalar_kind::SIGNED> const *>(from.get());
		break;
	case Scalar_kind::UNSIGNED:
		to = *static_cast<Type_from_size_t<size, Scalar_kind::UNSIGNED> const *>(from.get());
		break;
	default:
		throw Type_error{"Integer scalar expected, got:\n{}", from_type->debug_string()};
	}
}

template <class T>
static void set_val(T& to, Ref_r const & from)
{
	auto from_type = std::dynamic_pointer_cast<const Scalar_datatype>(from.type());
	if (!from_type) {
		throw Type_error{"Scalar expected, got:\n{}", from.type()->debug_string()};
	}
	switch (from_type->buffersize()) {
	case 1:
		sized_set_val<1>(to, from, from_type);
		break;
	case 2:
		sized_set_val<2>(to, from, from_type);
		break;
	case 4:
		sized_set_val<4>(to, from, from_type);
		break;
	case 8:
		sized_set_val<8>(to, from, from_type);
		break;
	}
}

struct mpi_plugin: Plugin {
	/// the MPI_Comm datatype
	std::shared_ptr<Scalar_datatype> m_mpi_comm_datatype = Scalar_datatype::make(Scalar_kind::UNKNOWN, sizeof(MPI_Comm), alignof(MPI_Comm));

	/// the MPI_Comm_f datatype
	std::shared_ptr<Scalar_datatype> m_mpi_comm_f_datatype
		= Scalar_datatype::make((std::is_signed_v<MPI_Fint> ? Scalar_kind::SIGNED : Scalar_kind::UNSIGNED), sizeof(MPI_Fint), alignof(MPI_Fint));

	/** Pretty name for the plugin that will be shown in the logger
	 *
	 * \return pretty name of the plugin
	 */
	static std::string pretty_name() { return "MPI"; }

	mpi_plugin(Context& ctx, PC_tree_t config)
		: Plugin{ctx}
	{
		int world_rank;
		MPI_Comm_rank(MPI_COMM_WORLD, &world_rank);
		add_predefined(ctx, "MPI_COMM_WORLD_rank", &world_rank, Scalar_datatype::make(Scalar_kind::SIGNED, sizeof(int)));

		// logger requires MPI_COMM_WORLD.rank data
		set_up_logger(ctx, PC_get(config, ".logging"));

		// share the MPI_Comm datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm", [this](Context&, PC_tree_t) { return m_mpi_comm_datatype; });

		//load MPI_COMM_WORLD
		MPI_Comm comm_world = MPI_COMM_WORLD;
		add_predefined(ctx, "MPI_COMM_WORLD", &comm_world, m_mpi_comm_datatype);

		//load MPI_COMM_SELF
		MPI_Comm comm_self = MPI_COMM_SELF;
		add_predefined(ctx, "MPI_COMM_SELF", &comm_self, m_mpi_comm_datatype);

		//load MPI_COMM_NULL
		MPI_Comm comm_null = MPI_COMM_NULL;
		add_predefined(ctx, "MPI_COMM_NULL", &comm_null, m_mpi_comm_datatype);

		// share the MPI_Comm_f datatype, it does not duplicate its content (collective), only copies it!
		ctx.add_datatype("MPI_Comm_f", [this](Context&, PC_tree_t) { return m_mpi_comm_f_datatype; });

		//load MPI_COMM_WORLD_F
		MPI_Fint comm_world_f = MPI_Comm_c2f(MPI_COMM_WORLD);
		add_predefined(ctx, "MPI_COMM_WORLD_F", &comm_world_f, m_mpi_comm_f_datatype);

		//load MPI_COMM_SELF_F
		MPI_Fint comm_self_f = MPI_Comm_c2f(MPI_COMM_SELF);
		add_predefined(ctx, "MPI_COMM_SELF_F", &comm_self_f, m_mpi_comm_f_datatype);

		//load MPI_COMM_NULL_F
		MPI_Fint comm_null_f = MPI_Comm_c2f(MPI_COMM_NULL);
		add_predefined(ctx, "MPI_COMM_NULL_F", &comm_null_f, m_mpi_comm_f_datatype);

		MPI_Comm_transtype(ctx, config, m_mpi_comm_datatype, m_mpi_comm_f_datatype);

		ctx.logger().info("Plugin loaded successfully");
	}

	void set_up_logger(Context& ctx, PC_tree_t)
	{
		ctx.logger().add_pattern_global_block("MPI %{MPI_COMM_WORLD_rank:06d}");
		ctx.logger().evaluate_global_pattern(ctx);
	}

	void add_predefined(Context& ctx, const string& name, void* data, Datatype_sptr type)
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

	~mpi_plugin() { context().logger().info("Closing plugin"); }

	/** Transtypes C/Fortran communicator depending on specification tree
	 *
	 * \param ctx PDI context
	 * \param tree PC tree of mpi plugin
	 * \param C_mpi_comm_type C mpi comm datatype
	 * \param F_mpi_comm_type Fortran mpi comm datatype
	 */
	void MPI_Comm_transtype(
		Context& ctx,
		PC_tree_t tree,
		std::shared_ptr<const Scalar_datatype> C_mpi_comm_type,
		std::shared_ptr<const Scalar_datatype> F_mpi_comm_type
	)
	{
		PC_tree_t transtype_tree = PC_get(tree, ".transtype");
		if (!PC_status(transtype_tree)) {
			ctx.logger().warn("`transtype' key is deprecated when transtyping C/Fortran MPI communicator, please use Comm_c2f/Comm_f2c");
			if (*C_mpi_comm_type == *F_mpi_comm_type) {
				throw Value_error{"Transtype failed, cannot detect the direction of transtype"};
			}
			int len;
			PC_len(transtype_tree, &len);
			for (int i = 0; i < len; i++) {
				string src_desc = to_string(PC_get(transtype_tree, "{%d}", i));
				string dst_desc = to_string(PC_get(transtype_tree, "<%d>", i));
				if (*ctx.desc(src_desc).default_type()->evaluate(ctx) == *C_mpi_comm_type) {
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
			int len;
			PC_len(c2f_tree, &len);
			for (int i = 0; i < len; i++) {
				transtype_C_to_F(ctx, to_string(PC_get(c2f_tree, "{%d}", i)), to_string(PC_get(c2f_tree, "<%d>", i)));
			}
		}
		PC_tree_t f2c_tree = PC_get(tree, ".Comm_f2c");
		if (!PC_status(f2c_tree)) {
			int len;
			PC_len(f2c_tree, &len);
			for (int i = 0; i < len; i++) {
				transtype_F_to_C(ctx, to_string(PC_get(f2c_tree, "{%d}", i)), to_string(PC_get(f2c_tree, "<%d>", i)));
			}
		}
	}

	/** Transtype C mpi comm to Fortran mpi comm
	 *
	 * \param ctx PDI context
	 * \param c_comm_desc descriptor name of C mpi comm (source)
	 * \param f_comm_desc descriptor name of Fortran mpi comm (destination)
	 */
	void transtype_C_to_F(Context& ctx, const string& c_comm_desc, const string& fortran_comm_desc)
	{
		Datatype_sptr const & mpi_comm_f_type = m_mpi_comm_f_datatype;

		ctx.callbacks().add_data_callback(
			[&ctx, fortran_comm_desc, mpi_comm_f_type](const string& c_comm_desc, Ref ref) {
				ctx.logger().debug("Transtype `{}' to `{}' (C->F)", c_comm_desc, fortran_comm_desc);
				Ref fortran_comm_ref{new MPI_Fint, [](void* p) { delete static_cast<MPI_Fint*>(p); }, std::move(mpi_comm_f_type), true, true};
				if (Ref_r ref_r{ref}) {
					*static_cast<MPI_Fint*>(Ref_w{fortran_comm_ref}.get()) = MPI_Comm_c2f(*static_cast<const MPI_Comm*>(ref_r.get()));
					ctx.desc(fortran_comm_desc).share(fortran_comm_ref, false, false);
				} else {
					throw Right_error{"Cannot read `{}' data to transtype to fortran communicator", c_comm_desc};
				}
			},
			c_comm_desc
		);

		ctx.callbacks().add_data_remove_callback(
			[&ctx, fortran_comm_desc](const std::string& c_comm_desc, Ref c_comm_ref) {
				ctx.logger().debug("`{}' no longer available, reclaim transtyped `{}'", fortran_comm_desc, c_comm_desc);
				if (Ref_w c_comm_ref_w{c_comm_ref}) {
					ctx.logger().debug("Transtype back `{}' to `{}' (F->C)", fortran_comm_desc, c_comm_desc);
					if (Ref_r fortran_comm_ref_r = ctx.desc(fortran_comm_desc).ref()) {
						*static_cast<MPI_Comm*>(c_comm_ref_w.get()) = MPI_Comm_f2c(*static_cast<const MPI_Fint*>(fortran_comm_ref_r.get()));
					} else {
						throw Right_error{"Cannot read `{}' data", c_comm_desc};
					}
				}
				ctx.desc(fortran_comm_desc).release();
			},
			c_comm_desc
		);
	}

	/** Transtype Fortran mpi comm to C mpi comm
	 *
	 * \param ctx PDI context
	 * \param f_comm_desc descriptor name of Fortran mpi comm (source)
	 * \param c_comm_desc descriptor name of C mpi comm (destination)
	 */
	void transtype_F_to_C(Context& ctx, const string& fortran_comm_desc, const string& c_comm_desc)
	{
		Datatype_sptr mpi_comm_type = m_mpi_comm_datatype;

		ctx.callbacks().add_data_callback(
			[&ctx, c_comm_desc, mpi_comm_type](const string& fortran_comm_desc, Ref ref) {
				ctx.logger().debug("Transtype `{}' to `{}` (F->C)", fortran_comm_desc, c_comm_desc);
				Ref c_comm_ref{new MPI_Comm, [](void* p) { delete static_cast<MPI_Comm*>(p); }, move(mpi_comm_type), true, true};
				if (Ref_r ref_r{ref}) {
					MPI_Fint f_comm;
					set_val(f_comm, ref_r);
					*static_cast<MPI_Comm*>(Ref_w{c_comm_ref}.get()) = MPI_Comm_f2c(f_comm);
					ctx.desc(c_comm_desc).share(c_comm_ref, false, false);
				} else {
					throw Right_error{"Cannot read `{}' data to transtype to C communicator", fortran_comm_desc};
				}
			},
			fortran_comm_desc
		);


		ctx.callbacks().add_data_remove_callback(
			[&ctx, c_comm_desc](const string& fortran_comm_desc, Ref fortran_comm_ref) {
				ctx.logger().debug("`{}' no longer available, reclaim transtyped `{}'", fortran_comm_desc, c_comm_desc);
				if (Ref_w fortran_comm_ref_w{fortran_comm_ref}) {
					ctx.logger().debug("Transtype back `{}' to `{}' (C->F)", c_comm_desc, fortran_comm_desc);
					if (Ref_r c_comm_ref_r = ctx.desc(c_comm_desc).ref()) {
						set_val(fortran_comm_ref_w, MPI_Comm_c2f(*static_cast<const MPI_Comm*>(c_comm_ref_r.get())));
					} else {
						throw Right_error{"Cannot read `{}' data", c_comm_desc};
					}
				} else {
					ctx.logger().error("Cannot write `{}' communicator back", fortran_comm_desc);
				}
				ctx.desc(c_comm_desc).release();
			},
			fortran_comm_desc
		);
	}
};

} // namespace

PDI_PLUGIN(mpi)
