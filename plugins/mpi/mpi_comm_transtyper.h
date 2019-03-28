/*******************************************************************************
 * Copyright (C) 2019 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef MPI_COMM_TRANSTYPER_H_
#define MPI_COMM_TRANSTYPER_H_

#include <mpi.h>

#include <memory>
#include <string>
#include <unordered_map>
#include <unordered_set>

#include <pdi/context.h>
#include <pdi/data_descriptor.h>
#include <pdi/datatype.h>
#include <pdi/logger.h>
#include <spdlog/spdlog.h>

namespace {

class MPI_Comm_transtyper 
{
    enum class MPI_Comm_type {INVALID, C, FORTRAN};
    PDI::Context& m_ctx;
    std::unordered_map<std::string, std::string> m_descs;
	std::unordered_set<PDI::Ref> m_refs;
public:
    MPI_Comm_transtyper(PDI::Context& ctx, PC_tree_t tree):
    m_ctx{ctx}
    {
        using std::string;
        using PDI::len;
        using PDI::to_string;

		if (!PC_status(tree)) {
			int nb_key = len(tree);
			for (int key_id = 0; key_id < nb_key; key_id++) {
				auto&& result = m_descs.emplace(to_string(PC_get(tree, "{%d}", key_id)), to_string(PC_get(tree, "<%d>", key_id)));
                if (result.second) {
                    m_ctx.logger()->debug("(MPI) Add desc `{}' to transtype to `{}' on share", result.first->first, result.first->second);
                } else {
                    m_ctx.logger()->warn("(MPI) Duplicate desc name (`{}') in `transtype'", result.first->first);
                }
			}
		}
    }

    MPI_Comm_type get_mpi_comm_type(const PDI::Datatype& datatype)
    {
        static const auto&& mpi_comm_c = m_ctx.datatype(PC_parse_string("MPI_Comm"))->evaluate(m_ctx);
        static const auto&& mpi_comm_f = m_ctx.datatype(PC_parse_string("MPI_Comm_f"))->evaluate(m_ctx);

        if (datatype == *mpi_comm_c) {
            return MPI_Comm_type::C;
        } else if (datatype == *mpi_comm_f) {
            return MPI_Comm_type::FORTRAN;
        } else {
            return MPI_Comm_type::INVALID;
        }
    }

    void convert_comm(const void* source, MPI_Comm_type source_type, void* dest, MPI_Comm_type dest_type)
    {
        using PDI::Error;

        if (dest_type == MPI_Comm_type::C) {
            if (source_type == MPI_Comm_type::C)
                *static_cast<MPI_Comm*>(dest) = *static_cast<const MPI_Comm*>(source);
            else if (source_type == MPI_Comm_type::FORTRAN)
                *static_cast<MPI_Comm*>(dest) = MPI_Comm_f2c(*static_cast<const MPI_Fint*>(source));
            else
                throw Error{PDI_ERR_IMPL, "(MPI) Converting unknown MPI communicator type"};
        } else if (dest_type == MPI_Comm_type::FORTRAN) {
            if (source_type == MPI_Comm_type::C)
                *static_cast<MPI_Fint*>(dest) = MPI_Comm_c2f(*static_cast<const MPI_Comm*>(source));
            else if (source_type == MPI_Comm_type::FORTRAN)
                *static_cast<MPI_Fint*>(dest) = *static_cast<const MPI_Fint*>(source);
            else
                throw Error{PDI_ERR_IMPL, "(MPI) Converting unknown MPI communicator type"};
        } else {
            throw Error{PDI_ERR_IMPL, "(MPI) Converting to unknown MPI communicator type"};
        }
    }

    PDI::Ref create_transtyped_ref(PDI::Ref ref, const PDI::Datatype& source_datatype, const PDI::Datatype& transtyped_datatype)
    {
        using PDI::Ref;
        using PDI::Ref_r;
        using PDI::Ref_w;

        bool writable = false;
        if (Ref_w wref = ref) writable = true;

        auto source_mpi_comm_type = get_mpi_comm_type(source_datatype);
        auto transtyped_mpi_comm_type = get_mpi_comm_type(transtyped_datatype);
        
        if (source_mpi_comm_type != MPI_Comm_type::INVALID) {
            if (transtyped_mpi_comm_type == MPI_Comm_type::C) {
                if (Ref_r rref = ref) {
                    void* ptr = new MPI_Comm;
                    convert_comm(rref.get(), source_mpi_comm_type, ptr, transtyped_mpi_comm_type);
                    return Ref{ptr, [](void* ptr) {delete static_cast<MPI_Comm*>(ptr);}, transtyped_datatype.clone_type(), true, writable};
                } else {
                    return Ref{new MPI_Comm, [](void* ptr) {delete static_cast<MPI_Comm*>(ptr);}, transtyped_datatype.clone_type(), false, writable};
                }
            } else if (transtyped_mpi_comm_type == MPI_Comm_type::FORTRAN) {
                if (Ref_r rref = ref) {
                    void* ptr = new MPI_Fint;
                    convert_comm(rref.get(), source_mpi_comm_type, ptr, transtyped_mpi_comm_type);
                    return Ref{ptr, [](void* ptr) {delete static_cast<MPI_Fint*>(ptr);}, transtyped_datatype.clone_type(), true, writable};
                } else {
                    return Ref{new MPI_Fint, [](void* ptr) {delete static_cast<MPI_Fint*>(ptr);}, transtyped_datatype.clone_type(), false, writable};
                }
            }
        }

        return {};
    }

    void data(const char* name, PDI::Ref ref) 
    {
        using std::unique_ptr;
        using PDI::Context;
        using PDI::Ref;
        using PDI::Ref_r;
        using PDI::Ref_w;

        auto&& descs_it = m_descs.find(name);
		if (descs_it == m_descs.end()) return;

        auto&& source_desc = m_ctx[descs_it->first];
        auto&& transtype_desc = m_ctx[descs_it->second];
        auto&& source_type_uptr = source_desc.default_type()->evaluate(m_ctx);
        auto&& transtype_type_uptr = transtype_desc.default_type()->evaluate(m_ctx);

        m_ctx.logger()->debug("(MPI) Transtype `{}' to `{}'", descs_it->first, descs_it->second);
        auto&& transtyped_ref = create_transtyped_ref(ref, *source_type_uptr, *transtype_type_uptr);
        if (!transtyped_ref) {
            m_ctx.logger()->warn("(MPI) Transtype from `{}' datatype to `{}' datatype is not supported", descs_it->first, descs_it->second);
            return;
        }
        
        if (*source_type_uptr == *transtype_type_uptr) {
            m_ctx.logger()->warn("(MPI) Descs `{}' and `{}' have the same datatype", descs_it->first, descs_it->second);
        }

        m_ctx.logger()->debug("(MPI) Share transtyped desc (`{}')", descs_it->second);
        transtype_desc.share(transtyped_ref, false, false);
        
        auto&& ref_copy = m_refs.emplace(ref).first;
        ref_copy->on_nullify([this, &source_desc, &transtype_desc](Ref r) {
            if (Ref_w wref = r) {
                this->m_ctx.logger()->debug("(MPI) `{}' no longer available, reclaim transtyped desc (`{}')", source_desc.name(), transtype_desc.name());
                auto transtype_mpi_comm_type = get_mpi_comm_type(*transtype_desc.default_type()->evaluate(this->m_ctx));
                unique_ptr<void, void(*)(void*)> ptr{nullptr, [](void*){}};
                if (transtype_mpi_comm_type == MPI_Comm_type::C) {
                    ptr = unique_ptr<void, void(*)(void*)>{transtype_desc.reclaim(), [](void* ptr) {delete static_cast<MPI_Comm*>(ptr);}};
                } else if (transtype_mpi_comm_type == MPI_Comm_type::FORTRAN) {
                    ptr = unique_ptr<void, void(*)(void*)>{transtype_desc.reclaim(), [](void* ptr) {delete static_cast<MPI_Fint*>(ptr);}};
                } else {
                    throw PDI::Error{PDI_ERR_IMPL, "(MPI) Unexpected MPI Communicator when transtyping back"};
                }
                this->m_ctx.logger()->debug("(MPI) Transtype back `{}' to `{}'", transtype_desc.name(), source_desc.name());    
                convert_comm(ptr.get(), transtype_mpi_comm_type, wref.get(), get_mpi_comm_type(*source_desc.default_type()->evaluate(this->m_ctx)));
            } else {
                this->m_ctx.logger()->debug("(MPI) `{}' no longer available, release transtyped desc (`{}')", source_desc.name(), transtype_desc.name());
                transtype_desc.release();
            }
            this->m_refs.erase(r);
        });	
    }
};

} // namespace <anonymous>

#endif // MPI_COMM_TRANSTYPER_H_