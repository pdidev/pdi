/*******************************************************************************
 * Copyright (C) 2020-2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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
	enum class Lang {INVALID, C, FORTRAN};
	PDI::Context& m_ctx;
	std::unordered_map<std::string, std::string> m_descs;
	std::unordered_set<PDI::Ref> m_refs;
	
	/// the MPI_Comm datatype
	const PDI::Datatype& m_mpi_comm_datatype;
	
	// create the MPI_Comm_f datatype
	const PDI::Datatype& m_mpi_comm_f_datatype;
public:
	MPI_Comm_transtyper ( PDI::Context& ctx, PC_tree_t tree, const PDI::Datatype& mpi_comm_datatype, const PDI::Datatype& mpi_comm_f_datatype ) :
		m_ctx{ctx}
		, m_mpi_comm_datatype{mpi_comm_datatype}
		, m_mpi_comm_f_datatype{mpi_comm_f_datatype}
	{
		using std::string;
		using PDI::len;
		using PDI::to_string;
		
		if ( !PC_status ( tree ) ) {
			int nb_key = len ( tree );
			for ( int key_id = 0; key_id < nb_key; key_id++ ) {
				auto&& result = m_descs.emplace ( to_string ( PC_get ( tree, "{%d}", key_id ) ), to_string ( PC_get ( tree, "<%d>", key_id ) ) );
				if ( result.second ) {
					m_ctx.logger()->debug ( "Add desc `{}' to transtype to `{}' on share", result.first->first, result.first->second );
				} else {
					m_ctx.logger()->warn ( "Duplicate desc name (`{}') in `transtype'", result.first->first );
				}
			}
		}
	}
	
	Lang lang ( const PDI::Datatype& datatype )
	{
		using PDI::Impl_error;
		if ( datatype == m_mpi_comm_datatype ) {
			return Lang::C;
		} else {
			//TODO: Ugly workaround, we consider anything not exactly a C datatype as a Fortran one...
			return Lang::FORTRAN;
		}
	}
	
	const PDI::Datatype& type ( const Lang lang )
	{
		using PDI::Impl_error;
		
		switch ( lang ) {
		case Lang::C:
			return m_mpi_comm_datatype;
		case Lang::FORTRAN:
			return m_mpi_comm_f_datatype;
		default:
			throw Impl_error{"Invalid MPI language for transtype"};
		}
	}
	
	void transtype ( const void* source, void* dest, const Lang source_lang )
	{
		using PDI::Impl_error;
		
		switch ( source_lang ) {
		case Lang::C:
			*static_cast<MPI_Fint*> ( dest ) = MPI_Comm_c2f ( *static_cast<const MPI_Comm*> ( source ) );
			m_ctx.logger()->debug ( "Transtyped (C->F) `{}' to `{}'", *static_cast<void const* const*> ( source ), *static_cast<MPI_Fint*> ( dest ) );
			break;
		case Lang::FORTRAN:
			*static_cast<MPI_Comm*> ( dest ) = MPI_Comm_f2c ( *static_cast<const MPI_Fint*> ( source ) );
			m_ctx.logger()->debug ( "Transtyped (F->C) `{}' to `{}'", *static_cast<MPI_Fint const*> ( source ), *static_cast<void const* const*> ( dest ) );
			break;
		default:
			throw Impl_error{"Invalid MPI communicator in transtype"};
		}
	}
	
	static Lang trans ( Lang orig_lang )
	{
		switch ( orig_lang ) {
		case Lang::C:
			return Lang::FORTRAN;
		case Lang::FORTRAN:
			return Lang::C;
		default:
			return Lang::INVALID;
		}
	}
	
	PDI::Ref transtype ( PDI::Ref ref, const Lang source_lang )
	{
		using PDI::Impl_error;
		using PDI::Ref;
		using PDI::Ref_r;
		using PDI::Ref_w;
		
		const Lang dest_lang = trans ( source_lang );
		
		size_t dest_len = 0;
		switch ( dest_lang ) {
		case Lang::FORTRAN:
			dest_len = sizeof ( MPI_Fint );
			break;
		case Lang::C:
			dest_len = sizeof ( MPI_Comm );
			break;
		default:
			throw Impl_error{"Invalid MPI language to transtype"};
		}
		void* const dest_data = ::operator new ( dest_len );
		
		bool writable = false;
		if ( Ref_w wref = ref ) {
			writable = true;
		}
		
		bool readable = false;
		if ( Ref_r rref = ref ) {
			readable = true;
			transtype ( rref.get(), dest_data, source_lang );
		}
		
		return Ref{dest_data, [] ( void* ptr ) {::operator delete ( ptr );}, type ( dest_lang ).clone_type(), readable, writable};
	}
	
	void data ( const char* orig_name_cstr, PDI::Ref orig_ref )
	{
		using std::unique_ptr;
		using PDI::Ref;
		using PDI::Ref_w;
		
		const std::string orig_name = orig_name_cstr;
		
		auto&& descs_it = m_descs.find ( orig_name );
		if ( descs_it == m_descs.end() ) {
			return;
		}
		const std::string trans_name = descs_it->second;
		
		auto&& trans_desc = m_ctx[trans_name];
		
		const Lang orig_lang = lang ( orig_ref.type() );
		
		m_ctx.logger()->debug ( "Transtype `{}' to `{}'", orig_name, trans_name );
		trans_desc.share ( transtype ( orig_ref, orig_lang ), false, false );
		
		m_refs.emplace ( orig_ref ).first->on_nullify ( [this, orig_name, orig_lang, &trans_desc] ( Ref orig_ref ) {
			m_ctx.logger()->debug ( "`{}' no longer available, reclaim transtyped desc (`{}')", orig_name, trans_desc.name() );
			unique_ptr<void, void ( * ) ( void* ) > trans_data{trans_desc.reclaim(), [] ( void* d )
			{
				::operator delete ( d );
			}};
			if ( Ref_w orig_wref = orig_ref ) {
				m_ctx.logger()->debug ( "Transtype back `{}' to `{}'", trans_desc.name(), orig_name );
				transtype ( trans_data.get(), orig_wref.get(), trans(orig_lang) );
			}
			m_refs.erase ( orig_ref );
		} );
	}
};

} // namespace <anonymous>

#endif // MPI_COMM_TRANSTYPER_H_
