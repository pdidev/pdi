/*******************************************************************************
 * Copyright (c) 2015, Julien Bigot - CEA (julien.bigot@cea.fr)
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

#include "config.h"

#include <cassert>
#include <cstdint>
#include <memory>
#include <string>

#include "pdi.h"
#include "pdi/paraconf_wrapper.h"
#include "pdi/status.h"
#include "pdi/value.h"

#include "pdi/datatype.h"

namespace PDI
{

namespace
{

using std::string;
using std::unique_ptr;
using std::vector;

/** Descriptor of a buffer and its content
 */
struct Buffer_descriptor {

	struct Dimension {
		/* Size of the array in this dimension in term of sub-elements
		 *
		 * The stride for a given dimension is the product of the element size by
		 * all size of dimensions with a lower index.
		 */
		size_t m_size;
		
		/* Index of the first sub-element that is actually part of the array in
		 * this dimension
		 */
		size_t m_start;
		
		/// Number of elements contained in the array in this dimension,
		size_t m_subsize;
		
	};
	
	/* Array of dimensions of the array from outer to inner dim
	 *
	 * The slowest (outer) dimension is m_dimensions[0], the fastest
	 * m_dimensions[m_dimensions.size()-1]
	 */
	vector<Dimension> m_dimensions;
	
	/// Size of the dense block contained in the array
	size_t dense_size;
	
	static Buffer_descriptor bufdesc(const Datatype &type);
	
	static Buffer_descriptor bufdesc(Scalar_datatype type)
	{
		Buffer_descriptor result;
		switch (type) {
		case PDI_T_INT8:
			result.dense_size = sizeof(int8_t);
			break;
		case PDI_T_INT16:
			result.dense_size = sizeof(int16_t);
			break;
		case PDI_T_INT32:
			result.dense_size = sizeof(int32_t);
			break;
		case PDI_T_INT64:
			result.dense_size = sizeof(int64_t);
			break;
		case PDI_T_FLOAT:
			result.dense_size = sizeof(float);
			break;
		case PDI_T_DOUBLE:
			result.dense_size = sizeof(double);
			break;
		case PDI_T_LONG_DOUBLE:
			result.dense_size = sizeof(long double);
			break;
		default:
			throw Error{PDI_ERR_CONFIG, "Unknown PDI type: #%d", type};
		}
		
		return result;
	}
	
	static Buffer_descriptor bufdesc(const Array_datatype &type)
	{
		Buffer_descriptor result = bufdesc(type.type);
		for (auto &&dim : type.m_dimensions) {
			size_t size = dim.m_size;
			size_t subsize = dim.m_subsize;
			if ((size == subsize) && result.m_dimensions.empty()) {   // dense case
				result.dense_size *= size;
			} else { // sparse case
				//TODO: handle the dense case here
				size_t start = dim.m_start;
				result.m_dimensions.push_back({size, start, subsize});
			}
		}
		return result;
	}
	
	size_t datasize() const
	{
		size_t result = dense_size;
		for (auto &&dim : m_dimensions) {
			result *= dim.m_subsize;
		}
		return result;
	}
	
	size_t buffersize() const
	{
		size_t result = dense_size;
		for (auto &&dim : m_dimensions) {
			result *= dim.m_size;
		}
		return result;
	}
	
};

/// ordering of array
typedef enum PDI_order_e {
	PDI_ORDER_C,
	PDI_ORDER_FORTRAN
} PDI_order_t;

/** Return a reordered index, i.e. a C style index given either a C for Fortran
 *  style one
 * \param ordered_index the initial C or Fortran ordered index
 * \param order the style of the initial index (C/Fortran)
 * \param size the size of the dimension the index accesses
 * \return the reordered index (C style)
 */
int ridx(int ordered_index, PDI_order_t order, int size)
{
	if (order == PDI_ORDER_FORTRAN) return size - ordered_index - 1;
	return ordered_index;
}

Buffer_descriptor Buffer_descriptor::bufdesc(const Datatype &type)
{
	switch (type.kind) {
	case PDI_K_SCALAR:
		return bufdesc(type.c.scalar);
	case PDI_K_ARRAY:
		return bufdesc(*type.c.array);
	case PDI_K_STRUCT:
		throw Error{PDI_ERR_IMPL, "Not implemented yet"};
	}
	assert(false);
}

PDI_status_t scalar_datatype_load(PC_tree_t node, Scalar_datatype *type)
{
	string tname;
	long kind = 0;
	try {
		tname = to_string(PC_get(node, ".type"));
		kind = to_long(PC_get(node, ".kind"));
	} catch (const Error &) {
		tname = to_string(node);
	}
	
	// For Fortran, we assume kind means number of bytes... TODO: autodetect
	if (tname == "char" && kind == 0) {  // C char
		*type = PDI_T_INT8;
	} else if (tname == "int" && kind == 0) {  // C int
		switch (sizeof(int)) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			throw Error{PDI_ERR_VALUE, "Unsupported int size: %d", sizeof(int) * 8};
		}
	} else if (tname == "int8"  && kind == 0) {  // C int8
		*type = PDI_T_INT8;
	} else if (tname == "int16"  && kind == 0) {  // C int16
		*type = PDI_T_INT16;
	} else if (tname == "int32"  && kind == 0) {  // C int32
		*type = PDI_T_INT32;
	} else if (tname == "int64"  && kind == 0) {  // C int64
		*type = PDI_T_INT64;
	} else if (tname == "float"  && kind == 0) {  // C float
		*type = PDI_T_FLOAT;
	} else if (tname == "double"  && kind == 0) {  // C double
		*type = PDI_T_DOUBLE;
	} else if (tname == "character") {     // Fortran character
		if (kind == 0) kind = PDI_CHARACTER_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			throw Error{PDI_ERR_VALUE, "Invalid kind for character: `%s'", tname.c_str()};
		}
	} else if (tname == "integer") {     // Fortran integer
		if (kind == 0) kind = PDI_INTEGER_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			throw Error{PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname.c_str()};
		}
	} else if (tname == "logical") {     // Fortran integer
		if (kind == 0) kind = PDI_LOGICAL_DEFAULT_KIND;
		switch (kind) {
		case 1:
			*type = PDI_T_INT8;
			break;
		case 2:
			*type = PDI_T_INT16;
			break;
		case 4:
			*type = PDI_T_INT32;
			break;
		case 8:
			*type = PDI_T_INT64;
			break;
		default:
			throw Error{PDI_ERR_VALUE, "Invalid kind for integer: `%s'", tname.c_str()};
		}
	} else if (tname == "real") {     // Fortran real
		if (kind == 0) kind = PDI_REAL_DEFAULT_KIND;
		switch (kind) {
		case 4:
			*type = PDI_T_FLOAT;
			break;
		case 8:
			*type = PDI_T_DOUBLE;
			break;
		case 16:
			*type = PDI_T_LONG_DOUBLE;
			break;
		default:
			throw Error{PDI_ERR_VALUE, "Invalid kind for real: `%s'", tname.c_str()};
		}
	} else {
		throw Error{PDI_ERR_VALUE, "Invalid scalar type: `%s'", tname.c_str()};
	}
	
	return PDI_OK;
}

PDI_status_t array_datatype_load(PC_tree_t node, Array_datatype *type)
{
	Array_datatype res_type;
	
	// Order: C or fortran ordering, default is C
	PDI_order_t order = PDI_ORDER_C;
	{
		string order_str;
		try {
			order_str = to_string(PC_get(node, ".order"));
		} catch (const Error &) {
			order_str = "c";
		}
		if (order_str == "c" || order_str == "C") {
			order = PDI_ORDER_C;
		} else if (order_str == "fortran" || order_str == "Fortran") {
			order = PDI_ORDER_FORTRAN;
		} else {
			throw Error{PDI_ERR_CONFIG, "Incorrect array ordering: `%s'", order_str.c_str()};
		}
	}
	
	int ndims;
	try { // multi dim array
		ndims = len(PC_get(node, ".sizes"));
		for (int ii = 0; ii < ndims; ++ii) {
			res_type.m_dimensions.emplace_back(to_string(PC_get(node, ".sizes[%d]", ridx(ii, order, ndims))));
		}
	} catch (const Error &) { // else single dim array
		ndims = 1;
		res_type.m_dimensions.emplace_back(to_string(PC_get(node, ".size")));
	}
	
	int nsubdim;
	try {
		nsubdim = PDI::len(PC_get(node, ".subsizes"));
	} catch (const Error &) {
		nsubdim = 0;
	}
	if (nsubdim && nsubdim != ndims) {
		throw Error{PDI_ERR_CONFIG, "Invalid size for subsizes %d, %d expected", len, ndims};
	}
	for (int ii = 0; ii < nsubdim; ++ii) {
		res_type.m_dimensions[ii].m_subsize = to_string(PC_get(node, ".subsizes[%d]", ridx(ii, order, ndims)));
	}
	
	int nstart;
	try {
		nstart = len(PC_get(node, ".starts"));
	} catch (const Error &) {
		nstart = 0;
	}
	if (nstart && nstart != ndims) {
		throw Error{PDI_ERR_CONFIG, "Invalid size for starts %d, %d expected", nstart, ndims};
	}
	for (int ii = 0; ii < nstart; ++ii) {
		res_type.m_dimensions[ii].m_start = to_string(PC_get(node, ".starts[%d]", ridx(ii, order, ndims)));
	}
	
	PDI_datatype_load(&res_type.type, PC_get(node, ".type"));
	
	new (type) Array_datatype{res_type};
	return PDI_OK;
}

PDI_status_t struct_datatype_load(PC_tree_t, Record_datatype *)
{
	throw Error{PDI_ERR_IMPL, "Structure support not implemented yet"};
}

/** Copies a subpart of from content into to
 *
 * \param dim the number of the subdimension to copy
 * \param from buffer holding the from data
 * \param from_size size of the data in from
 * \param to buffer holding the to data
 * \param to_size size of the data in to
 */
PDI_status_t buffer_do_copy(void *to, const Buffer_descriptor *to_desc, const void *from, const Buffer_descriptor *from_desc)
{
	size_t from_size = from_desc->datasize();
	size_t to_size = to_desc->datasize();
	
	// and now for a little defensive programming
	if (from_size != to_size) {
		throw Error{PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size};
	}
	
	if (from_desc->m_dimensions.empty() && to_desc->m_dimensions.empty()) {     // dense to dense
		if (from_desc->dense_size != to_desc->dense_size) {
			throw Error{PDI_ERR_TYPE, "Incompatible type size for dense copy: %ld vs. %ld .", (long)from_desc->dense_size, (long)to_desc->dense_size};
		}
		memcpy(to, from, from_desc->dense_size);
		
	} else { // sparse involved, explicit loop
		size_t nblocks;
		Buffer_descriptor from_blockdesc;
		size_t from_start;
		Buffer_descriptor to_blockdesc;
		size_t to_start;
		
		if (from_desc->m_dimensions.empty()) {   // dense to sparse
			nblocks = to_desc->m_dimensions.front().m_subsize;
			
			from_blockdesc = *from_desc;
			from_blockdesc.dense_size /= to_desc->m_dimensions.front().m_subsize;
			from_start = 0;
			
			// remove slowest dim
			to_blockdesc = {{to_desc->m_dimensions.begin() + 1, to_desc->m_dimensions.end()}, to_desc->dense_size};
			to_start = to_desc->m_dimensions.front().m_start;
			
		} else if (to_desc->m_dimensions.empty()) {   // sparse to dense
			nblocks = from_desc->m_dimensions.front().m_subsize;
			
			// remove slowest dim
			from_blockdesc = {{from_desc->m_dimensions.begin() + 1, from_desc->m_dimensions.end()}, from_desc->dense_size};
			from_start = from_desc->m_dimensions.front().m_start;
			
			to_blockdesc = *to_desc;
			to_blockdesc.dense_size /= from_desc->m_dimensions.front().m_subsize;
			to_start = 0;
			
		} else { // sparse to sparse
			if (from_desc->m_dimensions.front().m_subsize != to_desc->m_dimensions.front().m_subsize) {
				throw Error{PDI_ERR_TYPE, "Incompatible array type size for copy."};
			}
			nblocks = from_desc->m_dimensions.front().m_subsize;
			
			// remove slowest dim
			from_blockdesc = {{from_desc->m_dimensions.begin() + 1, from_desc->m_dimensions.end()}, from_desc->dense_size};
			from_start = from_desc->m_dimensions.front().m_start;
			
			// remove slowest dim
			to_blockdesc = {{to_desc->m_dimensions.begin() + 1, to_desc->m_dimensions.end()}, to_desc->dense_size};
			to_start = to_desc->m_dimensions.front().m_start;
			
		}
		size_t to_stride = to_blockdesc.buffersize();
		size_t from_stride = from_blockdesc.buffersize();
		from = (char *)from + from_start * from_stride;
		to = (char *)to + to_start * to_stride;
		for (size_t ii = 0; ii < nblocks; ++ii) {
			buffer_do_copy(to, &to_blockdesc, from, &from_blockdesc);
			from = (char *)from + from_stride;
			to = (char *)to + to_stride;
		}
	}
	
	return PDI_OK;
}

} // namespace <anonymous>


const Datatype PDI_UNDEF_TYPE;


PDI_status_t PDI_datatype_init_scalar(Datatype *dest, Scalar_datatype value)
{
	dest->kind = PDI_K_SCALAR;
	dest->c.scalar = value;
	return PDI_OK;
}

Datatype::Datatype(const Datatype &from):
	kind{from.kind}
{
	switch (kind) {
	case PDI_K_SCALAR:
		c.scalar = from.c.scalar;
		break;
	case PDI_K_ARRAY:
		c.array = new Array_datatype{*from.c.array};
		break;
	case PDI_K_STRUCT:
		throw Error{PDI_UNAVAILABLE, "Structure support not implemented"};
		break;
	}
}

Datatype::Datatype(Datatype &&from):
	kind{from.kind},
	c{from.c}
{
	from.kind = PDI_K_SCALAR;
	from.c.scalar = PDI_T_UNDEF;
}

Datatype &Datatype::operator=(const Datatype &from)
{
	kind = from.kind;
	switch (kind) {
	case PDI_K_SCALAR:
		c.scalar = from.c.scalar;
		break;
	case PDI_K_ARRAY:
		c.array = new Array_datatype{*from.c.array};
		break;
	case PDI_K_STRUCT:
		throw Error{PDI_UNAVAILABLE, "Structure support not implemented"};
		break;
	}
	return *this;
}

Datatype &Datatype::operator=(Datatype &&from)
{
	kind = from.kind;
	c = from.c;
	from.kind = PDI_K_SCALAR;
	from.c.scalar = PDI_T_UNDEF;
	return *this;
}

Datatype Datatype::densify() const
{
	switch (kind) {
	case PDI_K_SCALAR: {
		Datatype result ;
		PDI_datatype_init_scalar(&result, c.scalar);
		return result;
	}
	case PDI_K_ARRAY: {
		Datatype result;
		result.kind = PDI_K_ARRAY;
		result.c.array = new Array_datatype{c.array->densify()};
		return result;
	}
	default: {}
	}
	throw Error{PDI_ERR_IMPL, "Structure support not implemented"};
}

PDI_status_t PDI_datatype_load(Datatype *type, PC_tree_t node)
{
	// load as a scalar by default
	Datatype_kind kind = PDI_K_SCALAR;
	
	// size or sizes => array
	{
		PC_errhandler_t pc_handler = PC_errhandler(PC_NULL_HANDLER); // aka PC_try
		if (!PC_status(PC_get(node, ".size"))) kind = PDI_K_ARRAY;
		if (!PC_status(PC_get(node, ".sizes"))) kind = PDI_K_ARRAY;
		PC_errhandler(pc_handler); // aka PC_end_try
	}
	
	switch (kind) {
	case PDI_K_ARRAY: {// load the type as an array
		unique_ptr<Array_datatype> array{new Array_datatype};
		array_datatype_load(node, array.get());
		type->c.array = array.release();
		break;
	}
	case PDI_K_STRUCT: { // load the type as a structure
		unique_ptr<Record_datatype> struct_{new Record_datatype};
		struct_datatype_load(node, struct_.get());
		type->c.struct_ = struct_.release();
		break;
	}
	case PDI_K_SCALAR: { // load the type as a scalar
		scalar_datatype_load(node, &(type->c.scalar));
		break;
	}
	}
	type->kind = kind;
	return PDI_OK;
}

Datatype::~Datatype()
{
	switch (kind) {
	case PDI_K_ARRAY: {
		delete c.array;
	} break;
	case PDI_K_STRUCT: {
		assert(false && "Structs not implemented yet");
	} break;
	case PDI_K_SCALAR: {
		// nothing to do here
	} break;
	}
}

bool Datatype::is_dense() const
{
	switch (kind) {
	case PDI_K_SCALAR:
		return true;
	case PDI_K_ARRAY:
		return c.array->is_dense();
	case PDI_K_STRUCT:
		throw Error{PDI_ERR_IMPL, "Struct support not implemented"};
	}
	assert(false);
}

size_t Datatype::datasize() const
{
	return Buffer_descriptor::bufdesc(*this).datasize();
}

size_t Datatype::buffersize() const
{
	return Buffer_descriptor::bufdesc(*this).buffersize();
}

bool Array_datatype::is_dense() const
{
	for (auto &&dim : m_dimensions) {
		if (dim.m_size.to_long() != dim.m_subsize.to_long()) {
			return false;
		}
	}
	return type.is_dense();
}

Array_datatype Array_datatype::densify() const
{
	Array_datatype result;
	result.type = type.densify();
	for (auto &&dim : m_dimensions) {
		result.m_dimensions.push_back({dim.m_subsize});
	}
	return result;
}

PDI_status_t PDI_buffer_copy(void *to, const Datatype *to_type, const void *from, const Datatype *from_type)
{
	Buffer_descriptor from_desc = Buffer_descriptor::bufdesc(*from_type);
	Buffer_descriptor to_desc = Buffer_descriptor::bufdesc(*to_type);
	size_t from_size = from_desc.datasize();
	size_t to_size = to_desc.datasize();
	
	// and now for a little defensive programming
	if (from_size != to_size) {
		throw Error{PDI_ERR_TYPE, "Incompatible types for copy: %ld Bytes -> %ld Bytes", (long)from_size, (long)to_size};
	}
	
	buffer_do_copy(to, &to_desc, from, &from_desc);
	return PDI_OK;
}

}
