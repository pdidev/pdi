/*
 * SPDX-FileCopyrightText: 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * SPDX-FileCopyrightText: 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */

#ifndef PDI_TUPLE_DATATYPE_CASES_H_
#define PDI_TUPLE_DATATYPE_CASES_H_

#include <algorithm>
#include <iostream>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/scalar_datatype.h>
#include <pdi/tuple_datatype.h>

#include <vector>

/*
 * Struct prepared for TupleNotAlignedScalarsTest.
 * Order of the fields in Not_aligned_structure is important
 * to test padding sizes.
 */
struct TupleNotAlignedScalarsTest {
	//frame of the structure
	struct Not_aligned_structure {
		char c1;
		int i;
		char c2;
		unsigned int u;
		char c3;
		long l;
		char c4;
		unsigned long ul;
		char c5;
		float f;
		char c6;
		double d;
	};

	bool dense() { return true; }

	size_t datasize()
	{
		return 6 * sizeof(char) + sizeof(int) + sizeof(unsigned int) + sizeof(long) + sizeof(unsigned long) + sizeof(float) + sizeof(double);
	}

	size_t buffersize() { return sizeof(Not_aligned_structure); }

	size_t buffersize_after_densify() { return buffersize(); }

	size_t alignment()
	{
		return std::max({sizeof(char), sizeof(int), sizeof(unsigned int), sizeof(long), sizeof(unsigned long), sizeof(float), sizeof(double)});
	}

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	//tuple with scalar members (which are not aligned)
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Not_aligned_structure, c1), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, i), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int))},
			{offsetof(Not_aligned_structure, c2), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, u), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int))},
			{offsetof(Not_aligned_structure, c3), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, l), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long))},
			{offsetof(Not_aligned_structure, c4), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, ul), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long))},
			{offsetof(Not_aligned_structure, c5), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, f), PDI::Scalar_datatype::make(PDI::Scalar_kind::FLOAT, sizeof(float))},
			{offsetof(Not_aligned_structure, c6), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))},
			{offsetof(Not_aligned_structure, d), PDI::Scalar_datatype::make(PDI::Scalar_kind::FLOAT, sizeof(double))},
		},
		sizeof(Not_aligned_structure)
	);
};

/*
 * Struct prepared for TupleAlignedScalarsTest.
 * Order of the fields in Aligned_structure is important
 * to test padding sizes.
 */
struct TupleAlignedScalarsTest {
	struct Aligned_structure {
		int i;
		unsigned int u;
		long l;
		unsigned long ul;
		char c;
	};

	bool dense() { return true; }

	size_t datasize() { return sizeof(int) + sizeof(unsigned int) + sizeof(long) + sizeof(unsigned long) + sizeof(char); }

	size_t buffersize() { return sizeof(Aligned_structure); }

	size_t buffersize_after_densify()
	{
		return buffersize(); //datasize() + (alignof(long) - alignof(char));
	}

	size_t alignment() { return std::max({sizeof(int), sizeof(unsigned int), sizeof(long), sizeof(unsigned long), sizeof(char)}); }

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	//tuple with scalar members (which are aligned)
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Aligned_structure, i), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int))},
			{offsetof(Aligned_structure, u), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int))},
			{offsetof(Aligned_structure, l), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long))},
			{offsetof(Aligned_structure, ul), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long))},
			{offsetof(Aligned_structure, c), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char))}
		},
		sizeof(Aligned_structure)
	);
};

/*
 * Struct prepared for DenseArrayScalarsTest.
 * Order of the fields in Dense_array_structure is important
 * to test padding sizes.
 */
struct DenseArrayScalarsTest {
	struct Dense_array_structure {
		int i[3];
		unsigned int u[4];
		long l[5];
		unsigned long ul[6];
	};

	bool dense() { return true; }

	size_t datasize() { return 3 * sizeof(int) + 4 * sizeof(unsigned int) + 5 * sizeof(long) + 6 * sizeof(unsigned long); }

	size_t buffersize() { return sizeof(Dense_array_structure); }

	size_t buffersize_after_densify() { return datasize() + (alignof(long) - alignof(int)); }

	size_t alignment() { return std::max({sizeof(int), sizeof(unsigned int), sizeof(long), sizeof(unsigned long)}); }

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	//tuple with arrays (which are dense) containing scalars
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Dense_array_structure, i), PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), 3)},
			{offsetof(Dense_array_structure, u),
	         PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)), 4)},
			{offsetof(Dense_array_structure, l), PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), 5)},

			{offsetof(Dense_array_structure, ul),
	         PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)), 6)}
		},
		sizeof(Dense_array_structure)
	);
};

/*
 * Struct prepared for TupleSparseArrayScalarsTest.
 * Order of the fields in Sparse_structure is important
 * to test padding sizes.
 */
struct TupleSparseArrayScalarsTest {
	struct Sparse_array_structure {
		int i[100]; //buffer: 10 x 10; data: 4 x 4; start: (4, 4)
		long l[60]; //buffer: 3 x 20; data: 1 x 20; start (0, 1)
	};

	bool dense() { return false; }

	size_t datasize() { return 4 * 4 * sizeof(int) + 1 * 20 * sizeof(long); }

	size_t buffersize() { return sizeof(Sparse_array_structure); }

	size_t buffersize_after_densify() { return datasize(); }

	size_t alignment() { return std::max({sizeof(int), sizeof(long)}); }

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	//tuple with arrays (which are sparse) containing scalars
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Sparse_array_structure, i),
	         PDI::Array_datatype::
	             make(PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), 10, 3, 4), 10, 3, 4)},
			{offsetof(Sparse_array_structure, l),
	         PDI::Array_datatype::make(PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), 20), 3, 1, 1)}
		},
		sizeof(Sparse_array_structure)
	);
};

/*
 * Struct prepared for DenseTuplesInTupleTest.
 * Order of the fields in Dense_tuple is important
 * to test padding sizes.
 */
struct DenseTuplesInTupleTest {
	struct Dense_tuple {
		TupleAlignedScalarsTest::Aligned_structure aligned_scalar_tuple;
		DenseArrayScalarsTest::Dense_array_structure dense_array_tuple;
	};

	bool dense() { return true; }

	size_t datasize() { return scalar_structure_test->datasize() + array_structure_test->datasize(); }

	size_t buffersize() { return sizeof(Dense_tuple); }

	size_t buffersize_after_densify() { return scalar_structure_test->buffersize_after_densify() + array_structure_test->buffersize_after_densify(); }

	size_t alignment() { return std::max({scalar_structure_test->alignment(), array_structure_test->alignment()}); }

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	std::unique_ptr<TupleAlignedScalarsTest> scalar_structure_test{new TupleAlignedScalarsTest()};
	std::unique_ptr<DenseArrayScalarsTest> array_structure_test{new DenseArrayScalarsTest()};

	/*
	 * tuple with tuples (which are dense) which contain dense
	 * structures (scalars and arrays from previous tests)
	 */
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Dense_tuple, aligned_scalar_tuple), PDI::Datatype_sptr{scalar_structure_test->test_tuple()}},
			{offsetof(Dense_tuple, dense_array_tuple), PDI::Datatype_sptr{array_structure_test->test_tuple()}}
		},
		sizeof(Dense_tuple)
	);
};

/*
 * Struct prepared for SparseTuplesInTupleTest.
 * Order of the fields in Sparse_tuple is important
 * to test padding sizes.
 */
struct SparseTuplesInTupleTest {
	struct Sparse_tuple {
		TupleNotAlignedScalarsTest::Not_aligned_structure not_aligned_scalar_tuple;
		TupleSparseArrayScalarsTest::Sparse_array_structure sparse_array_tuple;
	};

	bool dense() { return false; }

	size_t datasize() { return scalar_structure_test->datasize() + array_structure_test->datasize(); }

	size_t buffersize() { return sizeof(Sparse_tuple); }

	size_t buffersize_after_densify() { return scalar_structure_test->buffersize_after_densify() + array_structure_test->buffersize_after_densify(); }

	size_t alignment() { return std::max({scalar_structure_test->alignment(), array_structure_test->alignment()}); }

	std::shared_ptr<PDI::Tuple_datatype> test_tuple() { return m_test_tuple; }

	std::unique_ptr<TupleNotAlignedScalarsTest> scalar_structure_test{new TupleNotAlignedScalarsTest()};
	std::unique_ptr<TupleSparseArrayScalarsTest> array_structure_test{new TupleSparseArrayScalarsTest()};

	/*
	 * tuple with tuples (which are dense) which contain sparse
	 * structures (scalars and arrays from previous tests)
	 */
	std::shared_ptr<PDI::Tuple_datatype> m_test_tuple = PDI::Tuple_datatype::make(
		std::vector<PDI::Tuple_datatype::Element>{
			{offsetof(Sparse_tuple, not_aligned_scalar_tuple), PDI::Datatype_sptr{scalar_structure_test->test_tuple()}},
			{offsetof(Sparse_tuple, sparse_array_tuple), PDI::Datatype_sptr{array_structure_test->test_tuple()}}
		},
		sizeof(Sparse_tuple)
	);
};

#endif // PDI_TUPLE_DATATYPE_TEST_H_
