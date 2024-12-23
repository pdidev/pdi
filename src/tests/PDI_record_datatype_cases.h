/*******************************************************************************
 * Copyright (C) 2021 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2018 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef PDI_RECORD_DATATYPE_CASES_H_
#define PDI_RECORD_DATATYPE_CASES_H_

#include <algorithm>

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/record_datatype.h>
#include <pdi/scalar_datatype.h>

#include <vector>

/*
 * Struct prepared for NotAlignedScalarsTest.
 * Order of the fields in Not_aligned_structure is important
 * to test padding sizes.
 */
struct NotAlignedScalarsTest {
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

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	//record with scalar members (which are not aligned)
	std::shared_ptr<PDI::Record_datatype> m_test_record = PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Not_aligned_structure, c1), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c1"},
			{offsetof(Not_aligned_structure, i), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), "i"},
			{offsetof(Not_aligned_structure, c2), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c2"},
			{offsetof(Not_aligned_structure, u), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)), "u"},
			{offsetof(Not_aligned_structure, c3), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c3"},
			{offsetof(Not_aligned_structure, l), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), "l"},
			{offsetof(Not_aligned_structure, c4), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c4"},
			{offsetof(Not_aligned_structure, ul), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)), "ul"},
			{offsetof(Not_aligned_structure, c5), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c5"},
			{offsetof(Not_aligned_structure, f), PDI::Scalar_datatype::make(PDI::Scalar_kind::FLOAT, sizeof(float)), "f"},
			{offsetof(Not_aligned_structure, c6), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c6"},
			{offsetof(Not_aligned_structure, d), PDI::Scalar_datatype::make(PDI::Scalar_kind::FLOAT, sizeof(double)), "d"},
		},
		sizeof(Not_aligned_structure)
	);
};

/*
 * Struct prepared for AlignedScalarsTest.
 * Order of the fields in Aligned_structure is important
 * to test padding sizes.
 */
struct AlignedScalarsTest {
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

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	//record with scalar members (which are aligned)
	std::shared_ptr<PDI::Record_datatype> m_test_record{PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Aligned_structure, i), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), "i"},
			{offsetof(Aligned_structure, u), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)), "u"},
			{offsetof(Aligned_structure, l), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), "l"},
			{offsetof(Aligned_structure, ul), PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)), "ul"},
			{offsetof(Aligned_structure, c), PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(char)), "c"}
		},
		sizeof(Aligned_structure)
	)};
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

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	//record with arrays (which are dense) containing scalars
	std::shared_ptr<PDI::Record_datatype> m_test_record{PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Dense_array_structure, i), PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), 3), "i"
	        },
			{offsetof(Dense_array_structure, u),
	         PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)), 4),
	         "u"},
			{offsetof(Dense_array_structure, l), PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), 5), "l"
	        },

			{offsetof(Dense_array_structure, ul),
	         PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)), 6),
	         "ul"}
		},
		sizeof(Dense_array_structure)
	)};
};

/*
 * Struct prepared for SparseScalarsTest.
 * Order of the fields in Sparse_structure is important
 * to test padding sizes.
 */
struct SparseArrayScalarsTest {
	struct Sparse_array_structure {
		int i[100]; //buffer: 10 x 10; data: 4 x 4; start: (4, 4)
		long l[60]; //buffer: 3 x 20; data: 1 x 20; start (0, 1)
	};

	bool dense() { return false; }

	size_t datasize() { return 4 * 4 * sizeof(int) + 1 * 20 * sizeof(long); }

	size_t buffersize() { return sizeof(Sparse_array_structure); }

	size_t buffersize_after_densify() { return datasize(); }

	size_t alignment() { return std::max({sizeof(int), sizeof(long)}); }

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	//record with arrays (which are sparse) containing scalars
	std::shared_ptr<PDI::Record_datatype> m_test_record{PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Sparse_array_structure, i),
	         PDI::Datatype_sptr{PDI::Array_datatype::make(
				 PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(int)), 10, 3, 4),
				 10,
				 3,
				 4
			 )},
	         "i"},
			{offsetof(Sparse_array_structure, l),
	         PDI::Array_datatype::make(PDI::Array_datatype::make(PDI::Scalar_datatype::make(PDI::Scalar_kind::SIGNED, sizeof(long)), 20), 3, 1, 1),
	         "l"}
		},
		sizeof(Sparse_array_structure)
	)};
};

/*
 * Struct prepared for DenseRecordsInRecordTest.
 * Order of the fields in Dense_record is important
 * to test padding sizes.
 */
struct DenseRecordsInRecordTest {
	struct Dense_record {
		AlignedScalarsTest::Aligned_structure aligned_scalar_record;
		DenseArrayScalarsTest::Dense_array_structure dense_array_record;
	};

	bool dense() { return true; }

	size_t datasize() { return scalar_structure_test->datasize() + array_structure_test->datasize(); }

	size_t buffersize() { return sizeof(Dense_record); }

	size_t buffersize_after_densify() { return scalar_structure_test->buffersize_after_densify() + array_structure_test->buffersize_after_densify(); }

	size_t alignment() { return std::max({scalar_structure_test->alignment(), array_structure_test->alignment()}); }

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	std::unique_ptr<AlignedScalarsTest> scalar_structure_test{new AlignedScalarsTest()};
	std::unique_ptr<DenseArrayScalarsTest> array_structure_test{new DenseArrayScalarsTest()};

	/*
	 * record with records (which are dense) which contain dense
	 * structures (scalars and arrays from previous tests)
	 */
	std::shared_ptr<PDI::Record_datatype> m_test_record{PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Dense_record, aligned_scalar_record), PDI::Datatype_sptr{scalar_structure_test->test_record()}, "dense_scalar_record"},
			{offsetof(Dense_record, dense_array_record), PDI::Datatype_sptr{array_structure_test->test_record()}, "dense_array_record"}
		},
		sizeof(Dense_record)
	)};
};

/*
 * Struct prepared for SparseRecordsInRecordTest.
 * Order of the fields in Sparse_record is important
 * to test padding sizes.
 */
struct SparseRecordsInRecordTest {
	struct Sparse_record {
		NotAlignedScalarsTest::Not_aligned_structure not_aligned_scalar_record;
		SparseArrayScalarsTest::Sparse_array_structure sparse_array_record;
	};

	bool dense() { return false; }

	size_t datasize() { return scalar_structure_test->datasize() + array_structure_test->datasize(); }

	size_t buffersize() { return sizeof(Sparse_record); }

	size_t buffersize_after_densify() { return scalar_structure_test->buffersize_after_densify() + array_structure_test->buffersize_after_densify(); }

	size_t alignment() { return std::max({scalar_structure_test->alignment(), array_structure_test->alignment()}); }

	std::shared_ptr<PDI::Record_datatype> test_record() { return m_test_record; }

	std::unique_ptr<NotAlignedScalarsTest> scalar_structure_test{new NotAlignedScalarsTest()};
	std::unique_ptr<SparseArrayScalarsTest> array_structure_test{new SparseArrayScalarsTest()};

	/*
	 * record with records (which are dense) which contain sparse
	 * structures (scalars and arrays from previous tests)
	 */
	std::shared_ptr<PDI::Record_datatype> m_test_record{PDI::Record_datatype::make(
		std::vector<PDI::Record_datatype::Member>{
			{offsetof(Sparse_record, not_aligned_scalar_record), scalar_structure_test->test_record(), "sparse_scalar_record"},
			{offsetof(Sparse_record, sparse_array_record), array_structure_test->test_record(), "sparse_array_record"}
		},
		sizeof(Sparse_record)
	)};
};

#endif // PDI_RECORD_DATATYPE_TEST_H_
