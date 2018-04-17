/*******************************************************************************
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

#include <pdi/pdi_fwd.h>
#include <pdi/array_datatype.h>
#include <pdi/scalar_datatype.h>
#include <pdi/record_datatype.h>

#include <vector>

/*
 * All records must inherit from this interface
 * for proper testing in tests/PDI_record_datatype.cxx
 */
struct Record_interface {
	//expected values from record functions
	virtual const bool dense() = 0;
	virtual const size_t datasize() = 0;
	virtual const size_t buffersize() = 0;
	virtual const size_t alignment() = 0;
	
	//tested record
	virtual PDI::Record_datatype* test_record() = 0;
};

/*
 * Struct prepared for SparseScalarsTest.
 * Order of the fields in Sparse_structure is important
 * to test padding sizes.
 */
struct SparseScalarsTest : Record_interface {
	//frame of the structure
	struct Sparse_structure {
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
	
	const bool dense() override
	{
		return false;
	}
	
	const size_t datasize() override
	{
		return 6 * sizeof(char) +
		    sizeof(int) +
		    sizeof(unsigned int) +
		    sizeof(long) +
		    sizeof(unsigned long) +
		    sizeof(float) +
		    sizeof(double);
	}
	
	const size_t buffersize() override
	{
		return sizeof(Sparse_structure);
	}
	
	const size_t alignment() override
	{
		return std::max({sizeof(char),
		            sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long),
		            sizeof(float),
		            sizeof(double)});
	}
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	//record with scalar members (which are sparse)
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Sparse_structure, c1),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c1"
			},
			{
				offsetof(Sparse_structure, i),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}},
				"i"
			},
			{
				offsetof(Sparse_structure, c2),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c2"
			},
			{
				offsetof(Sparse_structure, u),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)}},
				"u"
			},
			{
				offsetof(Sparse_structure, c3),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c3"
			},
			{
				offsetof(Sparse_structure, l),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}},
				"l"
			},
			{
				offsetof(Sparse_structure, c4),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c4"
			},
			{
				offsetof(Sparse_structure, ul),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)}},
				"ul"
			},
			{
				offsetof(Sparse_structure, c5),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c5"
			},
			{
				offsetof(Sparse_structure, f),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::FLOAT, sizeof(float)}},
				"f"
			},
			{
				offsetof(Sparse_structure, c6),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}},
				"c6"
			},
			{
				offsetof(Sparse_structure, d),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::FLOAT, sizeof(double)}},
				"d"
			},
		},
		sizeof(Sparse_structure)
	};
};

/*
 * Struct prepared for DenseScalarsTest.
 * Order of the fields in Dense_structure is important
 * to test padding sizes.
 */
struct DenseScalarsTest : Record_interface {
	struct Dense_structure {
		int i;
		unsigned int u;
		long l;
		unsigned long ul;
	};
	
	const bool dense() override
	{
		return true;
	}
	
	const size_t datasize() override
	{
		return sizeof(int) +
		    sizeof(unsigned int) +
		    sizeof(long) +
		    sizeof(unsigned long);
	}
	
	const size_t buffersize() override
	{
		return sizeof(Dense_structure);
	}
	
	const size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long)});
	}
	
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	//record with scalar members (which are dense)
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Dense_structure, i),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}},
				"i"
			},
			{
				offsetof(Dense_structure, u),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)}},
				"u"
			},
			{
				offsetof(Dense_structure, l),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}},
				"l"
			},
			{
				offsetof(Dense_structure, ul),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)}},
				"ul"
			}
		},
		sizeof(Dense_structure)
	};
};

/*
 * Struct prepared for DenseArrayScalarsTest.
 * Order of the fields in Dense_array_structure is important
 * to test padding sizes.
 */
struct DenseArrayScalarsTest : Record_interface {
	struct Dense_array_structure {
		int i[3];
		unsigned int u[4];
		long l[5];
		unsigned long ul[6];
	};
	
	const bool dense() override
	{
		return true;
	}
	
	const size_t datasize() override
	{
		return 3 * sizeof(int) +
		    4 * sizeof(unsigned int) +
		    5 * sizeof(long) +
		    6 * sizeof(unsigned long);
	}
	
	const size_t buffersize() override
	{
		return sizeof(Dense_array_structure);
	}
	
	const size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long)});
	}
	
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	//record with arrays (which are dense) containing scalars
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Dense_array_structure, i),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}},
						3
					}
				},
				"i"
			},
			{
				offsetof(Dense_array_structure, u),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)}},
						4
					}
				},
				"u"
			},
			{
				offsetof(Dense_array_structure, l),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}},
						5
					}
				},
				"l"
			},
			
			{
				offsetof(Dense_array_structure, ul),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)}},
						6
					}
				},
				"ul"
			}
		},
		sizeof(Dense_array_structure)
	};
};

/*
 * Struct prepared for SparseScalarsTest.
 * Order of the fields in Sparse_structure is important
 * to test padding sizes.
 */
struct SparseArrayScalarsTest : Record_interface {
	struct Sparse_array_structure {
		int i[100]; //buffer: 10 x 10; data: 4 x 4; start: (4, 4)
		long l[60]; //buffer: 3 x 20; data: 1 x 20; start (0, 1)
	};
	
	const bool dense() override
	{
		return false;
	}
	
	const size_t datasize() override
	{
		return 4 * 4 * sizeof(int) +
		    1 * 20 * sizeof(long);
	}
	
	const size_t buffersize() override
	{
		return sizeof(Sparse_array_structure);
	}
	
	const size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(long)});
	}
	
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	//record with arrays (which are sparse) containing scalars
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Sparse_array_structure, i),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr {
							new PDI::Array_datatype
							{
								PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}},
								10,
								3,
								4
							}
						},
						10,
						3,
						4
					}
				},
				"i"
			},
			{
				offsetof(Sparse_array_structure, l),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr {
							new PDI::Array_datatype
							{
								PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}},
								20
							}
						},
						3,
						1,
						1
					}
				},
				"l"
			}
		},
		sizeof(Sparse_array_structure)
	};
};

/*
 * Struct prepared for DenseRecordsInRecordTest.
 * Order of the fields in Dense_record is important
 * to test padding sizes.
 */
struct DenseRecordsInRecordTest : Record_interface {
	struct Dense_record {
		DenseScalarsTest::Dense_structure dense_scalar_record;
		DenseArrayScalarsTest::Dense_array_structure dense_array_record;
	};
	
	const bool dense() override
	{
		return true;
	}
	
	const size_t datasize() override
	{
		return scalar_structure_test->datasize() + array_structure_test->datasize();
	}
	
	const size_t buffersize() override
	{
		return sizeof(Dense_record);
	}
	
	const size_t alignment() override
	{
		return std::max({scalar_structure_test->alignment(),
		            array_structure_test->alignment()});
	}
	
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	std::unique_ptr<DenseScalarsTest> scalar_structure_test {new DenseScalarsTest()};
	std::unique_ptr<DenseArrayScalarsTest> array_structure_test {new DenseArrayScalarsTest()};
	
	/*
	 * record with records (which are dense) which contain dense
	 * structures (scalars and arrays from previous tests)
	 */
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Dense_record, dense_scalar_record),
				PDI::Datatype_uptr
				{
					scalar_structure_test->test_record()->clone_type()
				},
				"dense_scalar_record"
			},
			{
				offsetof(Dense_record, dense_array_record),
				PDI::Datatype_uptr
				{
					array_structure_test->test_record()->clone_type()
				},
				"dense_array_record"
			}
		},
		sizeof(Dense_record)
	};
};

/*
 * Struct prepared for SparseRecordsInRecordTest.
 * Order of the fields in Sparse_record is important
 * to test padding sizes.
 */
struct SparseRecordsInRecordTest : Record_interface {
	struct Sparse_record {
		SparseScalarsTest::Sparse_structure sparse_scalar_record;
		SparseArrayScalarsTest::Sparse_array_structure sparse_array_record;
	};
	
	const bool dense() override
	{
		return false;
	}
	
	const size_t datasize() override
	{
		return scalar_structure_test->datasize() + array_structure_test->datasize();
	}
	
	const size_t buffersize() override
	{
		return sizeof(Sparse_record);
	}
	
	const size_t alignment() override
	{
		return std::max({scalar_structure_test->alignment(),
		            array_structure_test->alignment()});
	}
	
	PDI::Record_datatype* test_record() override
	{
		return &m_test_record;
	}
	
	std::unique_ptr<SparseScalarsTest> scalar_structure_test {new SparseScalarsTest()};
	std::unique_ptr<SparseArrayScalarsTest> array_structure_test {new SparseArrayScalarsTest()};
	
	/*
	 * record with records (which are dense) which contain sparse
	 * structures (scalars and arrays from previous tests)
	 */
	PDI::Record_datatype m_test_record {
		std::vector<PDI::Record_datatype::Member> {
			{
				offsetof(Sparse_record, sparse_scalar_record),
				PDI::Datatype_uptr
				{
					scalar_structure_test->test_record()->clone_type()
				},
				"sparse_scalar_record"
			},
			{
				offsetof(Sparse_record, sparse_array_record),
				PDI::Datatype_uptr
				{
					array_structure_test->test_record()->clone_type()
				},
				"sparse_array_record"
			}
		},
		sizeof(Sparse_record)
	};
};

#endif // PDI_RECORD_DATATYPE_TEST_H_