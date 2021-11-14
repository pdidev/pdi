/*******************************************************************************
 * Copyright (C) 2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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
 * All tuples must inherit from this interface
 * for proper testing in tests/PDI_tuple_datatype.cxx
 */
struct Tuple_interface {
	//expected values from tuple functions
	virtual bool dense() = 0;
	virtual size_t datasize() = 0;
	virtual size_t buffersize() = 0;
	virtual size_t buffersize_after_densify() = 0;
	virtual size_t alignment() = 0;
	
	//tested tuple
	virtual PDI::Tuple_datatype* test_tuple() = 0;
	
	virtual ~Tuple_interface() {}
};

/*
 * Struct prepared for TupleNotAlignedScalarsTest.
 * Order of the fields in Not_aligned_structure is important
 * to test padding sizes.
 */
struct TupleNotAlignedScalarsTest : Tuple_interface {
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
	
	bool dense() override
	{
		return true;
	}
	
	size_t datasize() override
	{
		return 6 * sizeof(char) +
		    sizeof(int) +
		    sizeof(unsigned int) +
		    sizeof(long) +
		    sizeof(unsigned long) +
		    sizeof(float) +
		    sizeof(double);
	}
	
	size_t buffersize() override
	{
		return sizeof(Not_aligned_structure);
	}
	
	size_t buffersize_after_densify() override
	{
		return buffersize();
	}
	
	size_t alignment() override
	{
		return std::max({sizeof(char),
		            sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long),
		            sizeof(float),
		            sizeof(double)});
	}
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	//tuple with scalar members (which are not aligned)
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
			{
				offsetof(Not_aligned_structure, c1),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, i),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}}
			},
			{
				offsetof(Not_aligned_structure, c2),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, u),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)}}
			},
			{
				offsetof(Not_aligned_structure, c3),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, l),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}}
			},
			{
				offsetof(Not_aligned_structure, c4),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, ul),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)}}
			},
			{
				offsetof(Not_aligned_structure, c5),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, f),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::FLOAT, sizeof(float)}}
			},
			{
				offsetof(Not_aligned_structure, c6),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			},
			{
				offsetof(Not_aligned_structure, d),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::FLOAT, sizeof(double)}}
			},
		},
		sizeof(Not_aligned_structure)
	};
};

/*
 * Struct prepared for TupleAlignedScalarsTest.
 * Order of the fields in Aligned_structure is important
 * to test padding sizes.
 */
struct TupleAlignedScalarsTest : Tuple_interface {
	struct Aligned_structure {
		int i;
		unsigned int u;
		long l;
		unsigned long ul;
		char c;
	};
	
	bool dense() override
	{
		return true;
	}
	
	size_t datasize() override
	{
		return sizeof(int) +
		    sizeof(unsigned int) +
		    sizeof(long) +
		    sizeof(unsigned long) +
		    sizeof(char);
	}
	
	size_t buffersize() override
	{
		return sizeof(Aligned_structure);
	}
	
	size_t buffersize_after_densify() override
	{
		return buffersize(); //datasize() + (alignof(long) - alignof(char));
	}
	
	size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long),
		            sizeof(char)});
	}
	
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	//tuple with scalar members (which are aligned)
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
			{
				offsetof(Aligned_structure, i),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}}
			},
			{
				offsetof(Aligned_structure, u),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned int)}}
			},
			{
				offsetof(Aligned_structure, l),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(long)}}
			},
			{
				offsetof(Aligned_structure, ul),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::UNSIGNED, sizeof(unsigned long)}}
			},
			{
				offsetof(Aligned_structure, c),
				PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(char)}}
			}
		},
		sizeof(Aligned_structure)
	};
};

/*
 * Struct prepared for DenseArrayScalarsTest.
 * Order of the fields in Dense_array_structure is important
 * to test padding sizes.
 */
struct DenseArrayScalarsTest : Tuple_interface {
	struct Dense_array_structure {
		int i[3];
		unsigned int u[4];
		long l[5];
		unsigned long ul[6];
	};
	
	bool dense() override
	{
		return true;
	}
	
	size_t datasize() override
	{
		return 3 * sizeof(int) +
		    4 * sizeof(unsigned int) +
		    5 * sizeof(long) +
		    6 * sizeof(unsigned long);
	}
	
	size_t buffersize() override
	{
		return sizeof(Dense_array_structure);
	}
	
	size_t buffersize_after_densify() override
	{
		return datasize() + (alignof(long) - alignof(int));
	}
	
	size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(unsigned int),
		            sizeof(long),
		            sizeof(unsigned long)});
	}
	
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	//tuple with arrays (which are dense) containing scalars
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
			{
				offsetof(Dense_array_structure, i),
				PDI::Datatype_uptr
				{
					new PDI::Array_datatype
					{
						PDI::Datatype_uptr{new PDI::Scalar_datatype {PDI::Scalar_kind::SIGNED, sizeof(int)}},
						3
					}
				}
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
				}
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
				}
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
				}
			}
		},
		sizeof(Dense_array_structure)
	};
};

/*
 * Struct prepared for TupleSparseArrayScalarsTest.
 * Order of the fields in Sparse_structure is important
 * to test padding sizes.
 */
struct TupleSparseArrayScalarsTest : Tuple_interface {
	struct Sparse_array_structure {
		int i[100]; //buffer: 10 x 10; data: 4 x 4; start: (4, 4)
		long l[60]; //buffer: 3 x 20; data: 1 x 20; start (0, 1)
	};
	
	bool dense() override
	{
		return false;
	}
	
	size_t datasize() override
	{
		return 4 * 4 * sizeof(int) +
		    1 * 20 * sizeof(long);
	}
	
	size_t buffersize() override
	{
		return sizeof(Sparse_array_structure);
	}
	
	size_t buffersize_after_densify() override
	{
		return datasize();
	}
	
	size_t alignment() override
	{
		return std::max({sizeof(int),
		            sizeof(long)});
	}
	
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	//tuple with arrays (which are sparse) containing scalars
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
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
				}
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
				}
			}
		},
		sizeof(Sparse_array_structure)
	};
};

/*
 * Struct prepared for DenseTuplesInTupleTest.
 * Order of the fields in Dense_tuple is important
 * to test padding sizes.
 */
struct DenseTuplesInTupleTest : Tuple_interface {
	struct Dense_tuple {
		TupleAlignedScalarsTest::Aligned_structure aligned_scalar_tuple;
		DenseArrayScalarsTest::Dense_array_structure dense_array_tuple;
	};
	
	bool dense() override
	{
		return true;
	}
	
	size_t datasize() override
	{
		return scalar_structure_test->datasize() + array_structure_test->datasize();
	}
	
	size_t buffersize() override
	{
		return sizeof(Dense_tuple);
	}
	
	size_t buffersize_after_densify() override
	{
		return scalar_structure_test->buffersize_after_densify() +
		    array_structure_test->buffersize_after_densify();
	}
	
	size_t alignment() override
	{
		return std::max({scalar_structure_test->alignment(),
		            array_structure_test->alignment()});
	}
	
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	std::unique_ptr<TupleAlignedScalarsTest> scalar_structure_test {new TupleAlignedScalarsTest()};
	std::unique_ptr<DenseArrayScalarsTest> array_structure_test {new DenseArrayScalarsTest()};
	
	/*
	 * tuple with tuples (which are dense) which contain dense
	 * structures (scalars and arrays from previous tests)
	 */
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
			{
				offsetof(Dense_tuple, aligned_scalar_tuple),
				PDI::Datatype_uptr
				{
					scalar_structure_test->test_tuple()->clone_type()
				}
			},
			{
				offsetof(Dense_tuple, dense_array_tuple),
				PDI::Datatype_uptr
				{
					array_structure_test->test_tuple()->clone_type()
				}
			}
		},
		sizeof(Dense_tuple)
	};
};

/*
 * Struct prepared for SparseTuplesInTupleTest.
 * Order of the fields in Sparse_tuple is important
 * to test padding sizes.
 */
struct SparseTuplesInTupleTest : Tuple_interface {
	struct Sparse_tuple {
		TupleNotAlignedScalarsTest::Not_aligned_structure not_aligned_scalar_tuple;
		TupleSparseArrayScalarsTest::Sparse_array_structure sparse_array_tuple;
	};
	
	bool dense() override
	{
		return false;
	}
	
	size_t datasize() override
	{
		return scalar_structure_test->datasize() + array_structure_test->datasize();
	}
	
	size_t buffersize() override
	{
		return sizeof(Sparse_tuple);
	}
	
	size_t buffersize_after_densify() override
	{
		return scalar_structure_test->buffersize_after_densify() +
		    array_structure_test->buffersize_after_densify();
	}
	
	size_t alignment() override
	{
		return std::max({scalar_structure_test->alignment(),
		            array_structure_test->alignment()});
	}
	
	PDI::Tuple_datatype* test_tuple() override
	{
		return &m_test_tuple;
	}
	
	std::unique_ptr<TupleNotAlignedScalarsTest> scalar_structure_test {new TupleNotAlignedScalarsTest()};
	std::unique_ptr<TupleSparseArrayScalarsTest> array_structure_test {new TupleSparseArrayScalarsTest()};
	
	/*
	 * tuple with tuples (which are dense) which contain sparse
	 * structures (scalars and arrays from previous tests)
	 */
	PDI::Tuple_datatype m_test_tuple {
		std::vector<PDI::Tuple_datatype::Element> {
			{
				offsetof(Sparse_tuple, not_aligned_scalar_tuple),
				PDI::Datatype_uptr
				{
					scalar_structure_test->test_tuple()->clone_type()
				}
			},
			{
				offsetof(Sparse_tuple, sparse_array_tuple),
				PDI::Datatype_uptr
				{
					array_structure_test->test_tuple()->clone_type()
				}
			}
		},
		sizeof(Sparse_tuple)
	};
};

#endif // PDI_TUPLE_DATATYPE_TEST_H_

