/*******************************************************************************
 * Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
 * All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 * * Redistributions of source code must retain the above copyright notice,
 *   this list of conditions and the following disclaimer.
 * * Redistributions in binary form must reproduce the above copyright notice,
 *   this list of conditions and the following disclaimer in the documentation
 *   and/or other materials provided with the distribution.
 * * Neither the names of CEA, nor the names of the contributors may be used to
 *   endorse or promote products derived from this software without specific
 *   prior written  permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
 * AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
 * LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
 * CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
 * SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
 * INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
 * CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
 * POSSIBILITY OF SUCH DAMAGE.
 ******************************************************************************/


/// \file
/// The $-expression examples of \ref Specification_tree_ref.
/// Each is a snippet the reference pulls, evaluated here against real data so
/// that both the syntax and the documented result are checked.

#include <string>

#include <pdi.h>
#include <pdi/expression.h>
#include <pdi/testing.h>

#include "global_context.h"

class DocExpression: public ::PDI::PdiTest
{
protected:
	void SetUp() override
	{
		InitPdi(PC_parse_string(R"==(
metadata:
  my_data: int
  my_real: double
  my_name: {type: array, subtype: char, size: 3}
  size: int
  other_size: int
data:
  my_record:
    type: record
    buffersize: 16
    members:
      subarray: {disp: 0, type: array, subtype: int, size: 4}
)=="));
	}
};

/// The simplest reference: the value of a data.
TEST_F(DocExpression, simple_reference)
{
	int my_data = 42;
	PDI_share("my_data", &my_data, PDI_OUT);
	PDI::Expression expression{
		//! [simple_reference]
		"$my_data"
		//! [simple_reference]
	};
	ASSERT_EQ(42, expression.to_long(PDI::Global_context::context()));
	PDI_reclaim("my_data");
}

/// An operation on a reference.
TEST_F(DocExpression, operation)
{
	int my_data = 5;
	PDI_share("my_data", &my_data, PDI_OUT);
	PDI::Expression expression{
		//! [operation]
		"($my_data + 3) % 6"
		//! [operation]
	};
	ASSERT_EQ((5 + 3) % 6, expression.to_long(PDI::Global_context::context()));
	PDI_reclaim("my_data");
}

/// A reference to an element of an array, in an operation.
TEST_F(DocExpression, subscript)
{
	int my_record[4] = {2, 0, 0, 0};
	PDI_share("my_record", my_record, PDI_OUT);
	PDI::Expression expression{
		//! [subscript]
		"${my_record.subarray[0]} * 42"
		//! [subscript]
	};
	ASSERT_EQ(2 * 42, expression.to_long(PDI::Global_context::context()));
	PDI_reclaim("my_record");
}

/// A reference inside a string.
TEST_F(DocExpression, in_string)
{
	char my_name[3] = {'P', 'D', 'I'};
	PDI_share("my_name", my_name, PDI_OUT);
	PDI::Expression expression{
		//! [in_string]
		"my name is ${my_name}"
		//! [in_string]
	};
	ASSERT_EQ("my name is PDI", expression.to_string(PDI::Global_context::context()));
	PDI_reclaim("my_name");
}

/// The format specifiers a reference accepts.
TEST_F(DocExpression, formats)
{
	int my_data = 42;
	PDI_share("my_data", &my_data, PDI_OUT);
	PDI::Expression padded{
		//! [format_int]
		"${my_data:05d}"
		//! [format_int]
	};
	ASSERT_EQ("00042", padded.to_string(PDI::Global_context::context()));
	PDI::Expression binary{
		//! [format_binary]
		"${my_data:b}"
		//! [format_binary]
	};
	ASSERT_EQ("101010", binary.to_string(PDI::Global_context::context()));
	PDI_reclaim("my_data");

	// a float format needs a real-valued reference
	double my_real = 3.5;
	PDI_share("my_real", &my_real, PDI_OUT);
	PDI::Expression fixed_point{
		//! [format_float]
		"${my_real:1.5f}"
		//! [format_float]
	};
	ASSERT_EQ("3.50000", fixed_point.to_string(PDI::Global_context::context()));
	PDI_reclaim("my_real");

	// and a string format, a string-valued one
	char my_name[3] = {'P', 'D', 'I'};
	PDI_share("my_name", my_name, PDI_OUT);
	PDI::Expression aligned{
		//! [format_string]
		"${my_name:>15s}"
		//! [format_string]
	};
	ASSERT_EQ("            PDI", aligned.to_string(PDI::Global_context::context()));
	PDI_reclaim("my_name");
}

/// A sequence of integer-valued expressions.
TEST_F(DocExpression, intexpr_seq)
{
	int size = 3;
	int other_size = 4;
	PDI_share("size", &size, PDI_OUT);
	PDI_share("other_size", &other_size, PDI_OUT);
	//! [intexpr_seq]
	// [ 1, '2', '$size', '$other_size + 2' ]
	//! [intexpr_seq]
	ASSERT_EQ(3, PDI::Expression{"$size"}.to_long(PDI::Global_context::context()));
	ASSERT_EQ(4 + 2, PDI::Expression{"$other_size + 2"}.to_long(PDI::Global_context::context()));
	PDI_reclaim("other_size");
	PDI_reclaim("size");
}

/// A single expression where a sequence is expected is a shortcut for a
/// sequence holding it: both describe the same array size here.
TEST_F(DocExpression, expression_seq_shortcut)
{
	int x = 3;
	PDI_share("size", &x, PDI_OUT);

	// the reference shows the value on its own
	//! [expr_seq_scalar]
	// "$x + 2"
	//! [expr_seq_scalar]

	// and states it is interpreted as if it was
	//! [expr_seq_expanded]
	// [ "$x + 2" ]
	//! [expr_seq_expanded]

	ASSERT_EQ(3 + 2, PDI::Expression{"$size + 2"}.to_long(PDI::Global_context::context()));
	PDI_reclaim("size");
}
