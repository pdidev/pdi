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

//! [example]
#include <stdio.h>

#include <pdi.h>

void sum_and_multiply(void)
{
	void* number1;
	PDI_access("number1", &number1, PDI_IN);
	void* number2;
	PDI_access("number2", &number2, PDI_IN);
	void* sum;
	PDI_access("sum", &sum, PDI_OUT);
	void* product;
	PDI_access("product", &product, PDI_OUT);
	*((int*)sum) = *((int*)number1) + *((int*)number2);
	*((int*)product) = *((int*)number1) * *((int*)number2);
	PDI_release("number1");
	PDI_release("number2");
	PDI_release("sum");
	PDI_release("product");
}

int main(int argc, char* argv[])
{
	PC_tree_t conf = PC_parse_path("calculate.yml");
	PDI_init(conf);
	int foo = 4, bar = 5, res1 = 0, res2 = 0;
	printf("Before calculation, foo = %d, bar = %d, res1 = %d, res2 = %d.\n", foo, bar, res1, res2);
	PDI_multi_expose("calculate", "foo", &foo, PDI_OUT, "bar", &bar, PDI_OUT, "res1", &res1, PDI_IN, "res2", &res2, PDI_IN, NULL);
	printf("After calculation, foo = %d, bar = %d, res1 = %d, res2 = %d.\n", foo, bar, res1, res2);
	PDI_finalize();
	return 0;
}

//! [example]
