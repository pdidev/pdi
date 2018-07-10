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

#include <pdi/context.h>
#include <pdi/reference_datatype.h>
#include <pdi/ref_any.h>

#include <memory>

namespace PDI {

Reference_datatype::Reference_datatype(std::string name): m_name{name}
{}

Ref Reference_datatype::get_ref(Context& ctx)
{
	Data_descriptor& desc = ctx[m_name];
	return desc.ref();
}

Datatype_template_uptr Reference_datatype::clone() const
{
	return clone_type();
}

Datatype_uptr Reference_datatype::clone_type() const
{
	return std::unique_ptr<Reference_datatype> {new Reference_datatype{m_name}};
}

Datatype_uptr Reference_datatype::densify() const
{
	return clone_type();
}

Datatype_uptr Reference_datatype::evaluate(Context&) const
{
	return clone_type();
}

bool Reference_datatype::dense() const
{
	return true;
}

size_t Reference_datatype::datasize() const
{
	return 0;
}

size_t Reference_datatype::buffersize() const
{
	return 0;
}

size_t Reference_datatype::alignment() const
{
	return 0;
}

} // namespace PDI

