/*******************************************************************************
 * Copyright (C) 2020-2021 Institute of Bioorganic Chemistry Polish Academy of Science (PSNC)
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

#ifndef SET_VALUE_SHARE_OPERATION_H_
#define SET_VALUE_SHARE_OPERATION_H_

#include <map>
#include <set>
#include <string>

#include <pdi/context.h>
#include <pdi/paraconf_wrapper.h>

#include "operation.h"

namespace set_value {

class Share_operation : public Operation
{
    /// desc names that was shared
    std::set<std::string> m_data_to_release;

    /// map of data to share (shared new Ref (created from Expression))
    std::vector<std::pair<std::string, PC_tree_t>> m_data_to_share;

public:

    Share_operation(PDI::Context& ctx, PC_tree_t list_of_values);
    
    void execute() override;

    ~Share_operation();
};

} // namespace <anonymous>

#endif //SET_VALUE_SHARE_OPERATION_H_
