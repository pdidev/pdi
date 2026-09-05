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
/// The specification trees of the user_code documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <pdi.h>
#include <pdi/testing.h>

/// The functions the specification trees below name.
/// They must be exported for the plugin to find them; what they do is
/// irrelevant here, only that the trees naming them are accepted.
extern "C" void fun1(void) {}

extern "C" void fun2(void) {}

extern "C" void fun3(void) {}

extern "C" void fun4(void) {}

const char* CONFIG_ON_DATA_LIST = R"PDIYAML(
#! [on_data_list]
plugins:                       
    user_code:                   
        on_data:                   
            - my_data:                 
                when: '$cond>1'
                fun1: {in: $desc2, out: $desc3}
            - my_data:
                fun2: {}
                fun3: {out: $desc2}
#! [on_data_list]
)PDIYAML";

const char* CONFIG_ON_EVENT_LIST = R"PDIYAML(
#! [on_event_list]
plugins:                       
    user_code:                   
        on_event:                   
            - my_event:                 
                when: '$cond>1'
                fun1: {in: $desc2, out: $desc3}
            - my_event:
                fun2: {}
                fun3: {out: $desc2}
#! [on_event_list]
)PDIYAML";

const char* CONFIG_FULL_TREE = R"PDIYAML(
#! [full_tree]
metadata:
    cond: int
data:
    desc1: int                          
    desc2: float                   
    desc3: double                  
plugins:                       
    user_code:                   
        on_data:                   
            desc1:                 
                when: '$cond>1'
                fun1: {in: $desc2, out: $desc3}
            desc2:
                fun2: {}
                fun3: {out: $desc2}
        on_event:                  
            event1:                 
                when: '$cond<1'     
                fun2: {}
            event2:
                fun4: {param1: $desc2, param2: $desc1, param3: $desc3}
#! [full_tree]
)PDIYAML";

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct UserCodeDoc: public ::PDI::PdiTest {};

TEST_F(UserCodeDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_ON_DATA_LIST));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_ON_EVENT_LIST));
	FinalizePdi();
	InitPdi(PC_parse_string(CONFIG_FULL_TREE));
	FinalizePdi();
}
