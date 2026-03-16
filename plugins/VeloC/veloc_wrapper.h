/*******************************************************************************
 * Copyright (C) 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
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

#ifndef VELOC_PLUGIN_H
#define VELOC_PLUGIN_H

#include <pdi/plugin.h>
#include <pdi/context.h>
#include <pdi/expression.h>
#include <veloc.h>

#include <iostream>
#include <vector>
#include <unordered_map>
#include <string>
#include <algorithm>

using PDI::Context;
using PDI::each;
using PDI::opt_each;
using PDI::Error;
using PDI::Config_error;
using PDI::Plugin;
using PDI::Ref;
using PDI::Ref_r;
using PDI::to_long;
using PDI::to_string;
using std::string;
using std::unordered_map;
using std::vector;
using std::cout;
using std::endl;

class veloc_plugin : public Plugin
{
private:
    string veloc_file; 
    PDI::Expression when;
    long int failure_value;
    string cp_label; 
    string iter_name; 
    vector<string> protected_data; 
    bool restore_from_last_checkpoint;
    unordered_map<string, bool> register_memory_regions;
    PC_tree_t saved_config;
    string checkpoint_event_name; 
    long int recovered_iter;
    long int cp_counter;

public:
    veloc_plugin(Context& ctx, PC_tree_t config);

    ~veloc_plugin();

private:
    void write_checkpoint();

    void load_checkpoint();

    void event(const std::string& event);
    
    bool memoryRegionsWereRegistered();
};
#endif 
