/*******************************************************************************
 * Copyright (C) 2015-2024 Commissariat a l'energie atomique et aux energies alternatives (CEA)
 * Copyright (C) 2024 National Institute for Research in Digital Science and Technology (Inria)
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

 #ifndef Damaris_async_gather_H_
 #define Damaris_async_gather_H_
 
 #include <string>
 #include <map>
 #include <set>
 #include <tuple>
 #include <unordered_map>
 #include <unordered_set>
 
 #include <pdi/context.h>
 #include <pdi/expression.h>
 #include <pdi/pdi_fwd.h>
 
 // Definitions of the Damaris XML tag generators
 #include <Damaris.h>
 #include <damaris/util/DamarisVar.hpp>
 #include <damaris/model/ModifyModel.hpp>
 
 #include "damaris_wrapper.h"
 
 using PDI::Context;
 using std::unique_ptr;
 using std::list;
 using std::string;
 
 namespace damaris_pdi {
 
 class Damaris_async_gather
 {
     
 
     
 public:
     Damaris_async_gather(PDI::Context& ctx, PC_tree_t tree);
     
 }; // class Damaris_async_gather
 
 } // namespace damaris_pdi
 
 #endif // Damaris_async_gather_H_