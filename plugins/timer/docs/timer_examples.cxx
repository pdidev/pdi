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
/// The specification trees of the timer documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_EXAMPLE = R"PDIYAML(
#! [example]
plugins:
  timer: 
    timer_A: {start: "decl_hdf5_start_timer", stop: "decl_hdf5_stop_timer"}
    timer_B: "decl_hdf5"
    timer_C: ["toto", "titi"]
    timer_D: 
      start: "begin_timing"
      stop: "end_timing"
    timer_E: ["pdi"]
    output_to: cout
#! [example]
)PDIYAML";

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct TimerDoc: public ::PDI::PdiTest {};

TEST_F(TimerDoc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_EXAMPLE));
	FinalizePdi();
}
