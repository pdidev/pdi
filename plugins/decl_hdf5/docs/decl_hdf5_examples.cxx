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
/// The specification trees of the decl_hdf5 documentation.
/// Every one of them is a snippet the documentation pulls and that this test
/// feeds to %PDI, so that they are checked to be valid specification trees and
/// not merely valid YAML.

#include <mpi.h>

#include <pdi.h>
#include <pdi/testing.h>

const char* CONFIG_EXAMPLE = R"PDIYAML(
#! [example]
metadata: # small values for which PDI keeps a copy
  width:   int                    # per proc. width including ghost
  height:  int                    # per proc. height including ghost
  pwidth:  int                    # nb proc. in the x dim
  pheight: int                    # nb proc. in the y dim
  iter:    int                    # curent iteration id
  coord:   { type: array, subtype: int, size: 2 } # coordinate of the process as [x, y]
data:     # values that are not copied by PDI
  main_field:
    type: array
    subtype: double
    size: [$width, $height]
plugins:
  mpi: # loading MPI_Comm predefines (e.g. $MPI_COMM_WORLD)
  decl_hdf5: # a list of file to write to (can be a single element)
    file: data${coord[0]}x${coord[1]}.h5 # the file in which to write the data (required)
    collision_policy: write_into_and_warn # print a warning if file or any of dataset already exist
    on_event: newiter                    # the event that triggers these actions (default: trigger on data expose)
    when: "$iter>0 & $iter<11"           # a condition when to actually trigger the actions (default: always true)
    communicator: $MPI_COMM_SELF         # the MPI communicator used for HDF5 parallel synchronized write (default: $MPI_COMM_SELF, sequential write)
    datasets:                            # a list of datasets inside the file created on first access
      data/array: # a dataset name, datasets referenced but not defined are created just big enough to fit the data
        type: array
        subtype: double                # type of the data in the dataset
        size: [10, $width-2, $width-2] # size of the dataset
      data_iter.*/array: # a dataset name which is a regex. This name allows to write datasets that match this regex in the h5 file
        type: array
        subtype: double                # type of the data in the dataset
        size: [$width-2, $width-2]     # size of the dataset
    write: # a list or map of data to write (default: empty)
      main_field: # name of the data, it contains either a list or a single write to execute
        - dataset: data/array      # a dataset name (default: the data name)
          when: "$iter>0&$iter<11" # an additional condition when to actually trigger the actions (default: always true)
          mpio: INDEPENDENT              # the type of MPI-I/O parallel pointer (default: COLLECTIVE)
          memory_selection:
            size:  [$width-2, $height-2] # number of elements to transfer in each dimension (default: size of the full data)
            start: [1, 1]                # coordinate of the start point in memory relative to the shared data (default: 0 in each dimensions)
          dataset_selection:
            size:  [1, $width-2, $width-2] # number of elements to transfer in each dimension, must amount to the same number as the memory selection (default: size of memory slab)
            start: [$iter, 0, 0]           # coordinate of the start point in the file relative to the dataset (default: 0 in each dimensions)
          attributes:
            size: ($width-2)*($width-2)
            width: $width-2
            height: $width-2
        - dataset: data_iter${iter}/array  # a dataset name with an implicit name
          memory_selection:
            size:  [$width-2, $height-2] # number of elements to transfer in each dimension (default: size of the full data)
            start: [1, 1]                # coordinate of the start point in memory relative to the shared data (default: 0 in each dimensions)
          dataset_selection:
            size:  [ $width-2, $width-2] # number of elements to transfer in each dimension, must amount to the same number as the memory selection (default: size of memory slab)
            start: [ 0, 0]               # coordinate of the start point in the file relative to the dataset (default: 0 in each dimensions)

    read: # a list or map of data to read, similar to write (default: empty)
      - another_value
#! [example]
)PDIYAML";

/// A %PDI error makes the test fail with a diagnostic rather than
/// aborting, which is what ::PDI::PdiTest brings over a bare PDI_init.
struct DeclHdf5Doc: public ::PDI::PdiTest {};

TEST_F(DeclHdf5Doc, specification_trees)
{
	InitPdi(PC_parse_string(CONFIG_EXAMPLE));
	FinalizePdi();
}

/// The trees below use MPI communicators, so MPI has to be up for the whole run.
int main(int argc, char** argv)
{
	::testing::InitGoogleTest(&argc, argv);
	MPI_Init(&argc, &argv);
	int result = RUN_ALL_TESTS();
	MPI_Finalize();
	return result;
}
