!*******************************************************************************
! Copyright (C) 2026 Julien Bigot <julien@julien-bigot.fr>
! All rights reserved.
!
! Redistribution and use in source and binary forms, with or without
! modification, are permitted provided that the following conditions are met:
! * Redistributions of source code must retain the above copyright notice,
!   this list of conditions and the following disclaimer.
! * Redistributions in binary form must reproduce the above copyright notice,
!   this list of conditions and the following disclaimer in the documentation
!   and/or other materials provided with the distribution.
! * Neither the names of CEA, nor the names of the contributors may be used to
!   endorse or promote products derived from this software without specific
!   prior written  permission.
!
! THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
! AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
! IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
! ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE
! LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
! CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
! SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
! INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
! CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
! ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
! POSSIBILITY OF SUCH DAMAGE.
!******************************************************************************/


!! The Fortran examples of \ref YAML, the counterpart of yaml_examples.cxx.

program yaml_examples
  use paraconf
  use PDI
  implicit none

  !! [init_whole_file]
  type(PC_tree_t), target :: yaml_tree

  call PC_parse_path("example.yaml", yaml_tree)
  call PDI_init(yaml_tree)
  !! [init_whole_file]
  call PDI_finalize()

  !! [init_subtree]
  call PC_parse_path("example.yaml", yaml_tree)
  call PDI_init(PC_get(yaml_tree, ".pdi_subtree"))
  !! [init_subtree]
  call PDI_finalize()

end program yaml_examples
