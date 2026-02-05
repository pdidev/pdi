! SPDX-FileCopyrightText: 2015-2019 Commissariat a l'energie atomique et aux energies alternatives (CEA)
!
! SPDX-License-Identifier: BSD-3-Clause

module PDI_C

  use ISO_C_binding
  use paraconf

  implicit none

interface

  function PDI_init_C(treeconf) bind(C, name="PDI_init")
    use ISO_C_binding
    use paraconf
    integer(C_int) :: PDI_init_C
    type(PC_tree_t), value :: treeconf
  endfunction PDI_init_C
  
  
  function PDI_finalize_C() bind(C, name="PDI_finalize")
    use ISO_C_binding
    integer(C_int) :: PDI_finalize_C
  endfunction PDI_finalize_C
  
  
  function PDI_event_C(event) bind(C, name="PDI_event")
    use ISO_C_binding
    integer(C_int) :: PDI_event_C
    type(C_ptr), value :: event
  endfunction PDI_event_C
  
  
  function PDI_share_C(name, data, accessf) bind(C, name="PDI_share")
    use ISO_C_binding
    integer(C_int) :: PDI_share_C
    type(C_ptr), value :: name
    type(C_ptr), value :: data
    integer(C_int), value :: accessf
  endfunction PDI_share_C
  
  
  function PDI_release_C(name) bind(C, name="PDI_release")
    use ISO_C_binding
    integer(C_int) :: PDI_release_C
    type(C_ptr), value :: name
  endfunction PDI_release_C
  
  
  function PDI_reclaim_C(name) bind(C, name="PDI_reclaim")
    use ISO_C_binding
    integer(C_int) :: PDI_reclaim_C
    type(C_ptr), value :: name
  endfunction PDI_reclaim_C
  
  
  function PDI_expose_C(name, data, accessf) bind(C, name="PDI_expose")
    use ISO_C_binding
    integer(C_int) :: PDI_expose_C
    type(C_ptr), value :: name
    type(C_ptr), value :: data
    integer(C_int), value :: accessf
  endfunction PDI_expose_C
  
  
  function PDI_access_C(name, data, accessf) bind(C, name="PDI_access")
    use ISO_C_binding
    integer(C_int) :: PDI_access_C
    type(C_ptr), value :: name
    type(C_ptr), intent(OUT) :: data
    integer(C_int), value :: accessf
  endfunction PDI_access_C

  
  function PDI_transaction_begin_C( name ) bind(C, name="PDI_transaction_begin")
    use ISO_C_binding
    integer(C_int) :: PDI_transaction_begin_C
    type(C_ptr), value :: name
  endfunction PDI_transaction_begin_C
  
  
  function PDI_transaction_end_C() bind(C, name="PDI_transaction_end")
    use ISO_C_binding
    integer(C_int) :: PDI_transaction_end_C
  endfunction PDI_transaction_end_C
  
endinterface

endmodule PDI_C
