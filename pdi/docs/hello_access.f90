subroutine print_secret_msg
  use pdi
  implicit none

  integer, pointer               :: p_val
  character, pointer, contiguous :: p_msg(:)
  integer                        :: msg_ranks(1)

  call pdi_access("value", p_val, pdi_in)
  print *, "PDI value: ", p_val
  call pdi_release("value")

  ! In case of accessing arrays, PDI_access takes additional array argument with dimensions sizes
  msg_ranks(1) = 32
  call PDI_access("message", p_msg, pdi_in, msg_ranks)
  print *, "PDI message: ", p_msg
  call pdi_release("message")
  return
end subroutine print_secret_msg

program access
  use paraconf
  use pdi
  implicit none      

  type(pc_tree_t),target  :: conf
  integer                 :: my_value
  character(LEN=32)      :: secret_message

  call pc_parse_path("hello_access.yml", conf)
  call pdi_init(conf) 

  my_value = 42
  secret_message = "Watermelon is the tastiest fruit"                                 
  print *, "My value: ", my_value
  print *, "My message: ", secret_message

  call pdi_share("value", my_value, pdi_out)      
  call pdi_share("message", secret_message, pdi_out)

  call print_secret_msg()

  call pdi_reclaim("message")
  call pdi_reclaim("value")
  call pdi_finalize()
end program access
