program hello_multi_expose
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t)   :: conf
    integer           :: my_int
    real              :: my_float
    character         :: my_string(26)

    call PC_parse_path("hello_transaction.yml", conf)
    call PDI_init(conf)

    my_int = 0
    my_float = 0
    my_string = (/'R','G','B',' ','=',' ','R','e','a','l','l','y',' ',&
         'G','a','w','k','y',' ','B','i','s','c','u','i','t'/)
    
    call PDI_transaction_begin("event_between")
    
    call PDI_expose("my_int", my_int, PDI_OUT)
    call PDI_expose("my_float", my_float, PDI_OUT)
    call PDI_expose("my_string", my_string, PDI_OUT)
    
    call PDI_transaction_end()

    call PDI_finalize()

end program hello_multi_expose
