program hello_event
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t), target :: conf

    call PC_parse_path("hello_event.yml", conf)
    call PDI_init(conf)

    call PDI_event("Hello World Event")

    call PDI_finalize()

end program hello_event
