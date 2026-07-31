program expose
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t),target :: conf
    integer                :: my_world

    call PC_parse_path("hello_data.yml", conf)
    call PDI_init(conf)

    my_world = 42
    
    !! [share_reclaim]
    call PDI_share("world", my_world, PDI_OUT)
    call PDI_reclaim("world");
    !! [share_reclaim]
    
    !! [expose]
    call PDI_expose("world", my_world, PDI_OUT)
    !! [expose]

    call PDI_finalize()
end program expose
