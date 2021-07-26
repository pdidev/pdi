program hello_data
    use paraconf
    use PDI
    implicit none

    type(PC_tree_t),target :: conf
    integer                :: my_world

    call PC_parse_path("hello_data.yml", conf)
    call PDI_init(conf)

    my_world = 42
    call PDI_share("world", my_world, PDI_OUT)
    !variable my_world is shared with PDI

    call PDI_reclaim("world");
    !variable my_world is no longer shared with PDI

    call PDI_finalize()

end program hello_data
