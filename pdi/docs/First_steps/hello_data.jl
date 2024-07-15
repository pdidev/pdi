struct PC_tree_t
    # The tree status
    status::Cint
    # The document containing the tree
    document::Ptr{Cvoid}
    # the node inside the tree
    node ::Ptr{Cvoid}
end

# These are the values for enum PDI_inout_t 
PDI_NONE  = 0
PDI_IN    = 1
PDI_OUT   = 2
PDI_INOUT = 3

conf = @ccall "libparaconf".PC_parse_path("hello_data.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

my_world = Int32(42)

print("\n with PDI_share and PDI_reclaim \n")

@ccall "libpdi".PDI_share("world"::Cstring,my_world::Cint,PDI_OUT::Cint)::Cvoid
# variable my_world is shared with PDI

@ccall "libpdi".PDI_reclaim("world"::Cstring)::Cvoid
# variable my_world is no longer shared with PDI

@ccall "libpdi".PDI_finalize()::Cvoid









