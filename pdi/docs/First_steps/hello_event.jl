struct PC_tree_t
    # The tree status
    status::Cint
    # The document containing the tree
    document::Ptr{Cvoid}
    # the node inside the tree
    node ::Ptr{Cvoid}
end

# These are the values for enum PDI_inout_t 
PDI_NONE = 0
PDI_IN = 1
PDI_OUT = 2
PDI_INOUT = 3

conf = @ccall "libparaconf".PC_parse_path("hello_event.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

@ccall "libpdi".PDI_event("Hello World Event"::Cstring)::Cvoid

@ccall "libpdi".PDI_finalize()::Cvoid