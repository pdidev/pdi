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

# Version: julia +1.10.4

conf = @ccall "libparaconf".PC_parse_path("hello_multi_expose.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

x = Int32(0)
y = Float32(0.0)
z = "RGB = Really Gawky Biscuit"

@ccall "libpdi".PDI_multi_expose("event_between"::Cstring, 
                 "my_int"::Cstring, x::Ref{Cint}, PDI_OUT::Cint,
                 "my_float"::Cstring, y::Ref{Cfloat}, PDI_OUT::Cint,
                 "my_string"::Cstring, z::Cstring, PDI_OUT::Cint,
                 C_NULL::Ptr{Cvoid})::Cvoid

@ccall "libpdi".PDI_finalize()::Cvoid




