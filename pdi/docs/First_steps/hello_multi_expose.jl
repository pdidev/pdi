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

conf = @ccall "libparaconf".PC_parse_path("hello_multi_expose.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

# x = 0
# print("\n ",typeof(x))
# x = Int(0)
# x = Integer(0)

x = Int32(0)
# x=Cint(0)
# testxpdi = Int32(0)


# y = 0.0
y = Float32(0.0)
z = "RGB = Really Gawky Biscuit"

# print("\n ",typeof(x))
# print("\n ",typeof(y))
# print("\n ",typeof(z))

# print("\n int ", x)
# print("\n float ", y, " \n")

# value = Cint(0) 



# @ccall "libpdi".PDI_transaction_begin_C("event_between"::Cstring)::Cvoid

# @ccall "libpdi".PDI_expose("my_int"::Cstring, my_int::Cint, pdi_out)::Cvoid
# @ccall "libpdi".PDI_expose("my_float"::Cstring, my_float::Cfloat, pdi_out)::Cvoid
# @ccall "libpdi".PDI_expose("my_string"::Cstring, my_string::Cstring, pdi_out)::Cvoid

# @ccall "libpdi".PDI_transaction_begin_C()

@ccall "libpdi".PDI_multi_expose("event_between"::Cstring, 
                "world"::Cstring, x::Cint, PDI_OUT::Cint,
                 "my_float"::Cstring, y::Cfloat, PDI_OUT::Cint,
                 "my_string"::Cstring, z::Cstring, PDI_OUT::Cint,
                 C_NULL::Ptr{Cvoid})::Cvoid

                 #C_NULL::Ptr{Nothing}
                 # "my_value"::Cstring, value::Cint, PDI_OUT::Cint,
                #  "my_int"::Cstring, x::Cint, PDI_OUT::Cint,

#Result changes with str of "my_int"


@ccall "libpdi".PDI_finalize()::Cvoid




