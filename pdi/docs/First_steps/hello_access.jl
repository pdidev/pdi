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



function print_secret_msg() 
    # https://docs.julialang.org/en/v1/manual/calling-c-and-fortran-code/#Passing-Pointers-for-Modifying-Inputs-1
    value = Ref{Cint}(0) 
    message = Ref{Cstring}()

    @ccall "libpdi".PDI_access("my_value"::Cstring, value::Ref{Cint}, PDI_IN::Cint)::Cvoid
    
    print("\n value ", value[])
    # print("\n pointer ",value, " \n")

    @ccall "libpdi".PDI_release("my_value"::Cstring)::Cvoid
 
    # @ccall "libpdi".PDI_access("my_message"::Cstring, message::Ref{Ptr{UInt8}}, PDI_IN::Cint)::Cvoid
    @ccall "libpdi".PDI_access("my_message"::Cstring, message::Ref{Cstring}, PDI_IN::Cint)::Cvoid
    # print("\n pointer ", message, " \n")
    print("\n message ", unsafe_string(message[]), " \n")

    @ccall "libpdi".PDI_release("my_message"::Cstring)::Cvoid
end
 

conf = @ccall "libparaconf".PC_parse_path("hello_access.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

my_value = Int32(42)

@ccall "libpdi".PDI_share("my_value"::Cstring, my_value::Cint, PDI_OUT::Cint)::Cvoid

secret_msg = "Watermelon is the tastiest fruit"

@ccall "libpdi".PDI_share("my_message"::Cstring, secret_msg::Cstring, PDI_OUT::Cint)::Cvoid

print_secret_msg()

@ccall "libpdi".PDI_reclaim("my_message"::Cstring)::Cvoid
@ccall "libpdi".PDI_reclaim("my_value"::Cstring)::Cvoid

@ccall "libpdi".PDI_finalize()::Cvoid




