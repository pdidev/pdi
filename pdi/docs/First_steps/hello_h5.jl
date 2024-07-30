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

# inspired from heat-sim/io.yml

using MPI
MPI.Init()

comm = MPI.COMM_WORLD

# Version: julia +1.10.4

conf = @ccall "libparaconf".PC_parse_path("hello_h5.yml"::Cstring)::PC_tree_t

@ccall "libpdi".PDI_init(conf::PC_tree_t)::Cvoid

@ccall "libpdi".PDI_event("testing"::Cstring)::Cvoid

# Send meta-data to PDI
mpi_coords_x = 1
mpi_coords_y = 1
mpi_max_coords_x = 1
mpi_max_coords_y = 1
n=32
nx=n
ny=n
time = Float64(15.0)
nwrite = 0
pdi_array =zeros(nx,ny)

for j in 1:ny
    for i in 1:nx
        pdi_array[j,i]=1000*i+j
    end
end

@ccall "libpdi".PDI_multi_expose("init_PDI"::Cstring, 
         "mpi_coords_x"::Cstring, mpi_coords_x::Ref{Cint}, PDI_OUT::Cint,
         "mpi_coords_y"::Cstring, mpi_coords_x::Ref{Cint}, PDI_OUT::Cint,
         "mpi_max_coords_x"::Cstring, mpi_max_coords_x::Ref{Cint}, PDI_OUT::Cint,
         "mpi_max_coords_y"::Cstring, mpi_max_coords_y::Ref{Cint}, PDI_OUT::Cint,
         "nx"::Cstring, nx::Ref{Cint}, PDI_OUT::Cint,
         "ny"::Cstring, ny::Ref{Cint}, PDI_OUT::Cint,
         C_NULL::Ptr{Cvoid})::Cvoid

print("\n before write \n ")

# Expose the solution
@ccall "libpdi".PDI_multi_expose("write_data"::Cstring,
            "nwrite"::Cstring, nwrite::Ref{Cint}, PDI_OUT::Cint,
            "time"::Cstring, time::Ref{Cdouble}, PDI_OUT::Cint,
            "main_field"::Cstring, pdi_array::Ptr{Cdouble}, 
            PDI_OUT::Cint,
            C_NULL::Ptr{Cvoid})::Cvoid

print("\n after write \n ")

@ccall "libpdi".PDI_finalize()::Cvoid


