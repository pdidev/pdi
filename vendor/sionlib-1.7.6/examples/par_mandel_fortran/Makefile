FC           = mpif90
CC           = mpicc
CFLAGS       =
FFLAGS       =

#SIONlib
SIONCONFIG   = ../../bin/sionconfig
PREC         = 64

#----------------------------------------

SION_LDFLAGS = $(shell ${SIONCONFIG} --libs --mpi --f90 --${PREC})
SION_CFLAGS  = $(shell ${SIONCONFIG} --cflags --mpi --f90 --${PREC})

LIBS         = -lm $(SION_LDFLAGS)
USER_FFLAGS  = $(SION_CFLAGS) $(FFLAGS)

#------------ filenames ----------------
GEN_SRC = infostruct.f90 mandelsion.f90
GEN_OBJ = $(GEN_SRC:%.f90=%.o)
GEN_MOD = $(GEN_SRC:%.f90=%.mod)
PAR_SRC = mandelmpi.f90
PAR_OBJ = $(PAR_SRC:%.f90=%.o)
SEQ_SRC = mandelseq.f90
SEQ_OBJ = $(SEQ_SRC:%.f90=%.o)
ADD_DEP = Makefile
PAR_BIN = mandelmpi
SEQ_BIN = mandelseq
OUTPUT  = mandelcol.ppm mandelcol_procs.ppm simple.sion

#-------------------------------------------

.PHONY: all default clean

all: $(PAR_BIN) $(SEQ_BIN)

default: all

clean:
	rm -f *.o *.mod $(PAR_BIN) $(SEQ_BIN) $(OUTPUT)

%.o: %.f90 $(ADD_DEP)
	$(FC) $(USER_FFLAGS) -c $< -o $@

%.mod: %.o ;
	
ppmwrite.mod: ppmwrite_fortran.o ;

ppmwrite.o: ppmwrite.c $(ADD_DEP)
	$(CC) $(CFLAGS) -c $< -o $@

$(PAR_OBJ): $(PAR_SRC) $(GEN_MOD)
	$(FC) $(USER_FFLAGS) -c $< -o $@

$(SEQ_OBJ): $(SEQ_SRC) $(GEN_MOD) ppmwrite.mod
	$(FC) $(USER_FFLAGS) -c $< -o $@

$(PAR_BIN): $(GEN_OBJ) $(PAR_OBJ)
	$(FC) -o $@ $^ $(LIBS)

$(SEQ_BIN): $(GEN_OBJ) ppmwrite_fortran.o ppmwrite.o $(SEQ_OBJ)
	$(FC) -o $@ $^ $(LIBS)
