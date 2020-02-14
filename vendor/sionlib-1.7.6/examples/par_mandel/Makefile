CC           = mpicc
CFLAGS       =

#SIONlib
SIONCONFIG = ../../bin/sionconfig
PREC=64

#----------------------------------------

SION_LDFLAGS = $(shell ${SIONCONFIG} --libs --mpi --${PREC})
SION_CFLAGS  = $(shell ${SIONCONFIG} --cflags --mpi --${PREC})

LIBS         = -lm $(SION_LDFLAGS)
USER_CFLAGS  = $(SION_CFLAGS) $(CFLAGS)

#------------ filenames ----------------
GEN_SRC = mandelsion.c
PAR_SRC = $(GEN_SRC) mandelmpi.c
PAR_OBJ = $(PAR_SRC:%.c=%.o)
SEQ_SRC = $(GEN_SRC) mandelseq.c ppmwrite.c
SEQ_OBJ = $(SEQ_SRC:%.c=%.o)
ADD_DEP = Makefile
OUTPUT  = mandelcol.ppm mandelcol_procs.ppm simple.sion
PAR_BIN = mandelmpi
SEQ_BIN = mandelseq

#-------------------------------------------

.PHONY: all default clean

all: $(PAR_BIN) $(SEQ_BIN)

default: all

clean:
	rm -f *.o $(PAR_BIN) $(SEQ_BIN) $(OUTPUT)

%.o: %.c %.h $(ADD_DEP)
	$(CC) $(USER_CFLAGS) -c $< -o $@
	
$(PAR_BIN): $(PAR_OBJ)
	$(CC) -o $@ $^ $(LIBS)

$(SEQ_BIN): $(SEQ_OBJ)
	$(CC) -o $@ $^ $(LIBS)
