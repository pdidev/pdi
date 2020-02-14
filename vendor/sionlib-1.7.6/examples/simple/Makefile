include Makefile.defs

LDFLAGS_SER = $(LDFLAGS) `../../bin/sionconfig --libs`
CFLAGS_SER = $(CFLAGS) `../../bin/sionconfig --cflags`

LDFLAGS += `../../bin/sionconfig --libs --mpi --be --64`
CFLAGS += `../../bin/sionconfig --cflags --mpi --be --64`

FLDFLAGS += `../../bin/sionconfig --libs --mpi --be --64 --f90`
FCFLAGS += `../../bin/sionconfig --cflags --mpi --be --64 --f90`

# -- Rules --
all: sionser sionpar sionfortran

clean:
	rm -rf sionpar sionser sionfortran a.* *.o

sionpar.o: sionpar.c
	$(MPICC) sionpar.c -c $(CFLAGS)

sionpar: sionpar.o
	$(MPICC) sionpar.o -o sionpar $(LDFLAGS)

sionser.o: sionser.c
	$(CC) sionser.c -c $(CFLAGS_SER)

sionser: sionser.o
	$(CC) sionser.o -o sionser $(LDFLAGS_SER)

sionfortran.o: sionfortran.f90
	$(MPIF90) sionfortran.f90 -c $(FCFLAGS)

sionfortran: sionfortran.o
	$(MPIF90) sionfortran.o -o sionfortran $(FLDFLAGS)
