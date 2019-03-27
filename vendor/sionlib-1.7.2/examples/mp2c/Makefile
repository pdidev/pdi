include Makefile.defs

LDFLAGS += `../../bin/sionconfig --libs --ser --fe --f90`
FFLAGS += `../../bin/sionconfig --cflags --ser --fe --f90`

# -- Rules --
all: sionmp2c

clean:
	rm -rf sionmp2c a.* *.o

sionmp2c.o: sionmp2c.f90
	$(F90) sionmp2c.f90 -c $(FFLAGS)

sionmp2c: sionmp2c.o
	$(F90) sionmp2c.o -o sionmp2c $(LDFLAGS)
