.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 func.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

func.o func.mod: func.f90
	$(COMPILE.f90) -o func.o $<

main.o: main.f90 func.mod
	$(COMPILE.f90) -o main.o $<

.PHONY: clean
clean:
	-rm -f *.o *.mod main