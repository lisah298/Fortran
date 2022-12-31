.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 construct.f90 ownmatmul.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

construct.o construct.mod: construct.f90
	$(COMPILE.f90) -o construct.o $<

ownmatmul.o ownmatmul.mod: ownmatmul.f90
	$(COMPILE.f90) -o ownmatmul.o $<

main.o: main.f90 construct.mod ownmatmul.mod
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main