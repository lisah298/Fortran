.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c
FCFLAGS=-lblas

SOURCES=main.f90 construct.f90 ownmatmul.f90 trace.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) $(FCFLAGS) -o $@ $+

construct.o construct.mod: construct.f90
	$(COMPILE.f90) -o construct.o $<

ownmatmul.o ownmatmul.mod: ownmatmul.f90
	$(COMPILE.f90) -o ownmatmul.o $<

trace.o trace.mod: trace.f90
	$(COMPILE.f90) -o trace.o $<

main.o: main.f90 construct.mod ownmatmul.mod trace.mod
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main