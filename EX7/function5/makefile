.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 construct.f90 matmul.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

construct.o construct.mod: construct.f90
	$(COMPILE.f90) -o construct.o $<

matmul.o matmul.mod: matmul.f90
	$(COMPILE.f90) -o matmul.o $<

main.o: main.f90 construct.mod
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main