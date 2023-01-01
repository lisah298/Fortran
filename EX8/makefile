.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 algorithms.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) $(FCFLAGS) -o $@ $+

algorithms.o algorithms.mod: algorithms.f90
	$(COMPILE.f90) -o algorithms.o $<


main.o: main.f90 algorithms.mod
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main *.txt