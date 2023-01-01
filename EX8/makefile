.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 

main: $(subst .f90,.o,$(SOURCES))
	$(FC) $(FCFLAGS) -o $@ $+

#construct.o construct.mod: construct.f90
#	$(COMPILE.f90) -o construct.o $<


main.o: main.f90 
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main *.txt