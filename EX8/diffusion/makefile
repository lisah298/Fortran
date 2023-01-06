.SUFFIXES:
FC=gfortran
COMPILE.f90 = $(FC) -c

SOURCES=main.f90 einstein.f90 greenkubo.f90

main: $(subst .f90,.o,$(SOURCES))
	$(FC) -o $@ $+

einstein.o einstein.mod: einstein.f90 
	$(COMPILE.f90) -o einstein.o $<

greenkubo.o greenkubo.mod: greenkubo.f90 
	$(COMPILE.f90) -o greenkubo.o $<


main.o: main.f90 einstein.mod greenkubo.mod
	$(COMPILE.f90) -o main.o $<


.PHONY: clean
clean:
	-rm -f *.o *.mod main *.txt