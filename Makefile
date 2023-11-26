OBJS := main.o dsvdc.o dispmodule.o putstrmodule.o
PROG := svd
FLAGS := -Wl,-ld_classic
COMPILER := gfortran $(FLAGS)
COUNTRY_FLAGS = $(wildcard flags/*.png)
BITMAPS := $(patsubst flags/%.png, bitmaps/%.bmp, $(COUNTRY_FLAGS))

all: $(PROG) $BITMAPS
	./svd bitmaps/*

bitmaps/%.bmp:
	echo $@ $< $^

$(PROG): $(OBJS)
	$(COMPILER) -o $@ $^

$(OBJS): %.o: %.f90
	$(COMPILER) -c -o $@ $<

main.o: dsvdc.o dispmodule.o

dispmodule.o: putstrmodule.o

clean:
	rm -rf $(PROG) *.o *.mod

dist-clean:
	rm -rf bitmaps
