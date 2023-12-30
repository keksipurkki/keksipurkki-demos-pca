OBJS := main.o dsvdc.o dispmodule.o putstrmodule.o
PROG := svd
FLAGS := -Wl,-ld_classic -march=native -mtune=native -O3
COMPILER := gfortran $(FLAGS)
COUNTRY_FLAGS = $(wildcard flags/*.png)
BITMAPS := $(patsubst flags/%.png, bitmaps/%.bmp, $(COUNTRY_FLAGS))

all: $(PROG)
	./svd bitmaps/*.bmp

test: $(OBJS) .PHONY
	$(COMPILER) -c -o test.o utils.test.f90
	$(COMPILER) -o test test.o dispmodule.o putstrmodule.o
	./test

assets: $(BITMAPS)

bitmaps:
	mkdir -p bitmaps

bitmaps/%.bmp: bitmaps
	convert flags/$*.png -resize 128x85! -alpha opaque $@

$(PROG): $(OBJS)
	$(COMPILER) -o $@ $^

$(OBJS): %.o: %.f90
	$(COMPILER) -c -o $@ $<

main.o: dsvdc.o dispmodule.o

dispmodule.o: putstrmodule.o

clean:
	rm -rf $(PROG) test *.o *.mod

dist-clean:
	rm -rf bitmaps

.PHONY:
