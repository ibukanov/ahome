.PHONY: all clean

all:
clean:

clean_list := 

hs_sources := Main.hs
exe := test

clean_list += $(exe) $(hs_sources:.hs=.o) $(hs_sources:.hs=.hi)

$(exe) : $(hs_sources)
	ghc -o $@ -O2 $^

all: $(exe)

clean:
	rm -f $(clean_list)

