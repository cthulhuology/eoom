#
# Makefile for Erlang Out Of Memory
#

BEAMS = $(patsubst %.erl, %.beam, $(wildcard *.erl))

all: $(BEAMS)

%.beam : %.erl
	erlc $<


.PHONY: clean console
clean:
	rm *.beam

console: $(BEAMS)
	erl -s toolbar 

