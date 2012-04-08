#
# Makefile for Erlang Out Of Memory
#

BEAMS = $(patsubst %.erl, %.beam, $(wildcard *.erl))

all: $(BEAMS)

%.beam : %.erl
	erlc $<


.PHONY: clean
clean:
	rm *.beam

