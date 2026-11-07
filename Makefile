all: build

build:
	cd tp1; \
	dune build; \
	ocamlbuild netlist_simulator.byte

run:
	cd tp1; \
	./netlist_simulator.byte $(ARGS)

clean:
	cd tp1; \
	dune clean