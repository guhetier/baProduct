OCB_FLAGS = -use-ocamlfind -I src
OCB = 		ocamlbuild $(OCB_FLAGS)

all: 		native byte # profile debug

clean:
			$(OCB) -clean

native: 	sanity
			$(OCB) main.native

byte:		sanity
			$(OCB) main.byte

profile: 	sanity
			$(OCB) -tag profile main.native

debug: 		sanity
			$(OCB) -tag debug main.byte

sanity:
# check that packages can be found
			ocamlfind query yojson

test: 		native
			./main.native -i test.c -s test.spec -o out.c

.PHONY: all clean byte native profile debug sanity test
