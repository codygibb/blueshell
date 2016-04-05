OCB_FLAGS = -use-ocamlfind -I src

OCB = ocamlbuild $(OCB_FLAGS)

all:
	$(OCB) src/main.byte

clean: 
	$(OCB) -clean

unit:
	$(OCB) test/unit/tests.byte
	./tests.byte
	rm tests.byte

integration:
	$(OCB) test/integration/tests.byte
	./tests.byte test/integration/progs
	rm tests.byte

test: unit integration
