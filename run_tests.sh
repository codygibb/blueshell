ocamlbuild -use-ocamlfind test/integration/tests.byte || { echo 'Build failed'; exit 1; }
./test_integration_progs.byte test/integration/progs
