ocamlbuild -lib unix test/test_progs.byte || { echo 'Build failed'; exit 1; }
./test_progs.byte test/progs
