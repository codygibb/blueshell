echo ">>> Unit tests <<<"
echo
ocamlbuild -I src -use-ocamlfind test/unit/tests.byte || { echo 'Build failed'; exit 1; }
./tests.byte
rm tests.byte
echo
echo ">>> Integration tests <<<"
echo
ocamlbuild -I src -use-ocamlfind test/integration/tests.byte || { echo 'Build failed'; exit 1; }
./tests.byte test/integration/progs
rm tests.byte
