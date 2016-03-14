## blueshell
Shell scripting language

![Image of Mario Kart blue shell]
(http://orig02.deviantart.net/e538/f/2013/036/b/b/blue_shell_the_______by_dreamingsora-d5tye41.png)

Download the dependecies first:

    $ apt-get install ocaml
    $ apt-get install opam
    $ opam install menhir
    $ opam install core
    $ opam install ppx_jane
    $ opam install re2
    $ opam install oUnit

Make sure the tests pass:

    $ ./run_tests.sh
    
To run arbitrary `.blu` files:

    $ ocamlbuild -use-ocamlfind main.byte
    $ ./main.byte test.blu
