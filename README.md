## blueshell
Shell scripting language

![Image of Mario Kart blue shell]
(http://orig02.deviantart.net/e538/f/2013/036/b/b/blue_shell_the_______by_dreamingsora-d5tye41.png)

Download `ocaml`, `opam`, `menhir`, `core`, and `ppx_jane` first:

    $ apt-get install ocaml
    $ apt-get install opam
    $ opam install menhir
    $ opam install core
    $ opam install ppx_jane

Then compile and run:

    $ ocamlbuild --use-ocamlfind main.byte
    $ ./main.byte test.blu
