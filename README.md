# blueshell
Lightweight shell scripting language

![Image of Mario Kart blue shell]
(http://orig02.deviantart.net/e538/f/2013/036/b/b/blue_shell_the_______by_dreamingsora-d5tye41.png)

## Built-in

`argv` List of command line arguments, with the running file at index 0.

`max(n1, n2)` **TODO** Returns max of `n1` and `n2`, which can be either floats or ints.

#### str methods

`s[i]` Returns character at `i` in `s`.

`s[i] = c` Sets character at `i` to `c` in `s`. The length of `c` must be 1.

`s[start:stop]` **TODO** Returns a slice of `s` from index `start` (inclusive) to index `stop` (exclusive). If `start` or `stop` is omitted, then `0` and `s.len()` is used, respectively. The `stop` index is normalized, meaning `-1` evals to `s.len()-1`, `-2` evals to `s.len()-2`, etc.

`s.len()` Returns length of `s`.

`s.split(on)` Splits `s` according to delimiter str `s` and returns list of substrings.

`s.replace(pattern, s2)` **TODO** Replaces substrings in `s` matched by regex string `pattern` with string `s2`.

`s.fmt(d)` **TODO** Returns new string with embedded formats of `{key}` in `s` replaced by corresponding value in dict `d`. Values will be automatically converted to strings. Example: `"Hello, {name}! The number is {n}.".fmt({'name': 'Cody', 'n': 42})` returns `"Hello, Cody! The number is 42."`

#### list methods

`l[i]` Returns element at `i` in `l`.

`l[i] = x` Sets element at `i` to `x` in `l`.

`l[start:stop]` **TODO** Returns a slice of `l`. See `str` slice for details.

`l.len()` Returns length of `l`.

`l.push(x)` Pushes `x` to end of `l`.

`l.pushf(x)` **TODO** Pushes `x` to front of `l`.

`l.pop()` Removes and returns last element of `l`.

`l.popf()` **TODO** Removes and returns first element of `l`.

`l.extend(l2)` **TODO** Adds all elements of list `l2` to end of `l`.

`l.map(f)` **TODO** Applies function `f` to every element of `l`, returning a new list with the results.

`l.filter(f)` **TODO** Filters `l` by function `f`, returning a new list with elements that `f` returns `true` on.

`l.sort(f)` **TODO** Sorts `l` using comparator function `f`.


#### dict methods

`d[k]` Returns corresponding value of key `k` in `d`.

`d[k] = v` Sets key `k` to value `v` in `d`.

`d.len()` Returns length of `d`.

`d.del(k)` **TODO** Deletes key `k` from `d`.

#### file methods

`f.read()` **TODO** Returns string contents of `f`.

`f.lines()` **TODO** Returns iterator over the lines of `f`.

`f.write(s)` **TODO** Overwrite contents of `f` with string `s`.

`f.append(s)` **TODO** Append string `s` to the end of `f`.

## Build

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
