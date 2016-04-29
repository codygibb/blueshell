# blueshell

A lightweight shell scripting language

<img src="assets/blueshell.png" width="200"> 

Blueshell (a working title) focuses on allowing the programmer to write better Bash scripts. Any Bash script can and should be instead written in Blueshell. 

**But why?**

Blueshell offers predictable syntax and useful high-level constructs, while still providing a direct and simple interface to execute Bash commands.

Some useful constructs allow direct and simple access to the shell:

`$> touch foo.txt` Runs a command, exiting the script if the command fails. Stdout, stderr, and stdin are connected directly to the terminal. 

`out = $> ls` Runs a command and captures its output, exiting the script if the command fails. Stderr and stdin are ignored.

`out, err = try $> python might_fail.py` Try to run a command and capture its output. If the command exits non-zero, then the error code is also returned and the script does not exit.

`try $> python might_fail.py` Similar to the above, except leaves stdout, stderr, and stdin connected to the terminal, and ignores the error code. Semantically, the standalone `try` is the same as any command in a Bash script.

`$> echo "${'Hello, ' + 'world!'}" > ${file}.txt` Expressions can be injected into commands with `${E}`.

`cd '~/dir' { ... }` Execute a block of code within a given directory.

Example:

    ok := true
    cd '~/dev/blueshell' {
    	$> ocamlbuild -use-ocamlfind src/main.byte
    	tests := $> ls test/integration/progs/basic/*.blu
    	for t in tests.split('\n') {
    		exp_out := $> cat ${t}.out
    		out, err := try $> ./main.byte ${t}
    		if err != 0 || out != exp_out {
    			print t + ' failed'
    			ok = false
    		}
    	}
    }
    if ok {
        print 'All tests passed!'
    }

The above Blueshell script compiles the Blueshell interpreter and runs all of our basic integration tests, making sure all of the tests exit cleanly and produce expected output!

## TODO list:

- Error objects
- Change `print` to `println`, and then make `print` not output newlines.
- HTTP support

## Built-in

`argv` List of command line arguments, with the running file at index 0.

`max(n1, n2)` **TODO** Returns max of `n1` and `n2`, which can be either floats or ints.

#### str methods

`s[i]` Returns character at `i` in `s`.

`s[i] = c` Sets character at `i` to `c` in `s`. The length of `c` must be 1.

`s[start:stop]` Returns a slice of `s` from index `start` (inclusive) to index `stop` (exclusive). If `start` or `stop` is omitted, then `0` and `s.len()` is used, respectively. The `stop` index is normalized, meaning `-1` evals to `s.len()-1`, `-2` evals to `s.len()-2`, etc.

`s.len()` Returns length of `s`.

`s.split(pattern)` Splits `s` according to regex string `pattern` and returns list of substrings.

`s.replace(pattern, s2)` Replaces substrings in `s` matched by regex string `pattern` with string `s2`.

`s.fmt(...)` Returns new string with embedded formats of `"{}"` in `s` replaced by corresponding argument by position (`fmt` accepts variable number of arguments). Values will be automatically converted to strings. Example: `"Hello, {}! The number is {}.".fmt('Cody', 42)` returns `"Hello, Cody! The number is 42."`.

#### list methods

`l[i]` Returns element at `i` in `l`.

`l[i] = x` Sets element at `i` to `x` in `l`.

`l[start:stop]` Returns a slice of `l`. See `str` slice for details.

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

`d.del(k)` Deletes key `k` from `d`.

#### file methods

`f.read()` **TODO** Returns string contents of `f`.

`f.lines()` **TODO** Returns iterator over the lines of `f`.

`f.write(s)` **TODO** Overwrites contents of `f` with string `s`.

`f.append(s)` **TODO** Appends string `s` to the end of `f`.
