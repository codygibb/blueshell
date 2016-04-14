open Core.Std
open OUnit2

let suite =
  "Shell">:::
  [
    "expand_path_simple">::(fun ctx ->
      let env = String.Map.of_alist_exn [
        ("HOME", "/Users/codygibb");
        ("GOPATH", "/Users/codygibb/dev/go");
        ("PI", "3.14");
        ("FOO", "foo");
        ("BAR", "$FOO/bar");
        ("BAZ", "$BAR/baz");
      ]
      in
      let getenv = Map.find env in
      let expand_path = Shell.expand_path ~getenv in

      assert_equal "normal/path" (expand_path "normal/path");

      assert_equal
        ((Option.value_exn (getenv "HOME")) ^ "/foo")
        (expand_path "~/foo");

      assert_equal
        ((Option.value_exn (getenv "GOPATH")) ^ "/../ocaml")
        (expand_path "$GOPATH/../ocaml");

      assert_equal "foo/bar/baz" (expand_path "$BAZ");

      ()
    )
  ]
