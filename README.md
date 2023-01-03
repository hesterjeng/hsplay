# hsplay

This is a WIP project meant to be an implementation of splay trees done in the style of the `containers` library for OCaml.
The implementation is mostly imperative and is not guaranteed to work correctly.

# Usage

To install the library, you should be able to clone this repository, and then install it through `opam` via `opam install .` in the project's directory.

```
module S = Hsplay.Make (CCInt);;
let s = S.create ();;
S.insert s 6;;
S.insert s 4;;
S.mem s 3;;
S.mem s 4;;
```

`


