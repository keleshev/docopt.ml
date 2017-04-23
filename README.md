# Pseudo-code

```ocaml
(* Phase I: potentially compile-time *)

let reconcile_options ast options =
  let ast = resolve_option_aliases ast options in
  let typed_ast = infer_types ast in
  let typed_options = extract_options typed_ast in
  typed_ast, typed_options

let parse_docstring docstring =
  let ast = parse_usage_section docstring in
  let options = parse_options_section docstring in
  let typed_ast, typed_options = reconcile_options ast options in
  typed_ast, typed_options

let typed_ast, typed_options = parse_docstring docstring

(* Phase II: run-time *)

let docopt argv =
  let tokens = tokenize argv in
  let option_results, positional_args = parse_argv tokens typed_options in
  let map = patter_match ~pattern:typed_ast option_results positional_argv in
  map
```
