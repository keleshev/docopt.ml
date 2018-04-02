open StdLabels
module List = List

let (>>) f g x = g (f x)
let printf = Printf.printf

module Option = struct
  let value ~default = function
    | None -> default
    | Some x -> x
end

module Int = struct
  let of_string_exn = int_of_string

  let of_string s =
    try Some (of_string_exn s)
    with _ -> None
end

module String = struct
  include String

  let to_list string =
    let rec go i tail =
      if i < 0 then tail else go (i - 1) (string.[i] :: tail) in
    go (length string - 1) []

  let of_char_list chars =
    let array = Array.of_list chars in
    String.init (Array.length array) ~f:(Array.get array)

  let fold string ~init ~f =
    let length = length string in
    let rec go i acc =
      if i = length then acc else go (i + 1) (f acc string.[i]) in
    go 0 init

  let is_prefix =
    let rec go string ~prefix i =
      i < 0 || ((prefix.[i] = string.[i]) && go string ~prefix (i - 1)) in
    fun string ~prefix ->
      let prefix_len = length prefix in
      length string >= prefix_len && go string ~prefix (prefix_len - 1)

  let index_exn = index

  let index string char =
    try Some (index_exn string char)
    with Not_found -> None

  (* Extensions *)

  let partition ~on string = (* Similar to Python's *)
    match index string on with
    | None -> string, None
    | Some i ->  (* "--hai=bye" *)
        let left = sub string ~pos:0 ~len:i in
        let i = i + 1 in
        let right = sub string ~pos:i ~len:(length string - i) in
        left, Some right

  let get_opt string i =
    try Some string.[i]
    with Invalid_argument _ -> None
end

module Char = struct
  let to_int = Char.code
  let to_string = String.make 1
  let is_digit = function '0'..'9' -> true | _ -> false
  let get_digit_exn = let offset = to_int '0' in fun char ->
    assert (is_digit char);
    to_int char - offset
end


