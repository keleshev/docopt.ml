open Shim

module Source = struct
  module Core : sig
    type t = private {index: int; string: string}
    val of_string : string -> t
    val next : t -> (char * t) option
  end = struct
    type t = {index: int; string: string}

    let invariants {index; string} =
      index >= 0 && index <= String.length string

    let create_exn index string =
      let source = {index; string} in
      assert (invariants source);
      source

    let of_string =
      create_exn 0 (* exn: index of 0 is valid for all strings *)

    let next {index; string} =
      match String.get_opt string index with
      | None -> None
      | Some char ->
          (* exn: in this branch `index` is always a valid index of `string`
                  so `index + 1` will at most equal to `length string` *)
          Some (char, create_exn (index + 1) string)
  end

  include Core

  let to_string {index; string} = (* port slice to shim *)
    let length = String.length string in
    String.sub string ~pos:index ~len:(length - index)

  let empty = of_string ""
end

type 'result t = Source.t -> ('result * Source.t) option

let (>>=) parse f source =
  match parse source with
  | Some (result, remaining) -> f result remaining
  | None -> None

let (>=>) parser1 parser2 = parser1 >>= fun _ -> parser2

let (</>) parser1 parser2 source =
  match parser1 source with
  | Some _ as result -> result
  | None -> parser2 source

let (<|>) parser1 parser2 source =
  match parser1 source, parser2 source with
  | None, None -> None
  | Some _ as result, None -> result
  | None, (Some _ as result) -> result
  | Some _, Some _ -> failwith ("Ambiguous parse: " ^ Source.to_string source)

let fail : 'a t = fun _ -> None

let return result : 'a t = fun source -> Some (result, source)

let not_followed_by parser source =
  match parser source with
  | Some _ -> None
  | None -> Some ((), source)

let followed_by parser =
  not_followed_by (not_followed_by parser)

let eof source = match Source.next source with
  | None -> Some ((), Source.empty)
  | _ -> None

let option parser =
  (</>)
    (parser >>= fun result -> return (Some result))
    (return None)

let default value parser = parser </> return value

(* This might appear to be an identity function, but this will soon
   change when parser will switch to `result`, while Source will continue
   using `option` type. *)
let any source = match Source.next source with (* id? *)
  | Some (char, source) -> Some (char, source)
  | None -> None

let cons parser parsers =
  parser >>= fun result ->
  parsers >>= fun results ->
  return (result :: results)

let (<::>) = cons

let rec zero_or_more (parser : 'a t) =
  default [] (parser >>= fun result ->
    zero_or_more parser >>= fun results ->
    return (result :: results))

let one_or_more (parser : 'a t) =
  parser <::> (zero_or_more parser)

let satisfy predicate =
  any >>= fun char ->
  if predicate char then return char else fail

let parse parser source =
  match parser source with
  | Some (result, _) -> Some result
  | None -> None

let between left right =
  satisfy (fun char -> left <= char && char <= right)

let separated ~by parser =
  parser >>= fun first ->
  zero_or_more (by >=> parser) >>= fun rest ->
  return (first :: rest)

module Char = struct
  let set string =
    satisfy (String.contains string)

  let zero_or_more parser =
    zero_or_more parser >>= fun chars ->
    return (String.of_char_list chars)

  let one_or_more (parser : 'a t) =
    parser >>= fun first ->
    zero_or_more parser >>= fun rest ->
    return (Char.to_string first ^ rest)

  let one char = satisfy ((=) char)

  let whitespace = set " \t\n\r"
end

let char = Char.one

let string string : string t =
  String.fold string ~init:(return ()) ~f:(fun parser char ->
    parser >=> Char.one char >=> return ()) >>= fun () ->
  return string

module Decimal = struct
  let digit = between '0' '9'
  let zero = char '0'
  let non_zero = between '1' '9'
end

module Letters = struct
  module Case = struct
    let upper = between 'A' 'Z'
    let lower = between 'a' 'z'
  end
end

let letter = Letters.Case.(upper </> lower)
