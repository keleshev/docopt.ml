open Shim

module Source2 = struct
  module Core = struct
    type t = {index: int; string: string}

    let invariant {index; string} =
      index >= 0 && index <= String.length string

    let _empty = {index=0; string=""}

    let next {index; string} =
      match String.get_opt string index with
      | None -> None
      | Some char -> Some (char, {string; index=index + 1})
  end

  include Core
end

module Source = struct
  type t = End | Cons of char * t Lazy.t

  let rec of_stream stream =
    try Cons (Stream.next stream, lazy (of_stream stream))
    with Stream.Failure -> End

  let of_string = Stream.of_string >> of_stream

  let rec to_string = function (* O(n^2) *)
    | End -> ""
    | Cons (char, lazy rest) -> Char.to_string char ^ to_string rest
end

type 'result t = Source.t -> ('result * Source.t) option

let (>>=) parse f source =
  match parse source with
  | Some (result, remaining) -> f result remaining
  | None -> None

let (>=>) parser1 parser2 = parser1 >>= fun _ -> parser2

let (<|>) parser1 parser2 source =
  match parser1 source with
  | Some _ as result -> result
  | None -> parser2 source

let fail : 'a t = fun _ -> None

let return result : 'a t = fun source -> Some (result, source)

let not_followed_by parser source =
  match parser source with
  | Some _ -> None
  | None -> Some ((), source)

let followed_by parser =
  not_followed_by (not_followed_by parser)

let eof = function
  | Source.End -> Some ((), Source.End)
  | _ -> None

let default value parser = parser <|> return value

let any = function
  | Source.Cons (char, lazy source) -> Some (char, source)
  | Source.End -> None

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
  any >>= (fun char -> if predicate char then return char else fail)

let parse parser source =
  match parser source with
  | Some (result, _) -> Some result
  | None -> None

let between left right =
  satisfy (fun char -> left <= char && char <= right)

let char char = satisfy ((=) char)

let separated ~by parser =
  parser >>= fun first ->
  zero_or_more (by >=> parser) >>= fun rest ->
  return (first :: rest)

module Char = struct
  let zero_or_more parser =
    zero_or_more parser >>= fun chars ->
    return (String.of_char_list chars)

  let one_or_more (parser : 'a t) =
    parser >>= fun first ->
    zero_or_more parser >>= fun rest ->
    return (Char.to_string first ^ rest)
end

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

let letter = Letters.Case.(upper <|> lower)
