open Shim
open Parsing_framework

module Argv = struct
  module Token = struct
    type t =
      | Long_option of string * string option
      | Short_options of string
      | Argument of string
      | Dash
  end

  open Token

  let rec tokenize = function
    | [] -> []
    | "-" :: tail -> Dash :: tokenize tail
    | "--" :: tail -> List.map tail ~f:(fun s -> Argument s)
    | head :: tail when String.is_prefix ~prefix:"--" head ->
        let name, argument = String.partition ~on:'=' head in
        Long_option (name, argument) :: tokenize tail
    | head :: tail when String.is_prefix ~prefix:"-" head ->
        Short_options head :: tokenize tail
    | head :: tail ->
        Argument head :: tokenize tail
end
