


let hello = "world"

let (let*) = Result.bind

module Parameter = struct
  type option =
    | Long of string
    | Short of string

  type t =
    | Command of string
    | Argument of string
    
  let compare = compare
end

module Env = struct 
  include Map.Make (Parameter)

  type arity = One | Option | List

  let merge callback left right =
    let merger _key left right =
      match left, right with
        | Some one, None -> Some (callback (`Left one))
        | None, Some one -> Some (callback (`Right one))
        | Some l, Some r -> Some (callback (`Both (l, r)))
        | None, None -> assert false
    in
    merge merger left right
end

module Pattern = struct
  type t =
    | Parameter of Parameter.t
    | Sequence of t * t
    | Optional of t
    | Either of t * t
    | One_or_more of t
end

module Value = struct
  type t = 
    | Unit 
    | Bool of bool 
    | Int of int 
    | String of string 
    | Option of string option
    | List of string list
end

module Type = struct
  type _ t =
    | String: string t
    | Int: int t
    | Bool: bool t
  (*| Unit: unit t*)
  (*| Pair: 'a t * 'b t -> ('a * 'b) t*)
(*  | Bool
    | Enum of ...
    | Option of t
    | List of t
    | Pair of t * t *)

  let int_of_string source =
    match int_of_string_opt source with
    | Some n -> Ok n
    | None -> Error (`Can't_parse_x_expected_type (source, Int))
end


let rec infer = function
  | Pattern.Parameter p ->
      Env.singleton p Env.One
  | Pattern.Sequence (left, right) -> 
      let merger = function
        | `Left t | `Right t -> t
        | `Both _ -> Env.List
      in
      Env.merge merger (infer left) (infer right)
  | Pattern.Either (left, right) ->
      let merger = function
        | `Left Env.One | `Right Env.One -> Env.Option
        | `Left t | `Right t -> t
        | `Both (l, r) -> max l r
      in
      Env.merge merger (infer left) (infer right)
  | Pattern.Optional pattern ->
      infer pattern |> Env.map (function 
        | Env.One -> Env.Option 
        | a -> a
      )
  | Pattern.One_or_more pattern ->
      infer pattern |> Env.map (function _ -> Env.List)
      
(** Match positional-only pattern against positional-only args *)
(*let match_args ~env args =
  | Pattern.Argument a ->
      match args with
      | head :: tail -> *)


type _ t =
  | Term: 'a Type.t * string -> 'a t
  | Map: ('a -> 'b) * 'a t -> 'b t
  | Both: 'a t * 'b t -> ('a * 'b) t

let term term_t atom = Term (term_t, atom)
let map callback term = Map (callback, term)
let both first second = Both (first, second)


let rec eval
  : type a. argv:string list -> a t -> (a * string list, _) result
  = fun ~argv -> function

  | Term (Type.String, a) -> begin
      match argv with
      | head :: tail -> Ok (head, tail)
      | [] -> Error (`Missing_positional_argument a)
    end

  | Term (Type.Int, doc) ->
      let term = Term (Type.String, doc) in
      let* value, tail = eval ~argv (map Type.int_of_string term) in
      let* value in
      Ok (value, tail)

  (*| Term (Type.Pair (left_t, right_t), Sequence (left, right)) ->
      let* left_value, argv = eval ~argv (Term (left_t, left)) in
      let* right_value, argv = eval ~argv (Term (right_t, right)) in
      Ok ((left_value, right_value), argv)*)

  | Map (callback, term) ->
      let* value, tail = eval ~argv term in
      Ok (callback value, tail)

  | Both (left, right) ->
      let* left_value, argv = eval ~argv left in
      let* right_value, argv = eval ~argv right in
      Ok ((left_value, right_value), argv)

  | _ -> failwith "not implemented"      