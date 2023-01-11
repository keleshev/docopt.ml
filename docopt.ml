


let hello = "world"

let (let*) = Result.bind

module Env = struct 
  include Map.Make (String)

  let to_list m = to_seq m |> List.of_seq
end

module Doc = struct
  type option =
    | Long of string
    | Short of char

  type t =
    | Command of string
    | Argument of string
  (*| Option of option * string option*)
    (*| Atom of atom*)
    | Sequence of t * t
    | Optional of t
  (*uEither of t * t*)
    | One_or_more of t

  type 't t_open = [
    | `Command of string
    | `Argument of string
    | `Sequence of 't * 't
    | `Optional of 't
    | `One_or_more of 't
  ]

  type t' = [
    t' t_open
    | `x
  ]

  module Full = struct
    type t = [
      t t_open
      | `Either of t * t
    ]
  end
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

  module Dynamic = struct
    (*type scalar = Unit | String

    type t =
      | Scalar of scalar
      | Option of scalar
      | List of scalar*)

    type collection = Scalar | Option | List
    type base = Unit | String
    type t = base * collection

    let promote (left, _) (right, _) =
      match left, right with
      | Unit, Unit -> Unit, List
      | String, String -> String, List
      | Unit, String 
      | String, Unit -> failwith "incompatible types"
  end
end


let rec infer = function
  | Doc.Argument a -> Env.singleton a Type.Dynamic.(String, Scalar)
  | Doc.Command c -> Env.singleton c Type.Dynamic.(Unit, Scalar)
  | Doc.Sequence (left, right) -> 
      let env1 = infer left in
      let env2 = infer right in
      let merger _key (left: Type.Dynamic.t option) (right: Type.Dynamic.t option): Type.Dynamic.t option =
        match left, right with
        | Some left_t, None -> Some left_t
        | None, Some right_t -> Some right_t
        | None, None -> None
        | Some left_t, Some right_t -> Some (Type.Dynamic.promote left_t right_t)
      in
      Env.merge merger env1 env2
  | Doc.Optional doc ->
      let env = infer doc in
      let mapper: Type.Dynamic.t -> Type.Dynamic.t = function
        | x, Scalar -> x, Option
        | t -> t
      in
      Env.map mapper env
  | Doc.One_or_more doc ->
      let env = infer doc in
      Env.map (function x, _ -> x, Type.Dynamic.List) env


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