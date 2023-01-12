


let hello = "world"

let (let*) = Result.bind

module Env = struct 
  include Map.Make (String)

  let to_list m = to_seq m |> List.of_seq

  let combine combiner  =
    let merger key left right =
      match left, right with
      | Some one_t, None | None, Some one_t -> Some one_t
      | None, None -> None
      | Some left, Some right -> Some (combiner key left right)
    in
    merge merger

  let merge_result (type e) (callback: string -> _ -> (_, e) result) left right =
    let exception Break of e in
    let merger key left right =
      let result = match left, right with
        | Some one, None -> callback key (`Left one)
        | None, Some one -> callback key (`Right one)
        | Some l, Some r -> callback key (`Both (l, r))
        | None, None -> assert false
      in
      match result with
      | Ok x -> Some x
      | Error e -> raise_notrace (Break e)
    in
    try Ok (merge merger left right)
    with Break e -> Error e

end

module Pattern = struct
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
    | Either of t * t
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

    let optionize: t -> t = function
      | x, Scalar -> x, Option
      | t -> t
  end
end


let rec infer = function
  | Pattern.Argument a -> Env.singleton a Type.Dynamic.(String, Scalar)
  | Pattern.Command c -> Env.singleton c Type.Dynamic.(Unit, Scalar)
  | Pattern.Sequence (left, right) -> 
      let env1 = infer left in
      let env2 = infer right in
      let combiner _key (left, _) (right, _) =
        let open Type.Dynamic in
        match left, right with
        | Unit, Unit -> Unit, List
        | String, String -> String, List
        | Unit, String 
        | String, Unit -> failwith "incompatible types" in
      Env.combine combiner env1 env2
  | Pattern.Either (left, right) ->
      let env1 = infer left in
      let env2 = infer right in
      let merger _key left right =
        let open Type.Dynamic in
        match left, right with
        | Some one_t, None | None, Some one_t -> Some (optionize one_t)
        | Some (String, left_collection), Some (String, right_collection) ->
            Some (String, max left_collection right_collection)
        | Some (Unit, left_collection), Some (Unit, right_collection) ->
            Some (Unit, max left_collection right_collection)
        | Some (Unit, _), Some (String, _)
        | Some (String, _), Some (Unit, _) -> failwith "incompatible types"
        | None, None -> None
      in
      Env.merge merger env1 env2
  | Pattern.Optional doc ->
      let env = infer doc in
      Env.map Type.Dynamic.optionize env
  | Pattern.One_or_more doc ->
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