


let hello = "world"

let (let*) = Result.bind

module Term = struct
  type option =
    | Long of string
    | Short of char

  type t =
    | Command of string
    | Argument of string
    | Sequence of t * t
  (*| Option of option * string option
    | Optional of t
    | Either of t * t
    | One_or_more of t *)
end

module Type = struct
  type _ t =
    | String: string t
    | Int: int t
    | Unit: unit t
    | Pair: 'a t * 'b t -> ('a * 'b) t
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

type _ t =
  | Term: 'a Type.t * Term.t -> 'a t
  | Map: ('a -> 'b) * 'a t -> 'b t

let term term_t term = Term (term_t, term)
let map term callback = Map (term, callback)

(*let rec eval (type a) ~(argv: string list) (term: a t): (a, _) result =*)

let rec eval
  : type a. argv:string list -> a t -> (a, _) result
  = fun ~argv -> function

  | Term (Type.String, Term.Argument a) -> begin
      match argv with
      | head :: _ -> Ok head
      | [] -> Error (`Missing_positional_argument a)
    end

  | Term (Type.Int, (Argument _ as term)) ->
      let term = Term (Type.String, term) in
      let* result = eval ~argv (map Type.int_of_string term) in
      result
(*
  | Term (Type.Pair (left_t, right_t), Sequence (left, right)) ->
      let* result = eval ~argv (Term (left_t, left)) in
  *)    

  | Map (callback, term) ->
      let* result = eval ~argv term in
      Ok (callback result)
      
  | _ -> failwith "not implemented"
