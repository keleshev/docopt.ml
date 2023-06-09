let sprintf = Printf.sprintf


module Catlist = struct
  (* List with O(1) concatenation, but O(length) head *)
 
  type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t
  (* invariant: for all Concat (x, y). x <> Empty && y <> Empty *)

  let empty = Empty
  let singleton x = Singleton x

  let concat a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | x, y -> Concat (x, y)

  let cons item = function
    | Empty -> Singleton item
    | other -> Concat (Singleton item, other)

  let rec concat_map f = function
    | Singleton s -> f s
    | Concat (x, y) -> concat (concat_map f x) (concat_map f y)
    | other -> other

  let bind t f = concat_map f t

  let rec find predicate = function
    | Empty -> None
    | Singleton s when predicate s -> Some s
    | Singleton _ -> None
    | Concat (left, right) ->
        match find predicate left with
        | Some _ as result -> result
        | None -> find predicate right
end

module Levenshtein = struct
  (** Edit distance to give spelling hints *)

  (** Bounded Levenshtein distance
  
      As described in https://www.baeldung.com/cs/levenshtein-distance-computation
  
      let m = min(length(a), length(b))
      Time: O(m * min(m, bound))
      Space: O(m)
  *)
  let distance ?(bound=max_int) a b =
    let aux ~bound a b =
      let n = String.length a and m = String.length b in
      assert (n >= m);
      if n - m > bound then None else
      let fill_bound = min m bound in
      let previous = Array.make (m + 1) max_int in
      for j = 0 to fill_bound do previous.(j) <- j done;
      let current = Array.make (m + 1) max_int in
      for i = 0 to n - 1 do
        current.(0) <- i + 1;
        let stripe_start = max 0 (i - fill_bound)
        and stripe_end = min m (i + fill_bound) - 1 in
        if stripe_start > 0 then current.(stripe_start) <- max_int;
        for j = stripe_start to stripe_end do
          current.(j + 1) <-
            if a.[i] = b.[j] then
              previous.(j)
            else
              let deletion = previous.(j + 1)
              and insertion = current.(j)
              and substitution = previous.(j) in
              let smallest = min deletion (min insertion substitution) in
              if smallest = max_int then max_int else smallest + 1;
        done;
        for j = 0 to m do previous.(j) <- current.(j) done;
      done;
      let distance = previous.(m) in
      if distance > bound then None else Some distance
    in
    let n = String.length a and m = String.length b in
    if n > m then aux ~bound a b else aux ~bound b a

end

module MapMake (X: sig type t val compare : t -> t -> int end) = struct
  include Map.Make(X)

  let merge callback left right =
    let merger _key left right =
      match left, right with
      | Some one, None -> Some (callback (`Once one))
      | None, Some one -> Some (callback (`Once one))
      | Some l, Some r -> Some (callback (`Both (l, r)))
      | None, None -> assert false
    in
    merge merger left right

  let union callback left right =
    union (fun _key l r -> Some (callback l r)) left right

  let keys t = fold (fun key _value list -> List.cons key list) t []
end

module Map = MapMake (String)

let hello = "world"

let (let*) = Result.bind

module Atom = struct
  (* Toggle
     Valued 
     Switch
     Capture
     Strand
     String
     Label
     Variable
   *)
  type option =
    | Long of string
    | Short of string

  type t =
    | Command of string
    | Argument of string

  let to_string = function Command s | Argument s -> s
    
  let compare = compare

  let parse source = (* TODO: this is a temp stub *)
    if String.starts_with ~prefix:"<" source then Argument source else Command source

  module Map = MapMake (struct type nonrec t = t let compare = compare end)
end


module Pattern = struct
  type t =
    | Discrete of Atom.t (* <x>       *)
    | Sequence of t * t  (* <x> <y>   *)
    | Junction of t * t  (* <x> | <y> *)
    | Optional of t      (* [<x>]     *)
    | Multiple of t      (* <x>...    *)

  module Match = struct
    module Log = struct
      (** Instead of adding matches to a Map in O(log(n)) in many branches that
          might be discarded, add succesfull matches to a log in O(1), then
          build the Map once. *)

      type entry =
        | MatchedArgument of string * string
        | MatchedCommand of string

      type t = entry list
    end

    (* [<x>] <y>    y *)

    (* (<x> <y> | <z>) <w>    x y z w  *)

    (* <x>... <z>    x z w  *)
 
    (* <x> [<y>] [<z>] <w> - Report ambiguous grammars? 
       How to report a matching error?
       [-xyz] vs [x y] meaning? 
       Matching performance? *)

    let rec prefix pattern argv log =
      let open Atom in let open Log in
      let (let*) = Catlist.bind in
      match pattern, argv with
      | Discrete (Argument a), value :: rest -> 
          Catlist.singleton (MatchedArgument (a, value) :: log, rest)
      | Discrete (Command c), value :: rest when c = value ->
          Catlist.singleton (MatchedCommand c :: log, rest)
      | Discrete (Argument _), _ ->
          Catlist.empty
      | Discrete (Command _), _ ->
          Catlist.empty
      | Sequence (left, right), argv ->
          let* log, argv = prefix left argv log in
          prefix right argv log
      | Junction (left, right), argv ->
          let left = prefix left argv log
          and right = prefix right argv log in
          Catlist.concat left right
      | Optional pattern, argv ->
          Catlist.cons ([], argv) (prefix pattern argv log)
      | Multiple inner, argv ->
          let* log, argv = prefix inner argv log in
          Catlist.cons (log, argv) (prefix pattern argv log)

    let completely pattern argv =
      let results = prefix pattern argv [] in
      Catlist.find (fun (_log, argv) -> argv = []) results
      
    module Test = struct      
      let x, y = Discrete (Argument "<x>"), Discrete (Argument "<y>") in
      let _c, _d = Discrete (Command "c"), Discrete (Command "d") in
      (*assert (perform x ["x"] [] = [["<x>"], []]);
      assert (perform x ["x"] [] = [["<x>"], []]);
      assert (perform (Sequence (x, y)) ["x"; "y"] [] = [["<y>"; "<x>"], []]);
      assert (perform (Junction (x, y)) ["a"] [] = [["<x>"], []; ["<y>"], []]);*)
      assert (completely (Sequence (Multiple x, y)) ["a"; "b"]
              = Some ([MatchedArgument ("<y>", "b");
                       MatchedArgument ("<x>", "a")], []));
    end
  end
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

module Occurence = struct
  type plurality = (* String | Toggle *)
    | Required     (* <a>    | c      *)
    | Optional     (* [<a>]  | [c]    *)
    | Multiple     (* <a>... | c...   *)

  type capture =
    | Toggle (* Flag, command, option without argument *)
    | String (* Argument, option with argument *)

  type t = plurality * capture

  let _invariant = assert (Required < Optional && Optional < Multiple)

  let to_string _ = "TODO"

  let rec infer = function
    | Pattern.Discrete (Argument a) ->
        Map.singleton a (Required, String)
    | Pattern.Discrete (Command c) ->
        Map.singleton c (Required, Toggle)
    | Pattern.Sequence (left, right) -> 
        Map.union (fun _ (_, x) -> Multiple, x) (infer left) (infer right)
    | Pattern.Junction (left, right) ->
        let merger = function
          | `Once (Required, x) -> Optional, x
          | `Once t -> t
          | `Both ((l, _), (r, x)) -> max l r, x in
        Map.merge merger (infer left) (infer right)
    | Pattern.Optional pattern ->
        let mapper = function
          | Required, x -> Optional, x
          | other -> other in
        Map.map mapper (infer pattern)
    | Pattern.Multiple pattern ->
        Map.map (fun (_, x) -> Multiple, x) (infer pattern)
  
  let default_value = function
    | Required, String -> Value.String ""
    | Optional, String -> Value.Option None
    | Multiple, String -> Value.List []
    | Required, Toggle -> Value.Unit
    | Optional, Toggle -> Value.Bool false
    | Multiple, Toggle -> Value.Int 0
end

module Type = struct
  type _ t =
    | Unit: unit t
    | Bool: bool t
    | Int: int t
    | String: string t
    | Option: 'a t -> ('a option) t
    | List: 'a t -> ('a list) t
(*  | In_channel
    | Out_channel
    | Int32
    | Int64
    | Float
    | Char | UChar ?
    | Bytes ?
    | Set ?
    | Array: 'a t -> ('a array) t
    | Seq: 'a t -> ('a Seq.t) t
    | Result: 'a t -> 'e -> (('a, 'e) result) t
        Docopt.(get (result string `Not_found)) "<param>"
    | Enum: (string * 'enum) list -> 'enum t
        Docopt.(get (enum ["hello", `hello; "goodbye", `goodbye])) "<param>"
*)

  let int_of_string source =
    match int_of_string_opt source with
    | Some n -> Ok n
    | None -> Error (`Can't_parse_x_expected_type (source, Int))

  module Dynamic = struct
    type t = 
      | Unit
      | Bool
      | Int
      | String
      | Option of t
      | List of t

    (** Do we support parsing a value of this type from an argv parameter? *)
    let is_parsable = function
      | Int     
      | String -> true
      | Unit    
      | Bool    
      | Option _
      | List _ -> false

    let rec to_string = function
      | Unit -> "unit"
      | Bool -> "bool"
      | Int -> "int"
      | String -> "string"
      | Option t -> sprintf "(option %s)" (to_string t)
      | List t -> sprintf "(list %s)" (to_string t)

    let is_compatible occurence dynamic_type =
      match occurence, dynamic_type with
      | Occurence.(Required, String), t
      | Occurence.(Optional, String), Option t
      | Occurence.(Multiple, String), List t when is_parsable t -> true
      | Occurence.(Required, Toggle), Unit
      | Occurence.(Optional, Toggle), Bool
      | Occurence.(Multiple, Toggle), Int -> true
      | _ -> false

    module Set = Set.Make (struct type nonrec t = t let compare = compare end)
  end

  let rec to_dynamic: type a. a t -> Dynamic.t = function
    | Unit     -> Dynamic.Unit
    | Bool     -> Dynamic.Bool
    | Int      -> Dynamic.Int
    | String   -> Dynamic.String
    | Option t -> Dynamic.Option (to_dynamic t)
    | List t   -> Dynamic.List (to_dynamic t)
end

module Set = Type.Dynamic.Set


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

(** Infer an environment of all type annotations *)
let rec infer
  : type a. a t -> Type.Dynamic.Set.t Map.t
  = function
  | Term (t, string) -> 
      let set = Set.singleton (Type.to_dynamic t) in
      Map.singleton string set
  | Map (_callback, term) -> infer term
  | Both (first, second) -> 
      Map.union Set.union (infer first) (infer second)


let type_check type_env occurence_env =
  let errors = Map.fold (fun atom set errors ->
    Set.fold (fun dynamic_type errors ->
       match Map.find_opt atom occurence_env with
       | None ->
           let error = atom, Type.Dynamic.to_string dynamic_type, Map.keys occurence_env in
           `Atom_not_found error :: errors
       | Some occurence ->
           if Type.Dynamic.is_compatible occurence dynamic_type then
             errors
           else
             let error = atom, Type.Dynamic.to_string dynamic_type, Occurence.to_string occurence in
             `Type_error error :: errors
    ) set errors
  ) type_env [] in
  if errors = [] then Ok () else Error errors

let run
  : type a. argv:string list -> doc:Pattern.t -> a t -> (a, _) result
  = fun ~argv:_argv ~doc term ->

  let type_env = infer term in
  let occurence_env = Occurence.infer doc in
  let* () = type_check type_env occurence_env in
  let _defaults = Map.map Occurence.default_value occurence_env in
  failwith "not implemented"
  (*let env = Pattern.Match.run doc defaults in
  eval term ~env*)
  
(*
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

  | _ -> failwith "not implemented"       *)
