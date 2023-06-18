let sprintf = Printf.sprintf

let failwithf x = Printf.ksprintf failwith x
let debug = false
let printfn x = Printf.ksprintf (if debug then print_endline else (fun _ -> ())) x
let (let*) = Result.bind

module Chain = struct
  (* List with O(1) concatenation, but O(length) head *)
 
  type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t
  (* invariant: for all Concat (x, y). x <> Empty && y <> Empty *)

  let empty = Empty
  let singleton x = Singleton x

  let concat a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | x, y -> Concat (x, y)

  let rec concat_map f = function
    | Singleton s -> f s
    | Concat (x, y) -> concat (concat_map f x) (concat_map f y)
    | other -> other

  let rec find predicate = function
    | Singleton s when predicate s -> Some s
    | Empty | Singleton _ -> None
    | Concat (left, right) ->
        match find predicate left with
        | Some _ as result -> result
        | None -> find predicate right
end

module Levenshtein = struct
  (** Edit distance to give spelling hints
      https://www.baeldung.com/cs/levenshtein-distance-computation
  
      Time: O(min(a, b) * min(a, b, bound))
      Space: O(min(a, b))
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

module Map = struct
  include Map.Make(String)

  let merge ?(one=fun x -> x) ~both left right =
    let merger _key left right =
      match left, right with
      | Some x, None | None, Some x -> Some (one x)
      | Some l, Some r -> Some (both l r)
      | None, None -> assert false
    in
    merge merger left right

  let keys t = fold (fun key _value list -> List.cons key list) t []

  (** Change value for key, fail if key not already present *)
  let replace_exn key ~f map =
    map |> update key (function
      | Some x -> Some (f x) 
      | None -> failwithf "invalid key %S" key)
end

module Atom = struct
  type option =
    | Long of string
    | Short of string

  type t =
    | Command of string
    | Argument of string

  let parse source = (* TODO: this is a temp stub *)
    if String.starts_with ~prefix:"<" source then Argument source else Command source
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

module Match = struct
  type t =
    | Captured of string * string
    | Matched of string
end

module Pattern = struct
  type t =
    | Discrete of Atom.t (* <x>       *)
    | Sequence of t * t  (* <x> <y>   *)
    | Junction of t * t  (* <x> | <y> *)
    | Optional of t      (* [<x>]     *)
    | Multiple of t      (* <x>...    *)


  module NFA = struct
    type state = {id: int; transitions: transition array}
      (* {id; transitions=[|Consume (a, s)|]} is (id)--a-->(s) *)

    and transition =
      | Consume of Atom.t * state  (* Consume (a, s) is •-a->(s) *)
      | Epsilon of state           (* Epsilon s    is •-ε->(s) *)

    type t = state

    let invariant = function
      | {id=0; transitions=[||]} -> true (* final state *)
      | {id=_; transitions=[||]} | {id=0; transitions=_} -> false
      | _ -> true

    let create =
      let counter = ref 0 in
      fun transitions -> (incr counter; {id= !counter; transitions})

    let final = {id=0; transitions=[||]}
    let is_final {id; _} = (id = 0)

    module Set = struct 
      module H = Hashtbl.Make (struct
        type nonrec t = t
        let equal {id=a; _} {id=b; _} = (a = b)
        let hash {id; _} = Hashtbl.hash id
      end)

      type t = unit H.t
      
      let create () = H.create 64
      let add t key = H.add t key ()
      let mem = H.mem
    end

    let rec eval_transition transition log input visited =
      match transition, input with
      | Epsilon state, _ ->
          eval_state state log input visited
      | Consume (Argument a, state), Some arg ->
          printfn "%d: %S => %S" state.id a arg;
          Chain.singleton (state, Match.Captured (a, arg) :: log) 
      | Consume (Command c, state), Some arg when c = arg ->
          printfn "%d: %S" state.id c;
          Chain.singleton (state, Match.Matched c :: log)
      | Consume (Argument _, _), None
      | Consume (Command _, _), _ ->
          Chain.empty

    and eval_state ({transitions; _} as state) log input visited =
      if Set.mem visited state then Chain.empty else begin
        Set.add visited state;
        if is_final state && input = None then
          Chain.singleton (state, log)
        else
          let folder transition chain =
            (* Concat in revers order -- why? *)
            Chain.concat chain (eval_transition transition log input visited)
          in
          Array.fold_right folder transitions Chain.empty
      end

    let eval_chain chain input =
      let visited = Set.create () in
      chain |> Chain.concat_map (fun (state, log) ->
        eval_state state log input visited)

    let rec run chain = function
      | [] -> 
          let chain = eval_chain chain None in
          Chain.find (fun (state, _log) -> is_final state) chain
      | head :: tail ->
          let chain = eval_chain chain (Some head) in
          run chain tail
  end
  open NFA

  let rec to_nfa ?(next=NFA.final) = function
    | Discrete atom -> NFA.create [|Consume (atom, next)|]
    | Sequence (first, second) -> to_nfa first ~next:(to_nfa second ~next)
    | Junction (left, right) ->
        NFA.create [|
          Epsilon (to_nfa left ~next); 
          Epsilon (to_nfa right ~next);
        |]
    | Optional t ->
        NFA.create [|
          Epsilon (to_nfa t ~next); 
          Epsilon next;
        |]
    | Multiple t -> 
       let transitions = [|
          Epsilon NFA.final; (* mutated below *)
          Epsilon next;
       |] in
       let state = to_nfa t ~next:(create transitions) in
       transitions.(0) <- Epsilon state;
       state

  (* <x> [<y>] [<z>] <w> - Report ambiguous grammars? 
     How to report a matching error?
     [-xyz] vs [x y] meaning? 
     Matching performance? *)

  let run pattern ~argv ~defaults =
    let nfa = to_nfa pattern in
    match NFA.run (Chain.singleton (nfa, [])) argv with
    | Some (_, log) ->
        Ok (List.fold_left (fun map -> function
          | Match.Captured (atom, value) ->
              printfn "Captured (%S, %S)" atom value;
              Map.replace_exn atom map ~f:Value.(function
                | String _ -> String value
                | Option _  -> Option (Some value)
                | List tail -> List (value :: tail)
                | _ -> failwithf "bug: captured toggle type for %S" value)
          | Match.Matched atom ->
              printfn "Matched %S" atom;
              Map.replace_exn atom map ~f:Value.(function
                | Unit -> Unit
                | Bool _ -> Bool true
                | Int n -> Int (n + 1)
                | _ -> failwithf "bug: matched captured type for %S" atom))
          defaults log)
    | _ -> printfn " e"; Error [`Match_not_found]
end

module Defaults = struct
  open Pattern open Value

  let promote = function
    | String _ | Option _ | List _ -> List []
    | Unit | Bool _ | Int _ -> Int 0

  let optionalize = function
    | String _ -> Option None
    | Unit -> Bool false
    | other -> other

  let rec infer = function
    | Discrete (Argument a) -> Map.singleton a (String "")
    | Discrete (Command c) -> Map.singleton c Unit
    | Optional pattern -> Map.map optionalize (infer pattern)
    | Multiple pattern -> Map.map promote (infer pattern)
    | Sequence (left, right) -> 
        Map.merge (infer left) (infer right) ~both:(fun _ -> promote)
    | Junction (left, right) ->
        Map.merge (infer left) (infer right) ~both:max ~one:optionalize
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
    | Nativeint
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

    let rec to_string = function
      | Unit -> "unit"
      | Bool -> "bool"
      | Int -> "int"
      | String -> "string"
      | Option t -> sprintf "(option %s)" (to_string t)
      | List t -> sprintf "(list %s)" (to_string t)

    (** Do we support parsing a value of this type from an argv parameter? *)
    let is_parsable = function
      | Int     
      | String -> true
      | Unit    
      | Bool    
      | Option _
      | List _ -> false

    let is_compatible value dynamic_type =
      match value, dynamic_type with
      | Value.String _, t
      | Value.Option _, Option t
      | Value.List _, List t when is_parsable t -> true
      | Value.Unit, Unit
      | Value.Bool _, Bool
      | Value.Int _, Int -> true
      | _ -> false

    module Set = Set.Make (struct type nonrec t = t let compare = compare end)
  end

  (* Cast value to type. Ignore type errors, those are eliminated in
     [type_check]. Parse errors are still possible. *)
  let cast: type a. a t -> Value.t -> a = fun t value ->
    match t, value with
    | Unit, Value.Unit -> ()
    | Bool, Value.Bool bool -> bool
    | Int, Value.Int int -> int
    | Int, Value.String _ -> failwithf "TODO"
    | String, Value.String s -> s
    | (Option String), Value.Option o -> o
    | (Option _), Value.Option _ -> failwithf "TODO"
    | (List String), Value.List l -> l
    | (List _), Value.List _ -> failwithf "TODO"
    | _ -> failwithf "bug: this state should have been eliminated by type_check"
  
  let rec to_dynamic: type a. a t -> Dynamic.t = function
    | Unit     -> Dynamic.Unit
    | Bool     -> Dynamic.Bool
    | Int      -> Dynamic.Int
    | String   -> Dynamic.String
    | Option t -> Dynamic.Option (to_dynamic t)
    | List t   -> Dynamic.List (to_dynamic t)
end

module Set = Type.Dynamic.Set

let find key map = 
  match Map.find_opt key map with
  | None -> failwithf "bug: missing key %S after type check" key
  | Some value -> value

module Term = struct
  type _ t =
    | Get: 'a Type.t * string -> 'a t
    | Map: ('a -> 'b) * 'a t -> 'b t
    | Both: 'a t * 'b t -> ('a * 'b) t
  
  (** Infer environment of all type annotations *)
  let rec infer: type a. a t -> Type.Dynamic.Set.t Map.t = function
    | Get (t, atom) -> Map.singleton atom (Set.singleton (Type.to_dynamic t))
    | Map (_callback, term) -> infer term
    | Both (left, right) -> Map.merge ~both:Set.union (infer left) (infer right)

  (** Evaluate term by looking up values in the map *)
  let rec eval: type a. env:(Value.t Map.t) -> a t -> a = fun ~env -> function
    | Get (t, atom) -> Type.cast t (find atom env)
    | Map (callback, term) -> callback (eval ~env term)
    | Both (left, right) -> eval ~env left, eval ~env right
end

let type_check type_env value_env =
  let errors = Map.fold (fun atom types errors ->
    Set.fold (fun dynamic_type errors ->
       match Map.find_opt atom value_env with
       | None ->
           let error = atom, Type.Dynamic.to_string dynamic_type, Map.keys value_env in
           `Atom_not_found error :: errors
       | Some default ->
           if Type.Dynamic.is_compatible default dynamic_type then
             errors
           else
             let error = atom, Type.Dynamic.to_string dynamic_type, default in
             `Type_error error :: errors
    ) types errors
  ) type_env [] in
  if errors = [] then Ok () else Error errors

let run: type a. argv:string list -> doc:Pattern.t -> a Term.t -> (a, _) result =
  fun ~argv ~doc term ->
    let type_env = Term.infer term in
    let defaults = Defaults.infer doc in
    let* () = type_check type_env defaults in
    let* env = Pattern.run doc ~argv ~defaults in
    Ok (Term.eval term ~env)

(* Exports *)

let get type_annotation atom = Term.Get (type_annotation, atom)
let map callback term = Term.Map (callback, term)
let both first second = Term.Both (first, second)
let (let+) term callback = map callback term
let (and+) = both
let unit = Type.Unit
let bool = Type.Bool
let int = Type.Int
let string = Type.String
let option t = Type.Option t
let list t = Type.List t
