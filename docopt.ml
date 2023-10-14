let sprintf = Printf.sprintf
let failwithf x = Printf.ksprintf failwith x
let debug = false
let printfn x = Printf.ksprintf (if debug then print_endline else (fun _ -> ())) x

let (let*) = Result.bind

module Result = struct
  include Result

  let to_either = function Ok ok -> Either.Left ok | Error e -> Either.Right e

  let list_partition results =
    match List.partition_map to_either results with
    | value, [] -> Ok value
    | _, errors -> Error errors
end

module List = struct
  include List

  let fold ~cons ~nil list = fold_right cons list nil
end

module String = struct
  include String

  let slice_on_char char string =
    match index_opt string char with
    | None -> string, None
    | Some i ->
        sub string 0 i, Some (sub string (i + 1) (length string - i - 1))
end

module Map = struct
  (* Persistent immutable polymorphic map based on AA trees. *)
  type ('key, 'value) t =
    | Empty
    | Node of {
        level: int;
        key: 'key;
        value: 'value;
        left: ('key, 'value) t;
        right: ('key, 'value) t;
      }

  let skew = function
    | Node ({left=Node left; _} as t) when t.level = left.level ->
        Node {left with right=Node {t with left=left.right}}
    | other -> other

  let split = function
    | Node ({right=Node ({right=Node right_right; _} as right); _} as t)
      when right_right.level = t.level ->
        Node {right with left=Node {t with right=right.left};
                         level=right.level + 1}
    | other -> other

  let rec find key = function
    | Empty -> None
    | Node t ->
        match compare key t.key with
        | -1 -> find key t.left
        | +1 -> find key t.right
        | _  -> Some t.value

  let rec update ~key ~value ~choice = function
    | Empty -> Node {key; value; level=1; left=Empty; right=Empty}
    | Node t ->
        match compare key t.key with
        | -1 -> Node {t with left=update ~key ~value ~choice t.left} |> skew |> split
        | +1 -> Node {t with right=update ~key ~value ~choice t.right} |> skew |> split
        | _  -> Node {t with value=choice value t.value}

  let empty = Empty

  let singleton key value =
    Node {key; value; level=1; left=Empty; right=Empty}

  let rec mapi f = function
    | Node ({level=_; key; value; left; right} as t) ->
        Node {t with value=f key value; left=mapi f left; right=mapi f right}
    | Empty -> Empty

  let map f t = mapi (fun _ value -> f value) t

  let rec fold ~cons ~nil = function
    | Empty -> nil
    | Node {left; key; value; right; _} ->
        fold left ~cons ~nil:(cons key value (fold ~cons ~nil right))

  (* [union_biased a b] inserts every element from a into b
     Time: O(a * log(a + b)). *)
  let union_biased ~choice smaller bigger =
    fold smaller ~nil:bigger ~cons:(fun key value -> update ~key ~value ~choice)

  (* [union a b] set union
     Time: O(min(a, b) * log(a + b)) *)
  let union ~choice left right =
    match left, right with
    | Empty, t | t, Empty -> t
    | Node n, Node m when n.level >= m.level -> union_biased ~choice left right
    | _ -> union_biased ~choice:(fun x y -> choice y x) right left

  type 'a one_both = One of 'a | Both of 'a * 'a

  (* O(min(a, b) * log(a + b)) *)
  let merge ~one ~both left right =
    let left = map (fun x -> One x) left in
    let right = map (fun x -> One x) right in
    let choice left right =
      match left, right with
      | One l, One r -> Both (l, r)
      | _ -> assert false
    in
    let union = union left right ~choice in
    map (function One x -> one x | Both (x, y) -> both x y) union

  let add ~key ~value t = update ~key ~value ~choice:(fun x _ -> x) t

  let to_list t = fold ~cons:(fun k v tail -> (k, v) :: tail) ~nil:[] t
  let of_list list =
    let cons (key, value) tail = add ~key ~value tail in
    List.fold ~nil:empty ~cons list

  let keys t = fold ~cons:(fun key _ tail -> key :: tail) ~nil:[] t
  let values t = fold ~cons:(fun _ value tail -> value :: tail) ~nil:[] t
end

module Set = struct
  type 'a t = ('a, unit) Map.t

  let empty = Map.empty
  let singleton a = Map.singleton a ()
  let add ~key t = Map.add ~key ~value:() t
  let fold ~cons ~nil = Map.fold ~cons:(fun key _ -> cons key) ~nil
  let to_list = Map.keys
  let union t = Map.union ~choice:(fun _ _ -> ()) t
end

module MultiMap = struct
  type ('key, 'value) t = ('key, 'value list) Map.t
  let empty = Map.empty

  let add ~key ~value map =
    Map.update ~key ~value:[value] ~choice:List.append map

  let remove ~key map =
    match Map.find key map with
    | Some (head :: tail) -> Some (head, Map.add ~key ~value:tail map)
    | Some [] | None -> None
end

module MultiSet = struct
  type 'a t = ('a, int) Map.t

  let empty = Map.empty

  let add ~key map =
    let count = match Map.find key map with Some x -> x | _ -> 0 in
    Map.add map ~key ~value:(count + 1)

  let remove ~key map =
    match Map.find key map with
    | Some count when count > 0 -> Some (Map.add ~key ~value:(count - 1) map)
    | Some _ | None -> None
end

module MutableSet = struct
  type 'a t = ('a, unit) Hashtbl.t
  let create (): 'a t = Hashtbl.create 64
  let add t ~key = Hashtbl.add t key ()
  let mem = Hashtbl.mem
end

module Chain = struct
  (* List with O(1) concatenation, but O(length) head *)
  type 'a t = Empty | Singleton of 'a | Concat of 'a t * 'a t

  let _invariant = function Concat (Empty, Empty) -> false | _ -> true
  let is_empty = function Empty -> true | _ -> false
  let empty = Empty
  let singleton x = Singleton x
  let pair a b = Concat (Singleton a, Singleton b)

  let concat a b =
    match a, b with
    | Empty, x | x, Empty -> x
    | x, y -> Concat (x, y)

  let rec concat_map f = function
    | Singleton s -> f s
    | Concat (x, y) ->
        let left = concat_map f x
        and right = concat_map f y in
        concat left right
    | Empty as other -> other

  let rec find_map callback = function
    | Empty -> None
    | Singleton s -> callback s
    | Concat (left, right) ->
        match find_map callback left with
        | None -> find_map callback right
        | result -> result
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

module JSON = struct
  type t =
    | Null
    | Boolean of bool
    | Number of float
    | String of string
    | Array of t list
    | Object of (string * t) list

  let fprintf = Format.fprintf
  let sprintf = Format.asprintf

  let pp_list ~sep pp =
    Format.pp_print_list ~pp_sep:(fun ppf () -> fprintf ppf sep) pp

  (** Round-trippable floats printing *)
  let number_to_string n =
    let s = sprintf "%.15g" n in
    if Float.of_string s = n then
      s
    else
      sprintf "%.17g" n

  let pp_string_body ppf =
    String.iter (function
      | '"'    -> fprintf ppf {|\"|}
      | '\\'   -> fprintf ppf {|\\|}
      | '\b'   -> fprintf ppf {|\b|}
      | '\x0C' -> fprintf ppf {|\f|}
      | '\n'   -> fprintf ppf {|\n|}
      | '\r'   -> fprintf ppf {|\r|}
      | '\t'   -> fprintf ppf {|\t|}
      | '\x00'..'\x1F' as non_print_char ->
          fprintf ppf {|\u%.4X|} (Char.code non_print_char)
      | char   -> fprintf ppf {|%c|} char
    )

  let box pp ppf value = fprintf ppf "@[<hv>%a@]" pp value

  let rec pp ppf = function
    | Null      -> fprintf ppf "null"
    | Boolean b -> fprintf ppf "%b" b
    | Number n  -> fprintf ppf "%s" (number_to_string n)
    | String s  -> fprintf ppf {|"%a"|} pp_string_body s
    | Array a   -> fprintf ppf
       "[@;<0 2>%a@;<0 0>]" (pp_list ~sep:",@;<1 2>" (box pp)) a
    | Object o  -> fprintf ppf
       "{@;<0 2>%a@;<0 0>}" (pp_list ~sep:",@;<1 2>" (box pp_pair)) o

  and pp_pair ppf (field, value) =
    fprintf ppf {|"%a": %a|} pp_string_body field pp value

  let to_string = sprintf "%a" (box pp)
end

module Maybe = Stdlib.Option (* Alias to help distinguish options and options *)

(* * *)

module Option = struct
  type t = {canonical: string; synonyms: string list; argument: string Maybe.t}
end

module Argv = struct
  (* We could parse argv directly, but tokenizing makes error handling easier *)
  type token =
    | Option_with_argument of string * string
    | Option_without_argument of string
    | Argument of string

  (* Add argv to errors? Point out error location? *)
  let rec scan ~specs = function
    | [] -> []
    | "--" :: tail -> List.map (fun a -> Ok (Argument a)) tail
    | head :: tail when String.starts_with ~prefix:"--" head -> (
        let option, maybe_argument = String.slice_on_char '=' head in
        match Map.find option specs, maybe_argument with
        | None, _ -> Error (`Unknown_option head) :: scan ~specs tail
        | Some Option.{canonical; argument=Some _; _}, Some argument ->
            Ok (Option_with_argument (canonical, argument)) :: scan ~specs tail
        | Some {canonical; argument=Some argument; _}, None -> (
            match tail with
            | [] ->
                [Error (`Option_requires_argument (option, canonical, argument))]
            | head :: tail when String.starts_with ~prefix:"-" head ->
                let e = option, canonical, argument, head in
                Error (`Option_requires_argument_got e) :: scan ~specs tail
            | head :: tail ->
                Ok (Option_with_argument (canonical, head)) :: scan ~specs tail)
        | Some {canonical; argument=None; _}, Some argument ->
            let e = option, canonical, argument in
            Error (`Unnecessary_option_argument e) :: scan ~specs tail
        | Some {canonical; argument=None; _}, None ->
            Ok (Option_without_argument canonical) :: scan ~specs tail)
    | "-" :: _ -> failwith "TODO: -"
    | head :: tail when String.starts_with ~prefix:"-" head ->
        scan_short_options ~specs 1 head tail
    | head :: tail ->
        Ok (Argument head) :: scan ~specs tail

  and scan_short_options ~specs i head tail =
    if i >= String.length head then
      scan ~specs tail
    else
      let option = sprintf "-%c" String.(get head i) in
      match Map.find option specs with
      | None ->
          let e = `Unknown_option option in
          Error e :: scan_short_options ~specs (i + 1) head tail
      | Some Option.{canonical; argument=None; _} ->
          let result = Option_without_argument canonical in
          Ok result :: scan_short_options ~specs (i + 1) head tail
      | Some Option.{canonical; argument=Some argument; _} ->
          match String.(sub head (i + 1) (length head - i - 1)), tail with
          | "", head :: tail when String.starts_with ~prefix:"-" head ->
              let e = option, canonical, argument, head in
              Error (`Option_requires_argument_got e) :: scan ~specs tail
          | "", head :: tail ->
              Ok (Option_with_argument (canonical, head)) :: scan ~specs tail
          | "", [] ->
              [Error (`Option_requires_argument (option, canonical, argument))]
          | argument, tail ->
              Ok (Option_with_argument (canonical, argument)) :: scan ~specs tail

  let tokenize argv ~specs =
    scan argv ~specs |> Result.list_partition

  (** Pre-parsed argument vector *)
  type options = {values: (string, string) MultiMap.t; counts: string MultiSet.t}
  type t = {arguments: string list; options: options}

  let parse argv ~specs =
    let* tokens = tokenize argv ~specs in
    let nil = {arguments=[]; options={values=Map.empty; counts=MultiSet.empty}} in
    let cons token {arguments; options={values; counts}} = match token with
      | Option_with_argument (option, argument) ->
          let values = MultiMap.add ~key:option ~value:argument values in
          {arguments; options={values; counts}}
      | Option_without_argument option ->
          let counts = MultiSet.add ~key:option counts in
          {arguments; options={values; counts}}
      | Argument argument ->
          {arguments=argument :: arguments; options={values; counts}}
    in
    Ok (List.fold ~cons ~nil tokens)
end

module Atom = struct
  type option =
    | Long of string
    | Short of string

  type t =
    | Option of string * string Maybe.t
    | Command of string
    | Argument of string

  let of_string_unchecked source =
    if String.starts_with ~prefix:"<" source then
      Argument source
    else if String.starts_with ~prefix:"--" source then
      let option, argument = String.slice_on_char '=' source in
      Option (option, argument)
    else if String.starts_with ~prefix:"-" source then
      Option (source, None)
    else
      Command source
end

module Value = struct
  type t =
    | Unit
    | Bool of bool
    | Int of int
    | String of string
    | Option of string option
    | List of string list

  let to_json = function
    | Unit -> JSON.Boolean true
    | Bool b -> JSON.Boolean b
    | Int i -> JSON.Number (Float.of_int i)
    | String s -> JSON.String s
    | Option None -> JSON.Null
    | Option (Some s) -> JSON.String s
    | List l -> JSON.Array (List.map (fun s -> JSON.String s) l)
end

module Env = struct
  type t = (string, Value.t) Map.t

  let to_json env =
    let nil = [] in
    let cons key value list = (key, Value.to_json value) :: list in
    JSON.Object (Map.fold ~nil ~cons env)

  let debug t = to_json t |> JSON.to_string |> print_endline
end

module Match = struct
  type t =
    | Capture of string * string (* Positional and optional arguments *)
    | Literal of string (* Commands and options without arguments *)
end

module NFA = struct
  type state = {id: int; mutable transitions: transition Chain.t}
  and transition = Consume of Atom.t * state | Epsilon of state
  (* {id; transitions=Chain.singleton (Consume (a, s))}  is  (id)--a-->(s)
     {id; transitions=Chain.singleton (Epsilon s)}       is  (id)--ε-->(s) *)

  let create =
    let counter = ref 0 in
    fun transitions -> (incr counter; {id= !counter; transitions})

  let final = create Chain.empty
  let is_final = function {transitions; _} -> Chain.is_empty transitions

  let rec step_transition transition log (Argv.{values; counts} as options) ~input ~visited =
    match transition, input with
    | Epsilon state, input ->
        step_state_log (state, log, options) ~input ~visited
    | Consume (Argument a, state), Some arg ->
        printfn "%d: %S => %S" state.id a arg;
        Chain.singleton (state, Match.Capture (a, arg) :: log, options)
    | Consume (Command c, state), Some arg when c = arg ->
        printfn "%d: %S" state.id c;
        Chain.singleton (state, Match.Literal c :: log, options)
    | Consume (Argument _, _), None
    | Consume (Command _, _), _ ->
        Chain.empty
    | Consume (Option (option, None), state), input -> (
        match MultiSet.remove ~key:option counts with
        | Some counts ->
            let options = Argv.{values; counts} in
            let log = Match.Literal option :: log in
            step_state_log (state, log, options) ~input ~visited
        | None -> Chain.empty)
    | Consume (Option (option, Some _), state), input -> (
        match MultiMap.remove ~key:option values with
        | Some (value, values) ->
            let options = Argv.{values; counts} in
            let log = Match.Capture (option, value) :: log in
            step_state_log (state, log, options) ~input ~visited
        | None -> Chain.empty)

  and step_state_log (state, log, options) ~input ~visited =
    if MutableSet.mem visited (state.id, log) then
      Chain.empty (* Key optimisation: avoid visiting states many times *)
    else begin
      MutableSet.add visited ~key:(state.id, log);
      if is_final state && input = None then (* condition depends on options? *)
        Chain.singleton (state, log, options)
      else
        state.transitions |> Chain.concat_map (fun transition ->
          step_transition transition log options ~input ~visited)
    end

  let step_chain chain input =
    let visited = MutableSet.create () in
    let x = Chain.concat_map (step_state_log ~input ~visited) chain in
    printfn "(%d, %d)" (Hashtbl.length visited) (Gc.get_minor_free ());
    x

  let rec run_chain chain = function
    | [] ->
        let chain = step_chain chain None in
        let f (state, log, _options) = if is_final state then Some log else None in
        Chain.find_map f chain
    | head :: tail ->
        let chain = step_chain chain (Some head) in
        run_chain chain tail

  let run nfa ~argv:Argv.{arguments; options} ~defaults =
    match run_chain (Chain.singleton (nfa, [], options)) arguments with
    | None -> printfn " e"; Error [`Match_not_found] (* TODO: better report *)
    | Some log ->
        Ok (List.fold_left (fun map -> function
          | Match.Capture (atom, value) ->
              printfn "Captured (%S, %S)" atom value;
              Map.update map ~key:atom ~value:Value.Unit ~choice:(fun _ -> function
                | String _ -> String value
                | Option _  -> Option (Some value)
                | List tail -> List (value :: tail)
                | _ -> failwithf "bug: captured toggle type for %S" value)
          | Match.Literal atom ->
              printfn "Matched %S" atom;
              Map.update map ~key:atom ~value:Value.Unit ~choice:(fun _ -> function
                | Unit -> Unit
                | Bool _ -> Bool true
                | Int n -> Int (n + 1)
                | _ -> failwithf "bug: matched captured type for %S" atom))
          defaults log)
end

module Pattern = struct
  type t =
    | Discrete of Atom.t (* <x>       *)
    | Sequence of t * t  (* <x> <y>   *)
    | Junction of t * t  (* <x> | <y> *)
    | Optional of t      (* [<x>]     *)
    | Multiple of t      (* <x>...    *)

  (** Extract options from pattern. 
      Error means option takes an argument in on place, but not in another. *)
  let rec to_options = function
    | Discrete Atom.Option (name, argument) -> Map.singleton name (Ok argument)
    | Discrete Atom.(Command _ | Argument _) -> Map.empty
    | Sequence (left, right)
    | Junction (left, right) ->
        let choice one another =
          match one, another with
          | Ok (Some argument), Ok None
          | Ok None, Ok (Some argument) -> Error [argument]
          | Error left, Error right -> Error (left @ right)
          | Error left, Ok (Some right) -> Error (left @ [right])
          | Ok (Some left), Error right -> Error (left :: right)
          | Error e, _ | _, Error e -> Error e
          (* TODO: Is it useful to capture all unique argument names? *)
          | Ok argument, Ok _ -> Ok argument
        in
        Map.union ~choice (to_options left) (to_options right)
    | Optional pattern
    | Multiple pattern ->
        to_options pattern

  module Compiler = struct
    let arrow atom next =
      NFA.create (Chain.singleton (NFA.Consume (atom, next)))

    let fork left right =
      NFA.create (Chain.pair (NFA.Epsilon left) (NFA.Epsilon right))

    let loop callback next =
      let todo = NFA.create Chain.empty in
      let state = callback todo in
      todo.transitions <- (fork state next).transitions;
      state

    let rec run ~next = function
      | Discrete atom -> arrow atom next
      | Sequence (first, second) -> run first ~next:(run second ~next)
      | Junction (left, right) -> fork (run left ~next) (run right ~next)
      | Optional t -> fork (run t ~next) next
      | Multiple t -> loop (fun self -> run t ~next:(fork self next)) next
  end

  let compile = Compiler.run ~next:NFA.final

  (* <x> [<y>] [<z>] <w> - Report ambiguous grammars?
     How to report a matching error?
     [-xyz] vs [x y] meaning?
     Matching performance? *)
end

module Doc = struct
  (* Abstract syntax tree representing a docopt document *)
  type t = {usage: Pattern.t; options: Option.t list}
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
    | Discrete (Argument a | Option (a, Some _)) -> Map.singleton a (String "")
    | Discrete (Command a | Option (a, None)) -> Map.singleton a Unit
    | Optional pattern -> Map.map optionalize (infer pattern)
    | Multiple pattern -> Map.map promote (infer pattern)
    | Sequence (left, right) ->
        Map.union (infer left) (infer right) ~choice:(fun _ -> promote)
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

module Term = struct
  type _ t =
    | Get: 'a Type.t * string -> 'a t
    | Map: ('a -> 'b) * 'a t -> 'b t
    | Tuple: 'a t * 'b t -> ('a * 'b) t

  (** Infer environment of all type annotations *)
  let rec infer: type a. a t -> (string, Type.Dynamic.t Set.t) Map.t = function
    | Get (t, atom) -> Map.singleton atom (Set.singleton (Type.to_dynamic t))
    | Map (_callback, term) -> infer term
    | Tuple (left, right) -> Map.union (infer left) (infer right) ~choice:Set.union

  (** Evaluate term by looking up values in the map *)
  let rec eval: type a. env:((string, Value.t) Map.t) -> a t -> a = fun ~env -> function
    (* TODO: instead of .get, error about using unknown or non-canonical name *)
    | Get (t, atom) -> Type.cast t (Map.find atom env |> Maybe.get)
    | Map (callback, term) -> callback (eval ~env term)
    | Tuple (left, right) -> eval ~env left, eval ~env right
end

let type_check type_env value_env =
  let errors = Map.fold type_env ~nil:[] ~cons:(fun atom types errors ->
    Set.fold types ~nil:errors ~cons:(fun dynamic_type errors ->
      match Map.find atom value_env with
      | None ->
          let error = atom, Type.Dynamic.to_string dynamic_type, Map.keys value_env in
          `Atom_not_found error :: errors
      | Some default ->
          if Type.Dynamic.is_compatible default dynamic_type then
            errors
          else
            let error = atom, Type.Dynamic.to_string dynamic_type, default in
            `Type_error error :: errors)) in
  if errors = [] then Ok () else Error errors

let options_to_map options =
  let nil = Map.empty in
  let cons (option: Option.t) map =
    let nil = Map.add ~key:option.canonical ~value:option map in
    let cons synonym map = Map.add ~key:synonym ~value:option map in
    List.fold ~nil ~cons option.synonyms
  in
  List.fold ~nil ~cons options

let options_only_in_usage_section
    options_in_usage_section
    options_in_options_section =
  let cons option result collected =
    match Map.find option options_in_options_section, result with
    | None, Ok argument ->
       Ok Option.{canonical=option; synonyms=[]; argument} :: collected
    | _, Error arguments ->
        let e = option, arguments in
        Error (`Inconsistent_option_argument_in_usage e) :: collected
    | Some Option.{argument=Some _; _}, Ok (Some _)
    | Some Option.{argument=None; _}, Ok None -> collected
    | Some Option.{argument; _}, Ok argument' ->
        let e = option, argument, argument' in
        Error (`Inconsistent_option_argument_in_usage_vs_options e) :: collected
  in
  Map.fold ~nil:[] ~cons options_in_usage_section |> Result.list_partition

  
let reconcile Doc.{usage; options} =
  let options_in_options_section = options_to_map options in
  let options_in_usage_section = Pattern.to_options usage in
  let* _options_only_in_usage_section =
    options_only_in_usage_section options_in_usage_section options_in_options_section in
  (*let subset = usage_options usage in*)
  (* Replace "[options]" with options from "options:" section, sans those that already in usage. *)
  (* Make sure options in usage and options section are consistent. *)
  (* If no "[options]" shortcut, make sure every option from option section is present in usage. *)
  (* TODO: Make sure options section does not have duplicates, inconsistencies. *)
  let options = options_in_options_section in
  Ok (usage, options)

let run
  : type a. argv:string list -> doc:Doc.t -> a Term.t -> (a, _) result
  = fun ~argv ~doc term ->
    (* Static part *)
    let* pattern, options = reconcile doc in
    let type_env = Term.infer term in
    let defaults = Defaults.infer pattern in
    let* () = type_check type_env defaults in
    let nfa = Pattern.compile pattern in
    (* Dynamic part *)
    let* argv = Argv.parse argv ~specs:options in
    let* env = NFA.run nfa ~argv ~defaults in
    (* Env.debug env; *)
    Ok (Term.eval term ~env)

(* Exports *)

let get type_annotation atom = Term.Get (type_annotation, atom)
let map callback term = Term.Map (callback, term)
let tuple first second = Term.Tuple (first, second)
let (let+) term callback = map callback term
let (and+) = tuple
let unit = Type.Unit
let bool = Type.Bool
let int = Type.Int
let string = Type.String
let option t = Type.Option t
let list t = Type.List t
