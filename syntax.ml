module Abstract = struct
  module Atom = struct
    module Option = struct
      type t =
        | Long of string * string option
        | Short of string
    end

    type t =
      | Option of Option.t
      | Command of string
      | Argument of string
  end

  type t =
    | Atom of Atom.t
    | One_or_more of t
    | Optional of t list
    | Sequence of t list
    | Alternative of t list
end
