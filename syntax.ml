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

module Beta = struct
  module Option = struct
    type t = Short of char | Long of string

    module Spec = struct
      type nonrec t = {
        aliases: t list;
        parameters: [`None | `One | `Many];
      }
    end
  end

end


module Psi = struct
  type t =
    | Boolean of bool
    | String of string
    | Maybe of string option
    | List of string list
    | Count of int
end

module Type = struct
  type t =
    | Always_true (* unit *)
    | Maybe_true (* boolean *)
    | Always_string (* string *)
    | Maybe_string (* string option *)
    | List_of_strings (* string list *)
    | List_of_always_true (* unit list, natural *)

  type scalar = True | String
  type t2 =
    | Scalar of scalar
    | Option of scalar
    | List of scalar
end

module Value = struct
  type t =
    | None
    | Boolean of bool
    | String of string
    | List of string list
    | Number of int

  type scalar = True | String of string
  type t2 =
    | Scalar of scalar
    | Option of scalar option
    | List of scalar list (* ether True list or String list *)
end

module Omega = struct
  type scalar = True | Value of string
  type t =
    | Scalar of scalar
    | Maybe of scalar option
    | List of string list
    | Count of int

  type t2 = [
    | `always_true
    | `maybe_true (* boolean *)
    | `always_string of string
    | `maybe_string of string option
    | `string_list of string list
    | `always_true_list of [`always_true] list (* unit list, nat *)
  ]

  type t3 = [
    | `always_true
    | `maybe_true (* boolean *)
    | `always_string of string
    | `maybe_string of string option
    | `string_list of string list
    | `always_true_list of [`always_true] list (* unit list, nat *)
  ]

  let example = [
    "--foo", Scalar True;
    "--bar", Maybe (Some True);
    "--baz", Maybe None;
    "<foo>", List [];
  ]



end



