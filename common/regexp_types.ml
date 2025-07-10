exception PError of Location.t * string

let mkloc = Location.mkloc
let ( % ) f g x = f (g x)

type 'a t = 'a node Location.loc

and 'a node =
  | Code of 'a
  | Seq of 'a t list
  | Alt of 'a t list
  | Opt of 'a t
  | Repeat of (int * int option) Location.loc * 'a t
  | Nongreedy of 'a t
  | Capture of 'a t
  | Capture_as of string Location.loc * conv_ty option * 'a t
  | Named_subs of string Location.loc * string Location.loc option * conv_ty option * 'a t
  | Unnamed_subs of string Location.loc * 'a t
  | Call of Longident.t Location.loc
(* TODO: | Case_sense of t | Case_blind of t *)

and conv_ty =
  | Int : conv_ty
  | Float : conv_ty
