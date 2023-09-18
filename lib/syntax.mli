type t =
  | Sentence of t list
  | NP of t list
  | VP of t list
  | PP of t list
  | N of string
  | Pro of string
  | V of string
  | Adj of string
  | Adv of string
  | P of string
  | Aux of string
  | D of string
  | C of string

val is_valid : t -> bool
val raw_str : t -> string
val lbl_str : t -> string
val qtree_str : t -> string

val show_raw : t -> unit
val show_lbl : t -> unit
val show_qtree : t -> unit
