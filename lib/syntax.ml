open Format

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

let rec is_valid = function
  | Sentence t -> List.for_all is_valid t
  | NP t ->
      List.exists (function N _ | Pro _ -> true | _ -> false) t
      && List.for_all is_valid t
  | VP t ->
      List.exists (function V _ -> true | _ -> false) t
      && List.for_all is_valid t
  | PP t ->
      List.exists (function P _ -> true | _ -> false) t
      && List.for_all is_valid t
  | _ -> true

let rec pp_raw ppf = function
  | Sentence t | NP t | VP t | PP t ->
      fprintf ppf "@[%a@]" (pp_print_list ~pp_sep:pp_print_space pp_raw) t
  | N s | Pro s | V s | Adj s | Adv s | P s | Aux s | D s | C s ->
      fprintf ppf "%s" s

let rec pp_lbl ppf = function
  | Sentence t ->
      fprintf ppf "@[[S %a]@]" (pp_print_list ~pp_sep:pp_print_space pp_lbl) t
  | NP t ->
      fprintf ppf "@[[NP %a]@]" (pp_print_list ~pp_sep:pp_print_space pp_lbl) t
  | VP t ->
      fprintf ppf "@[[VP %a]@]" (pp_print_list ~pp_sep:pp_print_space pp_lbl) t
  | PP t ->
      fprintf ppf "@[[PP %a]@]" (pp_print_list ~pp_sep:pp_print_space pp_lbl) t
  | N s -> fprintf ppf "[N %s]" s
  | Pro s -> fprintf ppf "[Pro %s]" s
  | V s -> fprintf ppf "[V %s]" s
  | Adj s -> fprintf ppf "[Adj %s]" s
  | Adv s -> fprintf ppf "[Adv %s]" s
  | P s -> fprintf ppf "[P %s]" s
  | Aux s -> fprintf ppf "[Aux %s]" s
  | D s -> fprintf ppf "[D %s]" s
  | C s -> fprintf ppf "[C %s]" s

let rec pp_qtree ppf t =
  let ppv ppf = fprintf ppf "\\textit{%s}" in
  match t with
  | Sentence t ->
      fprintf ppf "@[[.S %a ]@]"
        (pp_print_list ~pp_sep:pp_print_space pp_qtree)
        t
  | NP t ->
      fprintf ppf "@[[.NP %a ]@]"
        (pp_print_list ~pp_sep:pp_print_space pp_qtree)
        t
  | VP t ->
      fprintf ppf "@[[.VP %a ]@]"
        (pp_print_list ~pp_sep:pp_print_space pp_qtree)
        t
  | PP t ->
      fprintf ppf "@[[.PP %a ]@]"
        (pp_print_list ~pp_sep:pp_print_space pp_qtree)
        t
  | N s -> fprintf ppf "[.N %a ]" ppv s
  | Pro s -> fprintf ppf "[.Pro %a ]" ppv s
  | V s -> fprintf ppf "[.V %a ]" ppv s
  | Adj s -> fprintf ppf "[.Adj %a ]" ppv s
  | Adv s -> fprintf ppf "[.Adv %a ]" ppv s
  | P s -> fprintf ppf "[.P %a ]" ppv s
  | Aux s -> fprintf ppf "[.Aux %a ]" ppv s
  | D s -> fprintf ppf "[.D %a ]" ppv s
  | C s -> fprintf ppf "[.C %a ]" ppv s

let raw_str t = pp_raw str_formatter t |> flush_str_formatter
let show_raw t = pp_raw std_formatter t

let lbl_str t =
  pp_print_string str_formatter "\\exi. ";
  pp_lbl str_formatter t |> flush_str_formatter

let show_lbl t =
  pp_print_string std_formatter "\\exi. ";
  pp_lbl std_formatter t

let qtree_str t =
  pp_print_string str_formatter "\\Tree ";
  pp_qtree str_formatter t |> flush_str_formatter

let show_qtree t =
  pp_print_string std_formatter "\\Tree ";
  pp_qtree std_formatter t
