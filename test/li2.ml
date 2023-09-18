open Format
open Lib.Syntax

let e1 =
  Sentence
    [
      NP [ N "Boris" ];
      VP [ V "talked"; PP [ P "about"; NP [ Pro "himself" ] ] ];
    ]

let e2 =
  Sentence
    [
      NP [ N "Boris" ];
      VP
        [
          V "talked";
          PP [ P "to"; NP [ D "the"; N "reporter" ] ];
          PP [ P "about"; NP [ Pro "himself" ] ];
        ];
    ]

let e3 =
  Sentence
    [
      NP [ N "Boris" ];
      VP
        [
          V "sent"; NP [ D "some"; N "poison" ]; PP [ P "to"; NP [ N "Andy" ] ];
        ];
    ]

let e4 = NP [ D "a"; Adj "fat"; N "cat" ]
let e5 = NP [ D "the"; Adj "invisible"; Adj "visible"; N "stars" ]

let validate t =
  show_raw t;
  print_newline ();
  show_lbl t;
  print_newline ();
  print_newline ();
  show_qtree t;
  print_newline ();
  assert (is_valid t)

let () =
  List.iter
    (fun t ->
      validate t;
      print_newline ())
    [ e1; e2; e3; e4; e5 ]
