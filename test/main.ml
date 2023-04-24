open OUnit2
open Game

(****************************************************************************
  Helper functions to pretty print various data structures needed in testing.
  ***************************************************************************)

(** [pp_string s] pretty-prints string [s]. Function *)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. *)
let pp_list pp_elt lst =
  let pp_elts lst =
    let rec loop n acc = function
      | [] -> acc
      | [ h ] -> acc ^ pp_elt h
      | h1 :: (h2 :: t as t') ->
          if n = 100 then acc ^ "..." (* stop printing long list *)
          else loop (n + 1) (acc ^ pp_elt h1 ^ "; ") t'
    in
    loop 0 "" lst
  in
  "[" ^ pp_elts lst ^ "]"

(****************************************************************************
  Testing state.ml functions
  ***************************************************************************)
(*Initializing players and games to be used in testing*)
let empty_game = State.init_state
let player0 = State.init_player "Hog Rider"
let player1 = State.init_player "Musketeer"
let player2 = State.init_player "Goblin"
let player3 = State.init_player "Dragon"
let state_tests = []

(****************************************************************************
  Running the full test suite
  ***************************************************************************)
let suite = "test suite for A2" >::: List.flatten [ state_tests ]
let _ = run_test_tt_main suite
