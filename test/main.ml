open OUnit2
open Game

(****************************************************************************
  Helper functions to pretty print various data structures needed in testing.
  ***************************************************************************)

(** [pp_string s] pretty-prints string [s]. Source: A2 Test Suite*)
let pp_string s = "\"" ^ s ^ "\""

(** [pp_list pp_elt lst] pretty-prints list [lst], using [pp_elt] to
    pretty-print each element of [lst]. Source: A2 Test Suite*)
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

let state_tests =
  [
    ( "Adding player 0 into game." >:: fun _ ->
      assert_equal [ player0 ]
        (State.add_player player0 empty_game |> State.get_player_list) );
    ( "Adding player 0 and player 1 into game." >:: fun _ ->
      assert_equal [ player0; player1 ]
        (State.add_player player0 empty_game
        |> State.add_player player1 |> State.get_player_list) );
    ( "Adding player 1 and player 0 into game." >:: fun _ ->
      assert_equal [ player1; player0 ]
        (State.add_player player1 empty_game
        |> State.add_player player0 |> State.get_player_list) )
    (* ( "Check if player0 has the card 1." >:: fun _ -> assert_equal true
       (State.add_player player1 empty_game |> State.add_player player0 |>
       State.add_player player2 |> State.get_player_list |>
       State.initialize_players_hands State.shuffle |> State.check_person
       player0 1) ); *);
  ]

(** still need to test assign_id, next_turn*)

(****************************************************************************
  Running the full test suite
  ***************************************************************************)
let suite = "test suite for A2" >::: List.flatten [ state_tests ]
let _ = run_test_tt_main suite
