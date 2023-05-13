open OUnit2
open Game

(****************************************************************************
  Helper functions to pretty print and test various data structures needed in
  testing.
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

(** [test_deck lst] checks that an initialized deck lst has 52 cards that follow
    the standard deck breakdown*)
let test_deck deck =
  let is_full lst = List.length lst = 52 in
  let rec is_standard lst prev cnt =
    match List.sort compare lst with
    | [] -> if cnt = 4 then true else false
    | h :: t ->
        if cnt = 4 then is_standard t h 1
        else if h = prev then is_standard t prev (cnt + 1)
        else false
  in
  is_full deck && is_standard deck 1 0

(****************************************************************************
  Testing state.ml functions
  ***************************************************************************)
(*Initializing players and games to be used in testing*)
let empty_game = State.init_state
let player0 = State.init_player "Hog Rider"
let player1 = State.init_player "Musketeer"
let player2 = State.init_player "Goblin"
let player3 = State.init_player "Dragon"

(** Testing functions that set up the game*)
let initialization_tests =
  [
    ( "Checking that initialize game has 0 players" >:: fun _ ->
      assert_equal [] (State.get_player_list empty_game) );
    ( "Checking that initialize game has 52 unique cards in the deck"
    >:: fun _ -> assert_equal true (test_deck (State.get_deck empty_game)) );
    ( "Checking that current player is no one (aka 0)" >:: fun _ ->
      assert_equal 0 (State.get_current_player_state empty_game) );
    ( "Adding player 0 into game." >:: fun _ ->
      assert_equal [ player0 ]
        (State.add_player player0 empty_game |> State.get_player_list) );
    ( "Adding player 0 and player 1 into game." >:: fun _ ->
      assert_equal [ player0; player1 ]
        (State.add_player player0 empty_game
        |> State.add_player player1 |> State.get_player_list) );
    ( "Adding all four players into the game." >:: fun _ ->
      assert_equal
        [ player0; player1; player2; player3 ]
        (State.add_player player0 empty_game
        |> State.add_player player1 |> State.add_player player2
        |> State.add_player player3 |> State.get_player_list) );
    ( "Checking that player 0 is initialized correctly" >:: fun _ ->
      assert_equal "Hog Rider" (State.get_player_name player0) );
    ( "Checking that player 1 is initialized correctly" >:: fun _ ->
      assert_equal "Musketeer" (State.get_player_name player1) );
    ( "Checking that player 2 is initialized correctly" >:: fun _ ->
      assert_equal "Goblin" (State.get_player_name player2) );
    ( "Checking that player 3 is initialized correctly" >:: fun _ ->
      assert_equal "Dragon" (State.get_player_name player3) );
  ]

(** still need to test assign_id, next_turn*)

(****************************************************************************
  Running the full test suite
  ***************************************************************************)
let suite = "test suite for A2" >::: List.flatten [ initialization_tests ]
let _ = run_test_tt_main suite
