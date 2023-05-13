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

(** [pp_player player] pretty prints a player's name. *)
let pp_player player = State.get_player_name player

(****************************************************************************
  Testing state.ml functions
  ***************************************************************************)
(*Initializing players and games to be used in testing*)

let empty_game = State.init_state
let player0 = State.init_player "Hog Rider"
let player1 = State.init_player "Musketeer"
let player2 = State.init_player "Goblin"
let player3 = State.init_player "Dragon"

let full_game =
  State.add_player player0 empty_game
  |> State.add_player player1 |> State.add_player player2
  |> State.add_player player3

(** Testing functions that set up the game*)

let initialization_tests =
  let find_player_test name exp_out input =
    name >:: fun _ ->
    assert_equal exp_out
      (State.assign_id full_game 0
      |> State.get_player_list |> State.find_player input |> State.get_id)
  in
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
    ( "Checking that assigning id doesn't change player list" >:: fun _ ->
      assert_equal "[Hog Rider; Musketeer; Goblin; Dragon]"
        (State.assign_id full_game 0
        |> State.get_player_list |> pp_list pp_player) );
    find_player_test "Checking player 0 has correct id after assignment" 0
      "Hog Rider";
    find_player_test "Checking player 1 has correct id after assignment" 1
      "Musketeer";
    find_player_test "Checking player 2 has correct id after assignment" 2
      "Goblin";
    find_player_test "Checking player 3 has correct id after assignment" 3
      "Dragon";
    ( "Checking that find non existent player raises exception" >:: fun _ ->
      assert_raises State.NoPlayer (fun () ->
          State.assign_id full_game 0
          |> State.get_player_list |> State.find_player "Bruh") );
    ( "Checking that adding players doesn't change initial turn" >:: fun _ ->
      assert_equal 0
        (State.get_current_player_state (State.assign_id full_game 0)) );
  ]

(** Testing functions that move the game along*)
let game_tests =
  [
    ( "Checking next turn functionalities, 1 step" >:: fun _ ->
      assert_equal 1
        (State.get_current_player_state (State.next_turn 4 full_game)) );
    ( "Checking next turn functionalities, 2 step" >:: fun _ ->
      assert_equal 2
        (State.get_current_player_state
           (State.next_turn 4 full_game |> State.next_turn 4)) );
    ( "Checking next turn functionalities, 3 step" >:: fun _ ->
      assert_equal 3
        (State.get_current_player_state
           (State.next_turn 4 full_game
           |> State.next_turn 4 |> State.next_turn 4)) );
    ( "Checking next turn functionalities, 4 step, back to the starting player"
    >:: fun _ ->
      assert_equal 0
        (State.get_current_player_state
           (State.next_turn 4 full_game
           |> State.next_turn 4 |> State.next_turn 4 |> State.next_turn 4)) );
    ( "Checking next turn functionalities, 5 step" >:: fun _ ->
      assert_equal 1
        (State.get_current_player_state
           (State.next_turn 4 full_game
           |> State.next_turn 4 |> State.next_turn 4 |> State.next_turn 4
           |> State.next_turn 4)) );
  ]

(****************************************************************************
  Running the full test suite
  ***************************************************************************)
let suite =
  "test suite for A2" >::: List.flatten [ initialization_tests; game_tests ]

let _ = run_test_tt_main suite
