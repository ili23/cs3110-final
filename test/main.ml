open OUnit2
open Game
(****************************************************************************
  Helper functions to pretty print and test various data structures needed in
  testing.
  ***************************************************************************)

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

(** [print_hand player] pretty prints a player's hands. *)
let print_hand player = State.get_player_hand player

let rec print_all_hands pl_list acc =
  match pl_list with
  | [] -> acc
  | h :: t -> print_all_hands t (print_hand h :: acc)

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

let pseudo_game0 = State.set_deck full_game [ 1; 2; 3; 4 ]
let pseudo_game1 = State.set_deck full_game [ 1 ]
let pseudo_game2 = State.set_deck full_game []
let player0_with_cards = State.add_card (State.repeat_add_card player0 1 2) 3
let player1_with_cards = State.add_card (State.repeat_add_card player1 4 2) 3

let exchange_game =
  let game_with_new_0 =
    State.update_player full_game player0 player0_with_cards
  in
  State.update_player game_with_new_0 player1 player1_with_cards

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

(****************************************************************************
  Setting up decks/players to be used in testing.
  ***************************************************************************)
let deck1 =
  let d = Array.make 52 1 in
  for x = 1 to 13 do
    for y = 0 to 3 do
      Array.set d ((4 * (x - 1)) + y) x
    done
  done;
  Array.to_list d

let deck2 =
  let rec create_deck acc num =
    match num with
    | 0 -> acc
    | x -> create_deck (x :: acc) (x - 1)
  in
  let thirteen = create_deck [] 13 in
  thirteen @ thirteen @ thirteen @ thirteen

let incomplete_deck =
  let rec create_deck acc num =
    match num with
    | 0 -> acc
    | x -> create_deck (x :: acc) (x - 1)
  in
  let deck = create_deck [] 13 in
  deck

let game_with_deck1 = State.set_deck full_game deck1
let game_with_deck2 = State.set_deck full_game deck2
let player0_scored1 = State.set_score player0 1
let player1_scored1 = State.set_score player1 2
let player2_scored1 = State.set_score player2 3
let player3_scored1 = State.set_score player3 4

let score_game_1 =
  let temp = State.update_player full_game player0 player0_scored1 in
  let temp1 = State.update_player temp player1 player1_scored1 in
  let temp2 = State.update_player temp1 player2 player2_scored1 in
  State.update_player temp2 player3 player3_scored1

let player0_scored2 = State.set_score player0 2
let player1_scored2 = State.set_score player1 2
let player2_scored2 = State.set_score player2 4
let player3_scored2 = State.set_score player3 4

let score_game_2 =
  let temp = State.update_player full_game player0 player0_scored2 in
  let temp1 = State.update_player temp player1 player1_scored2 in
  let temp2 = State.update_player temp1 player2 player2_scored2 in
  State.update_player temp2 player3 player3_scored2

let player0_scored3 = State.set_score player0 3
let player1_scored3 = State.set_score player1 4
let player2_scored3 = State.set_score player2 4
let player3_scored3 = State.set_score player3 4

let score_game_3 =
  let temp = State.update_player full_game player0 player0_scored3 in
  let temp1 = State.update_player temp player1 player1_scored3 in
  let temp2 = State.update_player temp1 player2 player2_scored3 in
  State.update_player temp2 player3 player3_scored3

(****************************************************************************
  Testing state.ml functions continued
  ***************************************************************************)
let initialize_hand_test =
  [
    ( "Initialize game1" >:: fun _ ->
      assert_equal
        [
          [ 4; 5; 5; 5; 5 ];
          [ 3; 3; 4; 4; 4 ];
          [ 2; 2; 2; 3; 3 ];
          [ 1; 1; 1; 1; 2 ];
        ]
        (print_all_hands
           (State.initialize_players_hands deck1
              (State.get_player_list game_with_deck1))
           []) );
    ( "Initialize game2" >:: fun _ ->
      assert_equal
        [
          [ 3; 4; 5; 6; 7 ];
          [ 1; 2; 11; 12; 13 ];
          [ 6; 7; 8; 9; 10 ];
          [ 1; 2; 3; 4; 5 ];
        ]
        (print_all_hands
           (State.initialize_players_hands deck2
              (State.get_player_list game_with_deck2))
           []) );
    ( "Raises exception on incomplete deck" >:: fun _ ->
      assert_raises State.Temporary (fun () ->
          State.initialize_players_hands incomplete_deck
            (State.get_player_list game_with_deck2)) );
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
    ( "Checking that current player with id is matched to correct player"
    >:: fun _ ->
      assert_equal "Hog Rider"
        (pp_player (State.assign_id full_game 0 |> State.get_current_player)) );
    ( "Current player with id is matched to correct player after next turn"
    >:: fun _ ->
      assert_equal "Musketeer"
        (pp_player
           (State.assign_id full_game 0
           |> State.next_turn 4 |> State.get_current_player)) );
    ( "Get winner when all players have same scores" >:: fun _ ->
      assert_equal
        [ "Dragon"; "Goblin"; "Musketeer"; "Hog Rider" ]
        (State.check_winner full_game) );
    ( "Get winner one winner" >:: fun _ ->
      assert_equal [ "Dragon" ] (State.check_winner score_game_1) );
    ( "Get winner two winners" >:: fun _ ->
      assert_equal [ "Dragon"; "Goblin" ] (State.check_winner score_game_2) );
    ( "Get winner three winners" >:: fun _ ->
      assert_equal
        [ "Dragon"; "Goblin"; "Musketeer" ]
        (State.check_winner score_game_3) );
  ]

let exchange_tests =
  [
    ( "Exchange one card test for sender" >:: fun _ ->
      assert_equal [ 1; 1; 3; 3 ]
        (State.find_player "Hog Rider"
           (State.exchange_cards player0_with_cards player1_with_cards 3
              exchange_game
           |> State.get_player_list)
        |> print_hand) );
    ( "Exchange one card test for receiver" >:: fun _ ->
      assert_equal [ 4; 4 ]
        (State.find_player "Musketeer"
           (State.exchange_cards player0_with_cards player1_with_cards 3
              exchange_game
           |> State.get_player_list)
        |> print_hand) );
    ( "Exchange two card sender" >:: fun _ ->
      assert_equal [ 1; 1; 3; 4; 4 ]
        (State.find_player "Hog Rider"
           (State.exchange_cards player0_with_cards player1_with_cards 4
              exchange_game
           |> State.get_player_list)
        |> print_hand) );
    ( "Exchange two card sender" >:: fun _ ->
      assert_equal [ 3 ]
        (State.find_player "Musketeer"
           (State.exchange_cards player0_with_cards player1_with_cards 4
              exchange_game
           |> State.get_player_list)
        |> print_hand) );
  ]

(****************************************************************************
  Testing functions that don't directly manipulate state but are used to in
  state.ml.
  ***************************************************************************)
let other_functions =
  [
    ( "Check if card is in an empty hand" >:: fun _ ->
      assert_equal false (State.check_hand [] 0) );
    ( "Card is first card in hand" >:: fun _ ->
      assert_equal true (State.check_hand [ 0; 1; 2; 3; 4 ] 0) );
    ( "Card is middle card in hand" >:: fun _ ->
      assert_equal true (State.check_hand [ 1; 2; 0; 1 ] 0) );
    ( "Card is middle card in hand" >:: fun _ ->
      assert_equal true (State.check_hand [ 1; 0; 2; 4; 6 ] 0) );
    ( "Card is last card in hand" >:: fun _ ->
      assert_equal true (State.check_hand [ 0; 1; 2; 3; 4 ] 4) );
    ( "Not in hand" >:: fun _ ->
      assert_equal false (State.check_hand [ 0; 1; 2; 3; 4 ] 5) );
    ( "Check if deck is nonempty with nonempty deck" >:: fun _ ->
      assert_equal true (State.check_deck full_game) );
    ( "Check if deck is nonempty with nonempty deck " >:: fun _ ->
      assert_equal false (State.remove_card_top 52 full_game |> State.check_deck)
    );
    ( "Add one card to player's hand" >:: fun _ ->
      assert_equal [ 1 ] (print_hand (State.add_card player0 1)) );
    ( "Add two card to player's hand in sorted order" >:: fun _ ->
      assert_equal [ 1; 2 ]
        (print_hand (State.add_card (State.add_card player0 1) 2)) );
    ( "Add two card to player's hand not sorted order" >:: fun _ ->
      assert_equal [ 0; 1 ]
        (print_hand (State.add_card (State.add_card player0 1) 0)) );
    ( "Add three card to player's hand not sorted order" >:: fun _ ->
      assert_equal [ 0; 1; 3 ]
        (print_hand
           (State.add_card (State.add_card (State.add_card player0 1) 0) 3)) );
    ( "Remove 0 cards from full deck" >:: fun _ ->
      assert_equal 52
        (List.length (State.remove_card_top 0 full_game |> State.get_deck)) );
    ( "Remove 10 cards from full deck" >:: fun _ ->
      assert_equal 42
        (List.length (State.remove_card_top 10 full_game |> State.get_deck)) );
    ( "Remove all cards from full deck" >:: fun _ ->
      assert_equal 0
        (List.length (State.remove_card_top 52 full_game |> State.get_deck)) );
    ( "Remove more cards than there are in deck" >:: fun _ ->
      assert_raises State.NoCardsLeft (fun () ->
          State.remove_card_top 53 full_game |> State.get_deck) );
    ( "Deal one card from deck" >:: fun _ ->
      assert_equal [ 1 ]
        (State.draw_from_pile pseudo_game0 player0 |> print_hand) );
    ( "Deal all cards from deck" >:: fun _ ->
      assert_equal [ 1 ]
        (State.draw_from_pile pseudo_game1 player0 |> print_hand) );
    ( "Deal cards from empty deck" >:: fun _ ->
      assert_raises State.NoCardsLeft (fun () ->
          State.draw_from_pile pseudo_game2 player3) );
    ( "Check quads in empty hand" >:: fun _ ->
      assert_equal [] (State.check_quad player0) );
    ( "Check quads in nonempty hand with no quads" >:: fun _ ->
      assert_equal []
        (State.add_card (State.add_card player0 1) 1 |> State.check_quad) );
    ( "Check quads in nonempty hand with one set of quads" >:: fun _ ->
      assert_equal [ 4 ] (State.repeat_add_card player0 4 4 |> State.check_quad)
    );
    ( "Check quads in nonempty hand with two set of quads" >:: fun _ ->
      assert_equal [ 4; 5 ]
        (State.repeat_add_card (State.repeat_add_card player0 4 4) 5 4
        |> State.check_quad) );
  ]

(****************************************************************************
  Testing command.ml functions
  ***************************************************************************)
let command_tests =
  [
    ( "Testing expected quit string" >:: fun _ ->
      assert_equal Command.Quit (Command.parse "quit") );
    ( "Testing random capitalization" >:: fun _ ->
      assert_equal Command.Quit (Command.parse "QUiT") );
    ( "Testing extra spaces in quit" >:: fun _ ->
      assert_equal Command.Quit (Command.parse " Quit  ") );
    ( "Testing extra spaces in quit" >:: fun _ ->
      assert_equal Command.Quit (Command.parse " QuIt  ") );
    ( "Testing bad quit input" >:: fun _ ->
      assert_raises Command.Unrecognized (fun () -> Command.parse "qu it") );
    ( "Testing normal request input" >:: fun _ ->
      assert_equal
        (Command.Request ("iram", 5))
        (Command.parse "request iram 5") );
    ( "Testing normal request input capitalization changes" >:: fun _ ->
      assert_equal
        (Command.Request ("joe", 51))
        (Command.parse "REQUEST joe 51") );
    ( "Testing empty" >:: fun _ ->
      assert_raises Command.Empty (fun () -> Command.parse "") );
    ( "Testing request with no other inputs" >:: fun _ ->
      assert_raises Command.Unrecognized (fun () -> Command.parse "request") );
    ( "Testing unrecognized" >:: fun _ ->
      assert_raises Command.Unrecognized (fun () -> Command.parse "BADINPUTS")
    );
    ( "Drawing multiple cards at once" >:: fun _ ->
      assert_equal [ 1; 1; 1 ] (State.repeat_add_card player0 1 3 |> print_hand)
    );
    ( "Drawing no cards at once" >:: fun _ ->
      assert_equal [] (State.repeat_add_card player0 1 0 |> print_hand) );
    ( "Drawing one card at once" >:: fun _ ->
      assert_equal [ 1 ] (State.repeat_add_card player0 1 1 |> print_hand) );
    ( "Drawing 0 doesn't change hand" >:: fun _ ->
      assert_equal [] (State.repeat_add_card player0 1 0 |> print_hand) );
    ( "Count cards where one card is in hand" >:: fun _ ->
      assert_equal 1 (State.add_card player0 1 |> State.count_cards 1) );
    ( "Count cards where multiple card is in hand" >:: fun _ ->
      assert_equal 5 (State.repeat_add_card player0 1 5 |> State.count_cards 1)
    );
    ( "Count cards where no card is in hand" >:: fun _ ->
      assert_equal 0 (State.repeat_add_card player0 1 0 |> State.count_cards 1)
    );
    ( "Count cards where mix of card is in hand" >:: fun _ ->
      assert_equal 5
        (State.repeat_add_card (State.repeat_add_card player0 3 2) 1 5
        |> State.count_cards 1) );
    ( "Check if player has card in empty hand" >:: fun _ ->
      assert_equal false (State.repeat_add_card player0 1 0 |> State.has_card 1)
    );
    ( "Card not in hand" >:: fun _ ->
      assert_equal false (State.repeat_add_card player0 2 1 |> State.has_card 1)
    );
    ( "Card in hand" >:: fun _ ->
      assert_equal true (State.repeat_add_card player0 2 1 |> State.has_card 2)
    );
    ( "Adding and then deleting leads to empty hand" >:: fun _ ->
      assert_equal []
        (State.repeat_add_card player0 2 1 |> State.delete_cards 2 |> print_hand)
    );
    ( "deleting some cards from hand" >:: fun _ ->
      assert_equal [ 1; 1; 1; 1; 1 ]
        (State.repeat_add_card (State.repeat_add_card player0 3 2) 1 5
        |> State.delete_cards 3 |> print_hand) );
    ( "deleting a single card from hand" >:: fun _ ->
      assert_equal [ 3; 3 ]
        (State.repeat_add_card (State.repeat_add_card player0 3 2) 1 1
        |> State.delete_cards 1 |> print_hand) );
    ( "deleting non-existent cards in hand" >:: fun _ ->
      assert_equal [ 2 ]
        (State.repeat_add_card player0 2 1 |> State.delete_cards 5 |> print_hand)
    );
  ]

(****************************************************************************
  Running the full test suite
  ***************************************************************************)
let suite =
  "Test Suite for State Functions"
  >::: List.flatten
         [
           initialization_tests;
           initialize_hand_test;
           game_tests;
           other_functions;
           command_tests;
           exchange_tests;
         ]

let _ = run_test_tt_main suite
