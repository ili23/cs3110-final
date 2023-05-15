open Game

let rec concat_newlines x acc =
  match x with
  | 0 -> acc
  | x -> concat_newlines (x - 1) "\n" ^ acc

let scrollTerminal = concat_newlines 23 ""
let remove_empty x = String.length x > 0

let rec int_list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> string_of_int h ^ " " ^ int_list_to_string t

let rec to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ "_" ^ to_string t

let initialize_name i =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    ("Please enter the name of player "
    ^ string_of_int (i + 1)
    ^ " (You cannot leave this blank. Try to keep the name to under 8 \
       characters so that it will be easier to request cards.)\n");
  print_string ">> ";
  let words = String.split_on_char ' ' (read_line ()) in
  let full_words = List.filter remove_empty words in
  match full_words with
  | [] -> State.init_player ("Unknown_Player_" ^ string_of_int i)
  | [ h ] -> State.init_player h
  | h :: t -> State.init_player (h ^ "_" ^ to_string t)

let rec add_player_x_times game counter = function
  | 0 -> game
  | x ->
      add_player_x_times
        (State.add_player (initialize_name counter) game)
        (counter + 1) (x - 1)

let initial_state num = add_player_x_times State.init_state 0 num

let deal_cards state num =
  State.update_players state
    (State.initialize_players_hands State.shuffle (State.get_player_list state))

let remove_card state num = State.remove_card_top (num * 5) state

let ready_state num =
  State.assign_id (remove_card (deal_cards (initial_state num) num) num) num

let rec cards_to_string hand =
  match hand with
  | [] -> ""
  | [ h ] -> (
      match h with
      | 1 -> "A"
      | 11 -> "J"
      | 12 -> "Q"
      | 13 -> "K"
      | _ -> string_of_int h)
  | h :: t -> (
      match h with
      | 1 -> "A " ^ cards_to_string t
      | 11 -> "J " ^ cards_to_string t
      | 12 -> "Q " ^ cards_to_string t
      | 13 -> "K " ^ cards_to_string t
      | _ -> string_of_int h ^ " " ^ cards_to_string t)

let rec print_hand p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Your hand is: " ^ cards_to_string (State.get_player_hand h) ^ "\n");
      print_hand t

let rec print_players p_list =
  match p_list with
  | [] -> ()
  | [ h ] ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("and " ^ State.get_player_name h)
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (State.get_player_name h ^ ", ");
      print_players t

let parse_command state =
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Please request a card from a player. \n";
  print_string ">> ";
  try Command.parse (read_line ())
  with Command.Unrecognized | Command.Empty ->
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Invalid request given. Enter another request. Remember the format is \
       'Request <player name> <card>' \n";
    raise Command.Unrecognized

let rec name_check (name : string) player_list =
  match player_list with
  | [] -> false
  | h :: t -> if State.get_player_name h = name then true else name_check name t

let plural count = if count > 1 then "s" else ""

let vowel card =
  match card with
  | 7 -> "an"
  | 11 -> "an"
  | _ -> "a"

let rec print_log list num =
  match list with
  | [] -> print_endline ""
  | h :: t ->
      if num > 0 then (
        ANSITerminal.print_string [ ANSITerminal.blue ] (h ^ "\n\n");
        print_log t (num - 1))
      else print_endline h

let full_print_log list num =
  print_string scrollTerminal;
  ANSITerminal.print_string [ ANSITerminal.blue ] "Log: \n";
  print_log list num

let rec print_names p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.blue ] (State.get_player_name h);
      print_string "     ";
      print_names t

let rec print_spaces num =
  match num with
  | 0 -> print_string ""
  | _ ->
      print_string " ";
      print_spaces (num - 1)

let rec print_scores p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        (string_of_int (State.get_score h));
      print_spaces (String.length (State.get_player_name h) - 1);
      print_string "     ";
      print_scores t

let rec shift_ready state num input =
  match input with
  | i when String.lowercase_ascii i = "ready" ->
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ] "\n Game Log: \n"
      in
      let _ = print_log (List.rev (State.get_log state)) 10 in
      let _ =
        ANSITerminal.print_string [ ANSITerminal.blue ] "Scoreboard: \n"
      in
      let _ = print_names (State.get_player_list state) in
      let _ = print_string "\n" in
      let _ = print_scores (State.get_player_list state) in
      let _ = print_string "\n" in
      state
  | i when String.lowercase_ascii i = "quit" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Thanks for playing! Farewell Go Fish-ers! \n";
      exit 0
  | i ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Not recognized, if you are ready to see your cards, enter 'ready' \
         without spaces or extra characters or if you want to quit, enter \
         'quit' \n\n";
      print_string ">> ";
      let new_input = read_line () in
      shift_ready state num new_input

let rec done_viewing state num input =
  match input with
  | x when String.lowercase_ascii x = "done" ->
      print_string scrollTerminal;
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Are you ready to see your cards? Make sure that the other players \
         can't see your cards. If you are ready, type 'ready'. As per usual, \
         type 'quit' to leave the game. \n";
      let input = read_line () in
      shift_ready state num input
  | x when x = "quit" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Thanks for playing! Farewell Go Fish-ers! \n";
      exit 0
  | x ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Not recognized, if you are done viewing your cards, enter 'done' \
         without spaces or extra characters or if you want to quit, enter \
         'quit' \n\n";
      print_string ">> ";
      let new_input = read_line () in
      done_viewing state num new_input

let one_turn state name card num =
  let players = State.get_player_list state in
  let current_player = State.get_current_player state in
  let sender = State.find_player name players in
  if State.has_card card sender then (
    let count = State.count_cards card sender in
    let new_state = State.exchange_cards current_player sender card state in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      (string_of_int count ^ " " ^ cards_to_string [ card ] ^ plural count
     ^ " received from " ^ name ^ "\n");
    let logged_state =
      State.add_log new_state
        (State.get_player_name current_player
        ^ " received " ^ string_of_int count ^ " " ^ cards_to_string [ card ]
        ^ plural count ^ " from " ^ name)
    in
    let curr_player = State.get_current_player logged_state in
    if List.length (State.check_quad curr_player) > 0 then (
      let newest_state =
        State.update_player logged_state curr_player
          (State.add_quad curr_player)
      in
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Congrats you collected all 4 " ^ cards_to_string [ card ] ^ "s\n");
      ANSITerminal.print_string [ ANSITerminal.blue ]
        ("Your new score is: "
        ^ string_of_int
            (State.get_score (State.get_current_player newest_state))
        ^ "\n");
      let log_state =
        State.add_log newest_state
          (State.get_player_name current_player
          ^ " collected all 4 " ^ cards_to_string [ card ] ^ "s")
      in
      log_state)
    else logged_state)
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "You guessed incorrectly! Go Fish!\n";
    let logged_state =
      State.add_log state
        (State.get_player_name current_player
        ^ " unsuccessfully requested for " ^ vowel card ^ " "
        ^ cards_to_string [ card ] ^ " from " ^ name)
    in
    let new_draw_player = State.draw_from_pile logged_state current_player in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ("You drew "
      ^ vowel (State.check_top_card state)
      ^ " "
      ^ cards_to_string [ State.check_top_card state ]
      ^ " from the deck! \n");
    let new_deck_state = State.remove_card_top 1 logged_state in
    let new_state =
      State.update_player new_deck_state current_player new_draw_player
    in
    let newest = State.next_turn num new_state in
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Are you done with your turn? If you are ready to move to the next \
       player, type 'done'. As per usual, type 'quit' to leave the game. \n";
    print_string ">> ";
    let input = read_line () in

    done_viewing newest num input)

let rec print_winner lst =
  let rec concat_winner list =
    match list with
    | [] -> ""
    | h :: t -> h ^ ", " ^ concat_winner t
  in
  match List.length lst with
  | 4 -> "Congrats on finishing the game! Everyone tied!"
  | 3 | 2 ->
      "Congrats on finishing the game! The winners are " ^ concat_winner lst
      ^ "."
  | 1 ->
      "Congrats on finishing the game! The winner is " ^ concat_winner lst ^ "."
  | _ -> "No winners!"

let rec card_checker p_list =
  match p_list with
  | [] -> true
  | h :: t ->
      if List.length (State.get_hand h) = 0 then false else card_checker t

let rec game_cycle (state : State.state) num =
  let has_cards = card_checker (State.get_player_list state) in
  if State.check_deck state && has_cards then
    let _ = print_hand [ State.get_current_player state ] in
    let _ = print_endline "\n" in
    try
      match parse_command state with
      | Command.Request (name, card) ->
          let players = State.get_player_list state in
          let current_player = State.get_current_player state in
          if State.check_person current_player card then
            if
              name_check name players
              && name <> State.get_player_name current_player
            then
              let new_state = one_turn state name card num in
              game_cycle new_state num
            else (
              ANSITerminal.print_string [ ANSITerminal.blue ]
                "Invalid name. Make sure to check your spelling and the \
                 request format. \n";
              game_cycle state num)
          else (
            ANSITerminal.print_string [ ANSITerminal.blue ]
              "You can only request cards you have. Make sure to double check \
               your input and the request format.\n";
            game_cycle state num)
      | Command.Quit ->
          ANSITerminal.print_string [ ANSITerminal.blue ]
            "Thank you for playing the game, hope you had fun, Farewell Go \
             Fish-ers!\n";
          exit 0
    with Command.Unrecognized -> game_cycle state num
  else if has_cards then (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "There are no more cards left in the deck which means the game is over. \n";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ((State.check_winner state |> print_winner) ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "The final scoreboard is: \n";
    print_names (State.get_player_list state);
    print_string "\n";
    print_scores (State.get_player_list state);
    print_string "\n";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "Thanks for playing! Hope you enjoyed our game. For bugs/feedback please \
       email il233@cornell.edu, jjx5@cornell.edu, or jl2748@cornell.edu. \n";
    exit 0)
  else (
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "A player is out of cards which means that the game is over.\n";
    ANSITerminal.print_string [ ANSITerminal.blue ]
      ((State.check_winner state |> print_winner) ^ "\n");
    ANSITerminal.print_string [ ANSITerminal.blue ]
      "The final scoreboard is: \n";
    print_names (State.get_player_list state);
    print_string "\n";
    print_scores (State.get_player_list state);
    exit 0)

let rec move_next state num input =
  match input with
  | i when String.lowercase_ascii i = "ready" -> game_cycle state num
  | i when String.lowercase_ascii i = "quit" ->
      ANSITerminal.print_string [ ANSITerminal.blue ] "Farewell Go Fish-ers \n";
      exit 0
  | i ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Not recognized, if you are ready to see your cards, enter 'ready' \
         without spaces or extra characters or if you want to quit, enter \
         'quit' \n\n";
      print_string ">> ";
      let new_input = read_line () in
      move_next state num new_input

let start_game num =
  let state = ready_state num in
  print_string scrollTerminal;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Here we go! The players will go in this order: ";
  print_players (State.get_player_list state);
  print_endline ".\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "As a reminder, here are some commands for you to use during the game: \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Type card requests in the format 'Request <player name> <card>' \n ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Type 'quit' to quit the game \n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Fire, let's get started!\n\n\n\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Are you ready to see your cards? Make sure that the other players can't \
     see your cards. If you are ready, type 'ready'. As per usual, type 'quit' \
     to leave the game. \n";
  print_string ">> ";
  let input = read_line () in
  move_next state num input

(** printHand (State.get_player_list (deal_cards (initial_state num) num));
    print_endline "Request cards from a player by typing 'Request <player name>
    <card>'"; print_endline "Fire, let's get started!"*)

let rec play_game input =
  match input with
  | i when String.lowercase_ascii i = "ready" -> start_game 4
  | i when String.lowercase_ascii i = "quit" ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Farewell Go Fish-ers. See you soon!";
      exit 0
  | i ->
      ANSITerminal.print_string [ ANSITerminal.blue ]
        "Not recognized, if you want to play the game, enter 'ready' without \
         spaces or extra characters or if you want to quit, enter 'quit' \n\n";
      print_string ">> ";
      let new_input = read_line () in
      play_game new_input

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string scrollTerminal;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Big Bactrian's Camel's Implementation of Go Fish.\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Here are some rules/tips to ensure the best experience: \n\n\
     Rules: \n\n\
     Honor code: Since the game is meant to be played on one device with each \
     player passing the device around, there will be some honor code built in. \
     Don't scroll through the terminal during your turn to look at other \
     people's hands. You can of course scroll if you need to look at the log, \
     just don't purposely search for other people's hands. This also means \
     that you shouldn't change the terminal size. (Use the default MacOS \
     terminal for the best experience.) \n\
    \ ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Now on how to play the game: This is meant to be a four player game. Each \
     player is initially dealt 5 cards. Then, each person gets a turn in the \
     order they enter their name. Before each turn, the player whose turn it \
     is will be asked to type in ready to make sure they are ready and are the \
     only ones seeing their card. \n\n\
     During a turn, a player can request a card from another player by typing \
     in 'Request <player name> <card>' or quit the game by typing in 'Quit'. \
     Players can only request for cards they already have in their hand. If \
     the player successfully requests a card from another player, the cards \
     are given to the current player, and the player can request again. \
     Otherwise, the current player will 'Go Fish' and draws a card from the \
     deck. \n\n\
     If a player gets 4 of a kind (i.e. 4 of the same value), then they get 1 \
     point added to their score. Those cards are removed from their hand. \n\
     The game ends when a player runs out of cards or when there are no cards \
     left in the deck to draw from. The winner(s) are the player(s) with the \
     most points when the game ends. \n\n\
     Tips: \n\n\
     Check the log and scoreboard for information on what other players have \
     been doing. \n\
    \ ";
  (* Need rules description here, we made it a 4 person game so def say
     something bout that here*)
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\
     Further instructions will be given later to help you. If all four players \
     have fully read and accept the rules, type 'ready' to begin! \
     Alternatively, type 'quit' if you don't want to play the game \n\n\
    \ ";
  print_string ">> ";
  match read_line () with
  | x -> play_game x
  | exception End_of_file -> ()

(*Need to add a check here to make sure that the input is an int*)
(* Execute the game engine. *)
let () = main ()
