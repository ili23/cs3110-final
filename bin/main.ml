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
  print_endline
    ("Please enter the name of player "
    ^ string_of_int (i + 1)
    ^ " (must be a string)");
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

let ready_state num = State.assign_id (deal_cards (initial_state num) num) num

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
      print_endline
        ("Your hand is: " ^ cards_to_string (State.get_player_hand h));
      print_hand t

let rec print_players p_list =
  match p_list with
  | [] -> ()
  | [ h ] -> print_string ("and " ^ State.get_player_name h)
  | h :: t ->
      print_string (State.get_player_name h ^ ", ");
      print_players t

let parse_command state =
  print_endline "Please request a card from a player";
  print_string ">> ";
  try Command.parse (read_line ())
  with Command.Unrecognized | Command.Empty ->
    print_endline
      "Invalid request given. Enter another request. Remember the format is \
       'Request <player name> <card>";
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
        print_string scrollTerminal;
        print_endline "Log: ";
        print_endline (h ^ "\n");
        print_log t (num - 1))
      else (
        print_endline "Log: ";
        print_endline h)

let rec shift_ready state num input =
  match input with
  | i when String.lowercase_ascii i = "ready" ->
      let _ = print_log (List.rev (State.get_log state)) 10 in
      state
  | i when String.lowercase_ascii i = "quit" ->
      print_endline "Farewell Go Fish-ers ";
      exit 0
  | i ->
      print_endline
        "Not recognized, if you are ready to see your cards, enter 'ready' \
         without spaces or extra characters or if you want to quit, enter \
         'quit' \n";
      print_string ">> ";
      let new_input = read_line () in
      shift_ready state num new_input

let one_turn state name card num =
  let players = State.get_player_list state in
  let current_player = State.get_current_player state in
  let sender = State.find_player name players in
  if State.has_card card sender then (
    let count = State.count_cards card sender in
    let new_state = State.exchange_cards current_player sender card state in
    print_endline
      (string_of_int count ^ " " ^ string_of_int card ^ plural count
     ^ " received from " ^ name);
    let logged_state =
      State.add_log new_state
        (State.get_player_name current_player
        ^ " received " ^ string_of_int count ^ " " ^ string_of_int card
        ^ plural count ^ " from " ^ name)
    in
    let curr_player = State.get_current_player logged_state in
    if List.length (State.check_quad curr_player) > 0 then (
      let newest_state =
        State.update_player logged_state curr_player
          (State.add_quad curr_player)
      in
      print_endline ("Congrats you collected all 4 " ^ string_of_int card ^ "s");
      print_endline
        ("Your new score is: "
        ^ string_of_int
            (State.get_score (State.get_current_player newest_state)));
      let log_state =
        State.add_log newest_state
          (State.get_player_name current_player
          ^ " collected all 4 " ^ string_of_int card ^ "s")
      in
      log_state)
    else logged_state)
  else (
    print_endline "You guessed incorrectly! Go Fish!";
    let logged_state =
      State.add_log state
        (State.get_player_name current_player
        ^ " unsuccessfully requested for " ^ vowel card ^ " "
        ^ string_of_int card ^ " from " ^ name)
    in
    let new_draw_player = State.draw_from_pile logged_state current_player in
    print_endline
      ("You drew "
      ^ vowel (State.check_top_card state)
      ^ " "
      ^ string_of_int (State.check_top_card state)
      ^ " from the deck!");
    let new_deck_state = State.remove_card_top 1 logged_state in
    let new_state =
      State.update_player new_deck_state current_player new_draw_player
    in
    let newest = State.next_turn num new_state in
    print_string scrollTerminal;
    print_endline
      "Are you ready to see your cards? Make sure that the other players can't \
       see your cards. If you are ready, type 'ready'. As per usual, type \
       'quit' to leave the game. ";
    let input = read_line () in
    shift_ready newest num input)

let rec print_winner lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ ", " ^ print_winner t

let rec game_cycle (state : State.state) num =
  if State.check_deck state then
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
              print_endline
                "Invalid name. Make sure to check your spelling and the \
                 request format.";
              game_cycle state num)
          else (
            print_endline
              "You can only request cards you have. Make sure to double check \
               your input and the request format.";
            game_cycle state num)
      | Command.Quit ->
          print_endline
            "Thank you for playing the game, hope you had fun, Farewell Go \
             Fish-ers!";
          exit 0
    with Command.Unrecognized -> game_cycle state num
  else (
    print_endline "No more cards left in the deck. The game is over";
    print_endline (State.check_winner state |> print_winner);
    exit 0)

let rec move_next state num input =
  match input with
  | i when String.lowercase_ascii i = "ready" -> game_cycle state num
  | i when String.lowercase_ascii i = "quit" ->
      print_endline "Farewell Go Fish-ers ";
      exit 0
  | i ->
      print_endline
        "Not recognized, if you are ready to see your cards, enter 'ready' \
         without spaces or extra characters or if you want to quit, enter \
         'quit' \n";
      print_string ">> ";
      let new_input = read_line () in
      move_next state num new_input

let start_game num =
  let state = ready_state num in
  print_string scrollTerminal;
  print_string "Here we go! The players will go in this order: ";
  print_players (State.get_player_list state);
  print_endline ".\n";
  print_endline
    "As a reminder, here are some commands for you to use during the game:";
  print_endline
    "Type card requests in the format 'Request <player name> <card>'";
  print_endline "Type 'quit' to quit the game";
  print_endline "Fire, let's get started!";
  print_endline
    "Are you ready to see your cards? Make sure that the other players can't \
     see your cards. If you are ready, type 'ready'. As per usual, type 'quit' \
     to leave the game. ";
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
      print_endline
        "Not recognized, if you want to play the game, enter 'ready' without \
         spaces or extra characters or if you want to quit, enter 'quit' \n";
      print_string ">> ";
      let new_input = read_line () in
      play_game new_input

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  print_string scrollTerminal;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Big Bactrian's Camel's Implementation of Go Fish.\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Here are some rules/tips to ensure the best experience: \n\n\
    \ Since the game is meant to be played on one device with each player \
     passing the device around, there will be some honor code built in. Don't \
     scroll through the terminal during your turn to look at other people's \
     hands. This also means that you shouldn't change the terminal size. (Use \
     the default MacOS terminal for the best experience.) \n\n\
    \ ";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Now on how to play the game: Placeholder \n ";
  (* Need rules description here, we made it a 4 person game so def say
     something bout that here*)
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Further instructions will be given later to help you. If all four players \
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
