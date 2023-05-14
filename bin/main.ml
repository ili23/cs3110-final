open Game

let rec concat_newlines x acc =
  match x with
  | 0 -> acc
  | x -> concat_newlines (x - 1) "\n" ^ acc

let scrollTerminal = print_string (concat_newlines 23 "")
let remove_empty x = String.length x > 0

let rec int_list_to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> string_of_int h ^ " " ^ int_list_to_string t

let rec to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ to_string t

let initialize_name i =
  print_endline
    ("Please enter the name of player (must be a string) " ^ string_of_int i);
  print_string "> ";
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

let rec print_hand p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      print_endline
        ("Your hand is: " ^ int_list_to_string (State.get_player_hand h));
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
  print_string "> ";
  try Command.parse (read_line ())
  with Command.Unrecognized | Command.Empty ->
    print_endline "Invalid request given. Enter another request.";
    raise Command.Unrecognized

let rec name_check (name : string) player_list =
  match player_list with
  | [] -> false
  | h :: t -> if State.get_player_name h = name then true else name_check name t

let plural count = if count > 1 then "s" else ""

let one_turn state name card num =
  let players = State.get_player_list state in
  let current_player = State.get_current_player state in
  let sender = State.find_player name players in
  if State.has_card card sender then (
    let new_state = State.exchange_cards current_player sender card state in
    let count = State.count_cards card sender in
    print_endline
      (string_of_int count ^ " " ^ string_of_int card ^ plural count
     ^ " received from " ^ name);
    new_state)
  else (
    print_endline "Go Fish";
    let new_draw_player = State.draw_from_pile state current_player in
    let new_state = State.update_player state current_player new_draw_player in
    let newest = State.next_turn num new_state in
    print_endline "drawn from pile";
    newest)

let rec game_cycle (state : State.state) num =
  if State.check_deck state then
    let _ = print_endline "\n \n \n" in
    let _ = print_hand [ State.get_current_player state ] in
    let _ = print_endline "\n \n \n" in
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
              print_endline "Invalid name. Enter another command";
              game_cycle state num)
          else (
            print_endline
              "You can only request cards you have. Enter another command";
            game_cycle state num)
      | Command.Quit ->
          print_endline "Farewell go fish-ers";
          exit 0
    with Command.Unrecognized -> game_cycle state num
  else (
    print_endline "No more cards left in the deck. The game is over";
    exit 0)

let start_game num =
  let clearTerminal : unit = print_endline "\n" in
  let state = ready_state num in
  print_string "Ready to begin?";
  print_players (State.get_player_list state);
  print_endline "\n";
  print_endline "Here are some commands for you to use before you begin:";
  print_endline
    "Type card requests in the format 'Request <player name> <card>'";
  print_endline "Type 'quit' to quit the game";
  print_endline "Fire, let's get started!";
  clearTerminal;
  game_cycle state num

(** printHand (State.get_player_list (deal_cards (initial_state num) num));
    print_endline "Request cards from a player by typing 'Request <player name>
    <card>'"; print_endline "Fire, let's get started!"*)

let rec play_game number_player =
  match number_player with
  | i when i < 3 ->
      print_endline
        "You can't start with less than 3 players, get some more friends. \n";
      print_endline "Please enter the number of players for the game.\n";
      print_string "> ";
      let input = read_line () in
      play_game (int_of_string input)
  | i when i > 2 -> start_game i
  | _ -> exit 1

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  scrollTerminal;
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "\n\nWelcome to Big Bactrian's Camel's Implementation of Go Fish.\n";
  ANSITerminal.print_string [ ANSITerminal.blue ]
    "Here are some rules/tips to ensure the best experience: \n\n\
    \    Since the game is meant to be played on one device with each player \
     passing the device around, there will be some honor code built in. Don't \
     scroll through the terminal during your turn to look at other people's \
     hands. \n\
    \ ";
  print_endline
    "Please enter the number of players for the game. (Enter an integer less \
     than 10)\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number -> play_game (int_of_string number)

(*Need to add a check here to make sure that the input is an int*)
(* Execute the game engine. *)
let () = main ()
