open Game

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
        (State.add_player game (initialize_name counter))
        (counter + 1) (x - 1)

let initial_state num = add_player_x_times State.init_state 0 num

let deal_cards state num =
  State.update_players state
    (State.initialize_players_hands (State.get_player_list state) State.shuffle)

let rec printHand p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      print_endline
        (State.get_player_name h ^ "'s hand is: "
        ^ int_list_to_string (State.get_player_hand h));
      printHand t

let parse_command state =
  print_endline "Please request a card from a player";
  print_string "> ";
  try Command.parse (read_line ())
  with Command.Unrecognized | Command.Empty ->
    print_endline "Invalid request given. Enter another request.";
    raise Command.Empty

let rec name_check (name : string) player_list =
  match player_list with
  | [] -> false
  | h :: t -> if State.get_player_name h = name then true else name_check name t

let rec game_cycle (state : State.state) =
  match parse_command state with
  | Command.Request (name, number) ->
      if name_check name (State.get_player_list state) then
        State.get_player_hand
      else (
        print_endline "Invalid name. Enter another command";
        game_cycle state)

let start_game num =
  printHand (State.get_player_list (deal_cards (initial_state num) num));
  print_endline
    "Request cards from a player by typing 'Request <player name> <card>'";
  print_endline "Fire, let's get started!";
  game_cycle (deal_cards (initial_state num) num)

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


let clearTerminal = 
  print_endline "";
  ()
(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\n Welcome to Go Fish.\n";
  print_endline
    "Please enter the number of players for the game. (Enter an integer less \
     than 10)\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number ->
      play_game (int_of_string number);
      exit 0

(*Need to add a check here to make sure that the input is an int*)
(* Execute the game engine. *)
let () = main ()
