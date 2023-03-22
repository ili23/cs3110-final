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
  print_endline ("Please enter the name of player " ^ string_of_int i);
  print_string "> ";
  let words = String.split_on_char ' ' (read_line ()) in
  let full_words = List.filter remove_empty words in
  match full_words with
  | [] -> State.init_player ("Unknown Player" ^ string_of_int i)
  | h :: t -> State.init_player (h ^ to_string t)

let rec add_player_x_times game counter = function
  | 0 -> game
  | x ->
      add_player_x_times
        (State.add_player game (initialize_name counter))
        (counter + 1) (x - 1)

let initial_state num = add_player_x_times State.init_state 0 num

let deal_cards state num =
  State.updatePlayers state
    (State.initialize_players_hands (State.getPlayerList state) State.fake_list)

let rec printHand p_list =
  match p_list with
  | [] -> ()
  | h :: t ->
      print_endline
        (State.getPlayerName h ^ "'s hand is: "
        ^ int_list_to_string (State.getPlayerHand h));
      printHand t

let start_game num =
  printHand (State.getPlayerList (deal_cards (initial_state num) num));
  print_endline "Fire, let's get started!"

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
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\n Welcome to Go Fish.\n";
  print_endline "Please enter the number of players for the game.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number ->
      play_game (int_of_string number);
      exit 0

(*Need to add a check here to make sure that the input is an int*)
(* Execute the game engine. *)
let () = main ()
