type player = {
  name : string;
  ready : bool;
  id : int;
  hand : int list;
  score : int;
  won_cards : int list;
}

type state = {
  deck : int list;
  players : player list;
  current_player : int;
      (*Could potentially swap this to player but this might be a little easier
        to change*)
}

let play_helper players =
  match players with
  | i -> print_endline "Make turn"

let remove_empty x = String.length x > 0

let rec to_string lst =
  match lst with
  | [] -> ""
  | h :: t -> h ^ to_string t

let initialize_name i player =
  print_endline ("Please enter the name of player" ^ string_of_int i);
  print_string "> ";
  let words = String.split_on_char ' ' (read_line ()) in
  let full_words = List.filter remove_empty words in
  match full_words with
  | [] -> { player with name = "Unknown Player" ^ string_of_int i }
  | h :: t -> { player with name = h ^ to_string t }

let rec play_game number_player =
  match number_player with
  | i when i < 3 ->
      print_endline
        "You can't start with less than 3 players, get some more friends. \n";
      print_endline "Please enter the number of players for the game.\n";
      print_string "> ";
      let input = read_line () in
      play_game (int_of_string input)
  | i when i > 2 ->
      play_helper i;
      exit 0
  | _ -> exit 1

(** [main ()] prompts for the game to play, then starts it. *)
let main () =
  ANSITerminal.print_string [ ANSITerminal.blue ] "\n\n Welcome to Go Fish.\n";
  print_endline "Please enter the number of players for the game.\n";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> ()
  | number -> play_game (int_of_string number)

(*Need to add a check here to make sure that the input is an int*)
(* Execute the game engine. *)
let () = main ()
