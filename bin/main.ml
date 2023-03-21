open Game

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
  | [] -> { player with State.name = "Unknown Player" ^ string_of_int i }
  | h :: t -> { player with State.name = h ^ to_string t }

let play_game number_player =
  match number_player with
  | i when i < 5 ->
      play_helper i;
      exit 0
  | i when i < 8 ->
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

(* Execute the game engine. *)
let () = main ()
